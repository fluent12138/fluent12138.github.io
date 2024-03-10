![fsm-游戏交易.excalidraw.png](https://cdn.acwing.com/media/article/image/2023/04/04/36510_86007705d2-fsm-游戏交易.excalidraw.png)

 **code** 
 
```
%%%-------------------------------------------------------------------
%%% @author 勒勒
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 3月 2023 14:38
%%%-------------------------------------------------------------------
-module(trade_fsm).
-author("勒勒").
-behavior(gen_fsm).
-record(state, {name = "", other, ownitems = [], otheritems = [], monitor, from}). % from就是gen_fsm中发送消息处
%% 公共API
-export([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).
%% gen_sfm回调函数
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
        % 定制的状态名
        idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3, wait/2, ready/2, ready/3
        ]).

%%% 公共API
start(Name) -> gen_fsm:start(?MODULE, [Name], []).

start_link(Name) -> gen_fsm:start_link(?MODULE, [Name], []).

%% 请求开始交易谈话. 当/如果对方接收时返回
trade(OwnPid, OtherPid) ->
  gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

%% 接收某个玩家的交易请求
accept_trade(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%% 从物品列表中选择一个进行交易
make_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% 撤销某个交易物品
retract_offer(OwnPid, Item) ->
  gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% 宣布自己就绪. 当对方也宣布自己就绪了, 交易就完成了.
ready(OwnPid) ->
  gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% 取消交易
cancel(OwnPid) ->
  gen_fsm:sync_send_all_state_event(OwnPid, cancel).

%%% FSM 到 FSM

%% 向另一个FSM发送交易会话请求
ask_negotiate(OwnPid, OtherPid) ->
  gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).

%% 转发玩家交易接收请求
accept_negotiate(OtherPid, OwnPid) ->
  gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% 转发玩家的交易物品提供信息
do_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {do_offer, Item}).

%% 转发玩家交易物品撤销请求
undo_offer(OtherPid, Item) ->
  gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% 询问对方是否就绪
are_you_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, are_you_ready).

%% 回复未就绪 -> 不在wait状态哦
not_yet(OtherPid) ->
  gen_fsm:send_event(OtherPid, not_yet).

%% 通知对方玩家当前处于等待进入ready状态
%% 状态会迁移到ready ?
am_ready(OtherPid) ->
  gen_fsm:send_event(OtherPid, 'ready!').

%% 确认fsm处于ready状态
ack_trans(OtherPid) ->
  gen_fsm:send_event(OtherPid, ack).

%% 询问是否可以提交
ask_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, ask_commit).

%% 开始同步提交
do_commit(OtherPid) ->
  gen_fsm:sync_send_event(OtherPid, do_commit).

%% 礼貌取消
notify_cancel(OtherPid) ->
  gen_fsm:send_all_state_event(OtherPid, cancel).

init(Name) ->
  {ok, idle, #state{name = Name}}.

%% 给玩家发送一条通知, 可以是一条发给玩家进程的消息
%% 打印在控制台就ok了
notice(#state{name = N}, Str, Args) ->
  io:format("~s : "++ Str ++ "~n", [N | Args]).

%% 记录非期望的消息
unexpected(Msg, State) ->
  io:format("~p receive unknown event ~p while in state ~p", [self(), Msg, State]).

%%% idle 请求/接收 交易
%% 异步版本, 关心其他玩家的交易请求
idle({ask_negotiate, OtherPid}, S = #state{}) ->
  Ref = monitor(process, OtherPid),
  notice(S, "~p asked for a trade negotiation", [OtherPid]),
  {next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};

idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

%% 同步版本, 本方玩家请求另一个玩家进行交易
idle({negotiate, OtherPid}, From, S=#state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S = #state{other = OtherPid, monitor = Ref, from = From}};

idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

%%% idle_wait
%% 接收方, 1.同意协商 2.在我们发送交易请求时, 他也发起了交易
idle_wait({ask_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
  gen_fsm:reply(S#state.from, ok), % 通知玩家一切正常
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};

%% 对方接受了我们的请求, 迁移negotiate状态
idle_wait({accept_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
  gen_fsm:reply(S#state.from, ok), % 通知玩家一切正常
  notice(S, "starting negotiation", []),
  {next_state, negotiate, S};

idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

%% 发送方, 接收对方玩家请求
idle_wait(accept_negotiate, _From, S = #state{other = OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "accepting negotiation", []),
  {reply, ok, negotiate, S};

idle_wait(Event, _From, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

%% 向物品列表中添加一个物品
add(Item, Items) ->
  [Item|Items].

%% 从物品列表中删除一个物品
remove(Item, Items) ->
  Items -- [Item].

%%% negotiate
%% 本方提供物品
negotiate({make_offer, Item}, S = #state{ownitems = OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "offering ~p", [Item]),
  {next_state, negotiate, S#state{ownitems = add(Item, OwnItems)}};

%% 本方撤销物品
negotiate({retract_offer, Item}, S = #state{ownitems = OwnItems}) ->
  undo_offer(S#state.other, Item),
  notice(S, "cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{ownitems = remove(Item, OwnItems)}};

%% 对方提供物品
negotiate({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
  notice(S, "other player offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};

%% 对方撤销物品
negotiate({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
  notice(S, "other player cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = OtherItems}};

%% 当在negotiate状态时, 接收到对方的ready信号, 直接拒绝.
negotiate(are_you_ready, S = #state{other = OtherPid}) ->
  io:format("Other user ready to trade ~n"),
  notice(S, "Other user ready to transfer goods : ~n You get ~p, The Other side gets ~p", [S#state.otheritems, S#state.ownitems]),
  not_yet(OtherPid),
  {next_state, negotiate, S};

negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

%% 玩家做完决策 -> 发送ready
negotiate(ready, From, S = #state{other = OtherPid}) ->
  are_you_ready(OtherPid),
  notice(S, "asking if ready, waiting", []),
  {next_state, wait, S = #state{from = From}};

negotiate(Event, _From, S) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, S}.

%%% wait

%% 当对方撤销/提供物品了, 回退到negotiate状态
wait({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed), % 通知negotiate状态
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};

wait({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed), % 通知negotiate状态
  notice(S, "other side cancelling offer of ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};

%% 对方询问本方是否ready
wait(are_you_ready, S = #state{}) ->
  am_ready(S#state.other),
  notice(S, "asked if ready, and I am. Waiting for same reply", []),
  {next_state, wait, S};

%% 对方不同意
wait(not_yet, S = #state{}) ->
  notice(S, "Other not ready yet", []),
  {next_state, wait, S};

%% 收到ready!
wait('ready!', S = #state{}) ->
  am_ready(S#state.other),
  ack_trans(S#state.other),
  gen_fsm:reply(#state.from, ok),
  notice(S, "other side is ready. Moving to ready state", []),
  {next_state, ready, S};

%% 不期待的消息
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.

%%% ready, 选择一个FSM去完成ready, erlang中进程是排好序的, 可以比较大小
priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OtherPid > OwnPid -> false.

ready(ack, S = #state{}) ->
  case priority(self(), S#state.other) of
      true ->
        try
          notice(S, "asking for commit", []),
          ready_commit = ask_commit(S#state.other),
          notice(S, "ordering commit", []),
          ok = do_commit(S#state.other),
          commit(S),
          {stop, normal, S}
        catch Class:Reason ->
          %% 退出!, ready_commit或do_commit失败了
          notice(S, "commit failed", []),
          {stop, {Class, Reason}, S}
        end;
      false ->
        {next_state, ready, S}
  end;

ready(Event, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

ready(ask_commit, _From, S) ->
  notice(S, "replying to ask_commit", []),
  {reply, ready_commit, ready, S};

ready(do_commit, _From, S) ->
  notice(S, "commiting...", []),
  commit(S),
  {stop, normal, ok, S};

ready(Event, _From, S) ->
  unexpected(Event, ready),
  {next_state, ready, S}.

commit(S = #state{}) ->
  io:format("Transaction completed for ~s.", [S#state.name]),
  io:format("Items send are: ~n ~p ~n received are: ~n ~p ~n", [S#state.ownitems, S#state.otheritems]),
  io:format("The operation should have some atomic save in a database").

%% 对方玩家取消交易
%% 停止正再做的工作 终止进程
handle_event(cancel, _StateName, S = #state{}) ->
  notice(S, "received cancel event", []),
  {stop, other_cancelled, S};

handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

%% 本方玩家终止交易, 必须通知对方玩家我们退出了
handle_sync_event(cancel, _From, _StateName, S = #state{}) ->
  notify_cancel(S#state.other),
  notice(S, "cancelling trade, sending cancel event", []),
  {stop, cancelled, ok, S};

handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

%% 处理对方FSM崩溃事件
handle_info({'DOWN', Ref, process, Pid, Reason}, _, S = #state{other = Pid, monitor=Ref}) ->
  notice(S, "Other side dead", []),
  {stop, {other_down, Reason}, S};

handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.

code_change(_OldVsn, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

terminate(_Reason, _StateName, _StateData) -> ok.
```
<!-- ##{"timestamp":1680616800}## -->