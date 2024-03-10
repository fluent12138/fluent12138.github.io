# OTP应用 - 进程池
## 架构图 - 监督树

![ppool架构图.excalidraw.png](https://cdn.acwing.com/media/article/image/2023/04/08/36510_68e5fc62d6-ppool架构图.excalidraw.png) 

## 监督者实现

### ppool_supersup流程图解
![ppool_supersup逻辑.excalidraw.png](https://cdn.acwing.com/media/article/image/2023/04/09/36510_f521f702d6-ppool_supersup逻辑.excalidraw.png) 

### code

```erlang
-module(ppool_supersup).  
-author("勒勒").  
-behavior(supervisor).  
%% API  
-export([start_link/0, stop/0, start_pool/3, stop_pool/1]).  
-export([init/1]).  
   
start_link() ->  
  io:format("start supersup ...~n"),  
  supervisor:start_link({local, ppool}, ?MODULE, []). %{local, Name} 命名  
  
stop() ->  
  case whereis(ppool) of  
    P when is_pid(P) ->  
      exit(P, kill);  
    _ -> ok  
  end.  
  
init([]) ->  
  MaxRestart = 6,  
  MaxTime = 3000,  
  {ok, {{one_for_one, MaxRestart, MaxTime}, []}}.  
  
%% Limit : 工作者进程个数; MFA元组 : 工作者进程监督者启动工作者进程需要的{M, F, A}元组  
start_pool(Name, Limit, MFA) ->  
  io:format("start pool ... args: {~p, ~p, ~p}~n", [Name, Limit, MFA]),  
  ChildSpec = {Name,  
               {ppool_sup, start_link, [Name, Limit, MFA]}, % {M, F, A}  
               permanent, 10500, supervisor, [ppool_sup]  
              },  
  supervisor:start_child(ppool, ChildSpec).  
  
stop_pool(Name) ->  
  supervisor:terminate_child(ppool, Name),  
  supervisor:delete_child(ppool, Name).
```
### ppool_sup流程
`start_link -> (supervisor) -> init(初始化信息, 需要开启serv)`
### code
```erlang
-module(ppool_sup).  
-author("勒勒").  
-behavior(supervisor).  
%% API  
-export([start_link/3, init/1]).  
  
start_link(Name, Limit, MFA) ->  
  io:format("start ppool_sup...~n"),  
  supervisor:start_link(?MODULE, {Name, Limit, MFA}).  
  
init({Name, Limit, MFA}) ->  
  io:format("init ppool_sup...~n"),  
  MaxRestart = 1,  
  MaxTime = 3600,  
  {ok, {{one_for_all, MaxRestart, MaxTime},  
        [{serv,  
          {ppool_serv, start_link, [Name, Limit, self(), MFA]}, % {M, F, A}  
          permanent, 5000, worker, [ppool_serv]  
        }]  
       }}.
```
### ppool_worker_sup流程
`start_link -> (supervisor) -> init(初始化信息, 使用simpler_one_for_one)`
code
```erlang
-module(ppool_worker_sup).  
-author("勒勒").  
-behavior(supervisor).  
%% API  
-export([start_link/1, init/1]).  
  
start_link(MFA = {_, _, _}) ->  
  supervisor:start_link(?MODULE, MFA).  
  
init({M, F, A}) ->  
  io:format("worker sup init...~n"),  
  
  io:format("MFA : ~p ~n", [{M, F, A}]),  
  MaxRestart = 5,  
  MaxTime = 3600,  
  {ok, {{simple_one_for_one, MaxRestart, MaxTime},  
        [{  
          ppool_worker, {M, F, A},  
          temporary, 5000, worker, [M]  
        }]  
    }}.
```
## 进程池服务器

### ppool_serv流程图
![ppool_serv逻辑.excalidraw.png](https://cdn.acwing.com/media/article/image/2023/04/08/36510_81a1a095d6-ppool_serv逻辑.excalidraw.png) 
### code
```erlang
-module(ppool_serv).  
-author("勒勒").  
-behavior(gen_server).  
-define(SPEC(MFA), {worker_sup,  
                    {ppool_worker_sup, start_link, [MFA]},  
                    permanent, 10000, supervisor, [ppool_worker_sup]  
                   }).  
-record(state, {limit = 0, sup, refs, queue = queue:new()}). % refs为监控器的引用  
%% API  
-export([start/4, start_link/4, run/2, sync_queue/2, async_queue/2, stop/1]).  
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).  
  
start(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->  
  gen_server:start({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).  
  
start_link(Name, Limit, Sup, MFA) when is_atom(Name), is_integer(Limit) ->  
  io:format("start ppool_serv ~p ... ~n ", [Name]),  
  gen_server:start_link({local, Name}, ?MODULE, {Limit, MFA, Sup}, []).  
  
run(Name, Args) ->  
  io:format("test start run ~n"),  
  gen_server:call(Name, {run, Args}).  
  
sync_queue(Name, Args) ->  
  gen_server:call(Name, {sync, Args}, infinity).  
  
async_queue(Name, Args) ->  
  gen_server:cast(Name, {async, Args}).  
  
stop(Name) ->  
  gen_server:call(Name, stop).  
  
init({Limit, MFA, Sup}) ->  
  %% 注释部分会造成死锁!  
  %% 在ppool_sup中需要启动serv, 等待init信息返回, 而init中使用start_child需要等待ppool_sup返回  
  %% {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),  
  io:format("init ppool_serv...~n"),  
  self() ! {start_worker_supervisor, Sup, MFA},  
  {ok, #state{limit = Limit, refs = gb_sets:empty()}}.  
  
%% 获取任务结束信息  
handle_info({'DOWN', Ref, process, _Pid,  _}, S = #state{refs = Refs}) ->  
  io:format("received down msg, bool : ~p ~n", [gb_sets:is_element(Ref, Refs)]),  
  case gb_sets:is_element(Ref, Refs) of  
    true -> handle_down_worker(Ref, S);  
    false -> {noreply, S}  
  end;  
  
handle_info({start_worker_supervisor, Sup, MFA}, S = #state{}) ->  
  io:format("start worker...~n"),  
  {ok, Pid} = supervisor:start_child(Sup, ?SPEC(MFA)),  
  io:format("start worker pid : ~p ~n", [Pid]),  
  {noreply, S#state{sup = Pid}};  
  
handle_info(Msg, State) ->  
  io:format("UnKnown msg : ~p ~n", [Msg]),  
  {noreply, State}.  
  
%% 处理结束任务  
handle_down_worker(Ref, S = #state{limit = L, sup = Sup, refs = Refs}) ->  
  case queue:out(S#state.queue) of  
    {{value, {From, Args}}, Q} -> % 处理同步  
      {ok, Pid} = supervisor:start_child(Sup, Args),  
      NewRef = erlang:monitor(process, Pid),  
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)), %为什么是insert 而不是add  
      gen_server:reply(From, {ok, Pid}),  
      {noreply, S#state{refs = NewRefs, queue = Q}};  
    {{value, Args}, Q} -> % 处理异步  
      {ok, Pid} = supervisor:start_child(Sup, Args),  
      NewRef = erlang:monitor(process, Pid),  
      NewRefs = gb_sets:insert(NewRef, gb_sets:delete(Ref, Refs)),  
      {noreply, S#state{refs = NewRefs, queue = Q}};  
    {empty, _} ->  
      {noreply, S#state{limit = L + 1, refs = gb_sets:delete(Ref, Refs)}}  
  end.  
  
%% run  
handle_call({run, Args}, _From, S = #state{limit = N, sup = Sup, refs = R}) when N > 0 ->  
  io:format("args : ~p, limit : ~p, sup : ~p, refs : ~p ~n", [Args, N, Sup, R]),  
  {ok, Pid} = supervisor:start_child(Sup, Args),  
  Ref = erlang:monitor(process, Pid),  
  {reply, {ok, Pid}, S#state{limit = N - 1, refs = gb_sets:add(Ref, R)}};  
  
handle_call({run, _Args}, _From, S = #state{limit = N}) when N =< 0 ->  
  {reply, noalloc, S};  
  
%% sync_queue  
handle_call({sync, Args}, _From, S = #state{limit = N, sup = Sup, refs = R}) when N > 0 ->  
  {ok, Pid} = supervisor:start_child(Sup, Args),  
  Ref = erlang:monitor(process, Pid),  
  {reply, {ok, Pid}, S#state{limit = N - 1, refs = gb_sets:add(Ref, R)}};  
  
handle_call({sync, Args}, From, S = #state{queue = Q}) ->  
  {noreply, S#state{queue = queue:in({From, Args}, Q)}};  
  
%% stop & 未知消息  
handle_call(stop, _From, State) -> {stop, normal, ok, State};  
  
handle_call(_Msg, _From, State) -> {noreply, State}.  
  
%% async_queue  
handle_cast({async, Args}, S = #state{limit = N, sup = Sup, refs =  R}) when N > 0 ->  
  {ok, Pid} = supervisor:start_child(Sup, Args),  
  Ref = erlang:monitor(process, Pid),  
  {noreply, S#state{limit = N - 1, refs = gb_sets:add(Ref, R)}};  
  
handle_cast({async, Args}, S = #state{limit = N, queue = Q}) when N =< 0 ->  
  {noreply, S#state{queue = queue:in(Args, Q)}};  
  
handle_cast(_Msg, State) -> {noreply, State}.  
  
code_change(_OldVsn, State, _Extra) -> {ok, State}.  
  
terminate(_Reason, _State) -> ok.
```

## 工作者
### 逻辑
`主要是通过超时事件定时发送消息`
### code
```erlang
-module(ppool_nagger).  
-author("勒勒").  
-behavior(gen_server).  
  
%% API  
-export([start_link/4, stop/1]).  
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).  
  
start_link(Task, Delay, Max, SendTo) ->  
  gen_server:start_link(?MODULE, {Task, Delay, Max, SendTo}, []).  
  
stop(Pid) -> gen_server:call(Pid, stop).  
  
init({Task, Delay, Max, SendTo}) ->  
  {ok, {Task, Delay, Max, SendTo}, Delay}.  
  
%%% otp回调函数  
handle_call(stop, normal, State) ->  
  {stop, normal, ok, State};  
  
handle_call(_Msg, _From, State) -> {noreply, State}.  
  
handle_cast(_Msg, State) -> {noreply, State}.  
  
%% 通过超时的方式发送消息  
handle_info(timeout, {Task, Delay, Max, SendTo}) ->  
  SendTo ! {self(), Task},  
  io:format("send task : ~p ~n", [Task]),  
  if Max =:= infinity ->  
       {noreply, {Task, Delay, Max, SendTo}, Delay};  
     Max =< 1 ->  
       {stop, normal, {Task, Delay, 0, SendTo}};  
     Max > 1 ->  
       {noreply, {Task, Delay, Max - 1, SendTo}, Delay}  
  end.  
  
code_change(_OldVsn, State, _Extra) -> {ok, State}.  
  
terminate(_Reason, _State) -> ok.
```

## [修改为OTP应用](https://git.acwing.com/fluentx/ppool/-/tree/main/) 


## 运行进程池

```erlang
erl -make 

erl -pa ebin/

make:all([load]).

application:start(ppool).

ppool:start_pool(nagger, 2, {ppool_nagger, start_link, []}).

%% 同步不入队
ppool:run(nagger, ["finish the chapter!", 1000, 5, self()]).
ppool:run(nagger, ["Watch a good movie!", 1000, 5, self()]).
ppool:run(nagger, ["Playing game!", 1000, 5, self()]).
flush().

%% 同步入队
ppool:sync_queue(nagger, ["finish the chapter!", 1000, 5, self()]).  
ppool:sync_queue(nagger, ["Watch a good movie!", 1000, 5, self()]).
ppool:sync_queue(nagger, ["Playing game!", 1000, 5, self()]).

%% 异步入队
ppool:async_queue(nagger, ["finish the chapter!", 1000, 5, self()]).  
ppool:async_queue(nagger, ["Watch a good movie!", 1000, 5, self()]).
ppool:async_queue(nagger, ["Playing game!", 1000, 5, self()]).

%% 查看当前运行的应用
application:which_applications().

application:stop(ppool).
```

<!-- ##{"timestamp":1680962400}## -->