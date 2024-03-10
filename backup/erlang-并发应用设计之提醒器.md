# 事件处理器

## 图解
![event-事件处理器.excalidraw.png](https://cdn.acwing.com/media/article/image/2023/04/05/36510_f828b03cd3-event-事件处理器.excalidraw.png) 

## version 2.0
```
-module(curling).
-author("勒勒").
%% API
-export([start_link/2, add_points/3, next_round/1, join_feed/2, leave_feed/2, game_info/1]).

start_link(TeamA, TeamB) ->
  {ok, Pid} = gen_event:start_link(),
  %% 记分板
  gen_event:add_handler(Pid, curling_scoreboard, []),
  %% 启动比赛状态累加器
  gen_event:add_handler(Pid, curling_accumulator, []),
  set_teams(Pid, TeamA, TeamB),
  {ok, Pid}.

set_teams(Pid, TeamA, TeamB) ->
  gen_event:notify(Pid, {set_teams, TeamA, TeamB}).

add_points(Pid, Team, N) ->
  gen_event:notify(Pid, {add_points, Team, N}).

next_round(Pid) -> gen_event:notify(Pid, next_round).

%% 为进程ToPid订阅比赛消息
join_feed(Pid, ToPid) ->
  HandlerId = {curling_feed, make_ref()},
  gen_event:add_handler(Pid, HandlerId, [ToPid]),
  HandlerId.

leave_feed(Pid, HandlerId) ->
  gen_event:delete_handler(Pid, HandlerId, leave_feed).

%% 返回当前比赛状态, 为迟到订阅的提供
game_info(Pid) ->
  gen_event:call(Pid, curling_accumulator, game_data).
```

## callback
- 通用callback
```
-module(gen_event_callback).
-author("勒勒").
-behavior(gen_event).
%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init([]) -> [].

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
```

- curling_scoreboard
```
-module(curling_scoreboard).
-author("勒勒").
-import(curling_scoreboard_hw, [set_teams/2, add_point/1, reset_board/1, next_round/0]).
%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2]).

init([]) -> {ok, []}.

handle_event({set_teams, TeamA, TeamB}, State) ->
  io:format("set teams"),
  curling_scoreboard_hw:set_teams(TeamA, TeamB),
  {ok, State};

handle_event({add_points, Team, N}, State) ->
  [curling_scoreboard_hw:add_point(Team) || _ <- lists:seq(1, N)],
  {ok, State};

handle_event(next_round, State) ->
  curling_scoreboard_hw:next_round(),
  {ok, State};

handle_event(_, State) -> {ok, State}.

handle_call(_, State) -> {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

```
- curling_feed
```
-module(curling_feed).
-author("勒勒").
-behavior(gen_event).
%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).

init([Pid]) -> {ok, Pid}.

handle_event(Event, Pid) ->
  Pid ! {curling_feed, Event},
  {ok, Pid}.

handle_call(_, State) -> {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

```
- curling_accumulator

```
-module(curling_accumulator).
-author("勒勒").
-behavior(gen_event).
%% API
-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2]).
-record(state, {teams = orddict:new(), round = 0}).

init([]) -> {ok, #state{}}.

handle_event({set_teams, TeamA, TeamB}, S = #state{teams = T}) ->
  Teams = orddict:store(TeamA, 0, orddict:store(TeamB, 0, T)),
  {ok, S#state{teams =  Teams}};

handle_event({add_points, Team, N}, S = #state{teams = T}) ->
  Teams = orddict:update_counter(Team, N, T), %% 如果存在则 Val += incr, 不存在添加一个Key Val(为空直接添加, 否则依次寻找)
  {ok, S#state{teams = Teams}};

handle_event(next_round, S = #state{}) ->
  {ok, S#state{round = S#state.round + 1}};

handle_event(_Event, State = #state{}) -> {ok, State}.

handle_call(game_data, S = #state{teams = T, round = R}) ->
  {ok, {orddict:to_list(T), {round, R}}, S};

handle_call(_, State) -> {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
```

## mock curling_scoreboard_hw
```
-module(curling_scoreboard_hw).
-author("勒勒").

%% API
-export([set_teams/2, next_round/0, add_point/1, reset_board/0]).

%% 在计分板上显示参赛队伍
set_teams(TeamA, TeamB) ->
  io:format("Scoreboard: Team ~s vs Team ~s ~n", [TeamA, TeamB]).

next_round() ->
  io:format("Scoreboard: round over ~n").

add_point(Team) ->
  io:format("Scoreboard:increased score of team ~s by 1~n", [Team]).

reset_board() ->
  io:format("Scoreboard: All teams are undefined and al1 scores are 0~n").

```

<!-- ##{"timestamp":1679839200}## -->