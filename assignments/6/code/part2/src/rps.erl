-module(rps).
-behaviour(gen_server).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).
-export([init/1, handle_call/3, handle_cast/2, coordLoop/7]).

start() -> gen_server:start_link(?MODULE, [], []).
coordLoop(A,B,C,D,E,F,G) -> rps_game:coordLoop(A,B,C,D,E,F,G).

init([]) ->
    State = {0, [], [], false, {"nothing", noone}},
    Return = {ok, State},
    Return.

% State = {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}
% InQueue = [{Name, ID, Rounds}...]
% Ongoing = [{FromID, OppID, CoordID}...],

% Finish
handle_call(are_we_draining, _From, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    if 
        Draining -> Return = {reply, yes, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}};
        true -> Return = {reply, no, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}}
    end,
    Return;

handle_call({statistics}, _From, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    Return = {reply, {ok, LongestGame, erlang:length(InQueue), erlang:length(Ongoing)}, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}},
    Return.



% Finish
handle_cast({done_draining, {P1ID, P2ID, From}}, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    NewOngoing = lists:delete({P1ID, P2ID, From}, Ongoing),
    if 
        erlang:length(NewOngoing) > 0 -> 
            Return = {noreply, {LongestGame, InQueue, NewOngoing, Draining}},
            Return;
        true -> 
            DrainID ! DrainMsg,
            gen_server:stop(self())
    end;

% Finish
handle_cast({drain, PID, Msg}, {LongestGame, InQueue, Ongoing, _, {_, _}}) ->
    % 给InQueue里每个人发server_stopping
    lists:map(fun({_, ID, _}) -> ID ! {error, server_stopping} end, InQueue),
    if
        erlang:length(Ongoing) > 0 ->
            Return = {noreply, {LongestGame, [], Ongoing, true, {Msg, PID}}},
            Return;
        true -> 
            PID ! Msg,
            gen_server:stop(self())
    end;

% Finish
handle_cast({game_ended, GameLen, {P1ID, P2ID, GameID}}, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    if
        GameLen > LongestGame -> 
            TmpLongest = GameLen;
        true -> TmpLongest = LongestGame
    end,
    NewOngoing = lists:delete({P1ID, P2ID, GameID}, Ongoing),
    Return = {noreply, {TmpLongest, InQueue, NewOngoing, Draining, {DrainMsg, DrainID}}},
    Return;

% Finish
% 给匹配的玩家发送server_stopping消息并不改变服务器状态
handle_cast({queue_up, _, _, FromID}, {LongestGame, InQueue, Ongoing, true, {DrainMsg, DrainID}}) ->
    FromID ! server_stopping,
    Return = {noreply, {LongestGame, InQueue, Ongoing, true, {DrainMsg, DrainID}}},
    Return;

% Finish
handle_cast({queue_up, Name, Rounds, FromID}, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    if 
        Rounds < 1 -> 
            FromID ! {error, nonpositive_rounds},
            {noreply, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}};
        true ->
            case lists:keysearch(Rounds, 3, InQueue) of
                {value, {OppName, OppID, _}} -> 
                    CoordID = spawn(rps, coordLoop, [self(), FromID, OppID, 0, 0, Rounds, 0]),
                    OppID ! {ok, Name, CoordID},
                    FromID ! {ok, OppName, CoordID},
                    NewQueue = lists:delete({OppName, OppID, Rounds}, InQueue),
                    NewOngoing = [{FromID, OppID, CoordID} | Ongoing],
                    Return = {noreply, {LongestGame, NewQueue, NewOngoing, Draining, {DrainMsg, DrainID}}};
                false ->
                    NewQueue = [{Name, FromID, Rounds} | InQueue],
                    Return = {noreply, {LongestGame, NewQueue, Ongoing, Draining, {DrainMsg, DrainID}}}
            end,
            Return
    end.

% Finish
queue_up(BrokerRef, Name, Rounds) -> 
    gen_server:cast(BrokerRef, {queue_up, Name, Rounds, self()}),
    receive
        {ok, OppName, Coordinator} -> {ok, OppName, Coordinator};
        server_stopping -> server_stopping;
        {error, Error} -> {error, Error}
    end.

move(Coordinator, Choice) -> 
    Coordinator ! {self(), {move, Choice}},
    receive
        Reply -> Reply
    end.

statistics(BrokerRef) -> 
    Reply = gen_server:call(BrokerRef, {statistics}),
    Reply.

% Finish
drain(BrokerRef, Pid, Msg) -> gen_server:cast(BrokerRef, {drain, Pid, Msg}).
