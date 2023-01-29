-module(async).

-behaviour(gen_server).
-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).
-export([init/1,handle_cast/2,handle_call/3]).

new(Fun, Arg) -> 
    case gen_server:start(?MODULE, {Fun, Arg}, []) of
        {ok, Pid} -> Pid;
        {error, Reason} -> throw(Reason)
    end.

wait(Aid) -> 
    case gen_server:call(Aid, wait) of
        {ok, Result} -> Result;
        {error, Reason} -> throw(Reason);
        _ -> wait(Aid)
    end.

poll(Aid) -> 
    case gen_server:call(Aid, poll) of
        {ok, Result} -> Result;
        {error, Reason} -> Reason;
        _ -> nothing
    end.

wait_catch(Aid) -> 
    case gen_server:call(Aid, wait) of
        {ok, Result} -> Result;
        {error, Reason} -> {exception,Reason};
        _ -> wait_catch(Aid)
    end.

wait_any([]) -> [];
wait_any([Head|Aids]) -> 
    case wait_catch(Head) of
        {exception, Reason} -> [{Head, Reason}|wait_any(Aids)];
        Result -> [{Head, Result}|wait_any(Aids)]
    end.

init({Fun, Arg}) -> 
    Me = self(),
    spawn(
        fun() ->
            try
                Res = Fun(Arg),
                gen_server:cast(Me, {ok, Res})
            catch
                Reason -> gen_server:cast(Me, {error, Reason})
            end
        end),
    {ok, {non,non}}.%传入的是loop的初始状态

handle_cast({ok, Res}, _) -> 
    {noreply, {ok, Res}};
handle_cast({error, Reason}, _) ->
    {noreply, {error, Reason}}.

handle_call(wait, _From, State) -> %wait是一个call,接受来自第二个进程的消息，第三个参数是loop的状态
    {reply, State, State};
handle_call(poll, _From, State) ->
    {reply, State, State}.
