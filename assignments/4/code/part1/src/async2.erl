-module(async2).

-export([new/2, wait/1, poll/1,wait_catch/1,wait_any/1]).

new(Fun, Arg) -> spawn(fun() -> 
    Me = self(),
    spawn(fun() ->
      try
        Res = Fun(Arg),
        Me ! {ok, Res}
      catch
        _:Reason -> Me ! {error, Reason}
      end
    end),
    loop({a,a})
  end
).

poll(Aid) -> 
    Aid ! {self(),poll},
    receive
        {done,Res} -> {ok,Res};
        {error,Reason} -> Reason;
        {_,_} -> nothing
end.

wait(Aid) -> 
    Aid ! {self(),wait},
    receive
        {done,Res} -> Res;
        {error,Reason} -> throw(Reason)
end.

wait_catch(Aid) ->
    Aid ! {self(),wait_catch},
    receive
        {done,Res} -> Res;
        {error,Reason} -> {exception,Reason};
        {_,_} -> wait_catch(Aid)
end.

wait_any([]) ->[];
wait_any([Head|Aids]) ->
    case wait_catch(Head) of
        {exception,Reason} -> throw(Reason);
        Res -> [{Head,Res}|wait_any(Aids)]
    end.

loop(State) ->
    receive
        {From,wait} ->
            case State of
                {done,_} -> From ! State;
                {error,_} -> From ! State;
                _ -> receive {ok,Res} -> From ! {done,Res}; 
                            {error,Reason} -> From ! {error,Reason} end
            end,
            loop(State);
        {From,poll} ->
            From ! State,
            loop(State);
        {ok,Res} ->
            loop({done,Res});
        {error,Reason} ->
            loop({error,Reason});
        {From,wait_catch} ->
            From ! State,
            loop(State)
    end.

% c(async).
% E = async:new(fun(X)->timer:sleep(5000),X+1 end, 1).
% async:poll(E).  
% async:wait(E).