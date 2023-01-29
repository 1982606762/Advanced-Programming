-module(cfib).
-compile(export_all).

cfib(0) -> 0;
cfib(1) -> 1;

cfib(N) ->
    Me = self(),
    _Ch1 = spawn(fun() -> Me ! cfib(N-1) end),
    _Ch2 = spawn(fun() -> Me ! cfib(N-2) end),
        receive
            N1 ->
            receive
                N2 -> 
                    io:format(N1,N2),
                    N1 + N2
        end
    end.

reverse([]) -> [];
reverse([H|T]) -> reverse(T)++[H].