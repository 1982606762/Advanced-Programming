-module(test). 
% -export([move/2]).
-compile(export_all).

add(X,Y) -> X+Y.
move(Dir,{X,Y})-> 
    if X>=0,Y>=0 -> 
        case Dir of 
            up -> {X,Y+1};
            down -> {X,Y-1};
            left -> {X-1,Y};
            right -> {X+1,Y}
        end;
        true -> throw(asdasdada)
    end.

