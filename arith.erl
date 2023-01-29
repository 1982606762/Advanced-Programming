-module(arith).
-export([add/2,sub/2,mul/2,ddiv/2]).
add(X,Y) -> X+Y.
sub(X,Y) -> X-Y.
mul(X,Y) -> X*Y.
ddiv(X,Y) -> X div Y.
%
% -module(test).
% -compile(export_all).
