-module(test_time).
-export([time_test/0]).
-import(cfib,[cfib/1]).

time_test() ->
    
    % 执行一次 statistic(runtime) 或 statistic(wall_clock) 让其记录初始调用的时间
    statistics(runtime),
    statistics(wall_clock),
    
    cfib(10),
    %% 业务代码 ...
 
    {_, Time} = statistics(runtime),      % 返回第二个参数表示距离上次调用该函数的时间差（毫秒）
    {_, Time2} = statistics(wall_time),   % 同上
    
    io:format("Run Time ~p Milliseconds: ", [Time]),
    io:format("Run Time ~p Milliseconds: ", [Time2]).