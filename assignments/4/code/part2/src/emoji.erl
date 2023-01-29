-module(emoji).

% -compile(export_all).
-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

% -type shortcode() :: string().
% -type emoji() :: binary().
% -type analytic_fun(State) :: fun((shortcode(), State) -> State).

% Finish
start(Initial) -> 
    A = getShortCodes1(Initial),
    case checkDuplicate(A) of
        true ->
            try 
                Newlist = addelement(Initial),
                E = spawn(fun() -> loop(Newlist) end),
                {ok,E}
            catch
                _:Reason -> {error,Reason}
            end;
        false -> {error,"duplicate shortcode"}
    end.

% Finish
getShortCodes1(List) ->
    case List of
        [] -> [];
        [{Short,_}|Tail] -> [Short] ++ getShortCodes1(Tail)
    end.

% Finish
checkDuplicate(Initial) ->
    case Initial of
        [] -> true;
        [_ | []] -> true;
        [Head | Tail] -> 
            case lists:member(Head,Tail) of
                true -> false;
                false -> checkDuplicate(Tail)
            end
    end.

% Finish
new_shortcode(E, Short, Emoji) -> 
    E ! {self(),newshortcode,Short,Emoji},
    receive
        {E,ok} -> ok;
        {E,{error,Reason}} -> {error,Reason}
    end.


alias(E, Short, Alias) -> 
    E ! {self(),alias,Short,Alias},
    receive
        {E,ok} -> ok;
        {E,{error,Reason}} -> {error,Reason}
    end.

% Finish
delete(E, Short) ->  
    E ! {self(),delete,Short},
    ok.

lookup(E, Short) -> 
    E ! {self(),lookup,Short}, 
    receive
        {E,{ok,Emoji}} -> {ok,Emoji};
        {E,no_emoji} -> no_emoji
    end.

analytics(E, Short, Fun, Label, Init) -> 
    E ! {self(),analytics,Short,Fun,Label,Init},
    receive
        {E,ok} -> ok;
        {E,{error,Reason}} -> {error,Reason}
    end.

get_analytics(E, Short) -> 
    E ! {self(),get_analytics,Short},
    receive
        {E,{ok,Stat}} -> {ok,Stat};
        {E,{error,Reason}} -> {error,Reason}
    end.

remove_analytics(E, Short, Label) -> 
    E!{self(),remove_analytics,Short,Label},
    ok.

stop(E) -> 
    E ! {self(),stop},
    receive 
        {E,ok} -> ok;
        {E,{error,Reason}} -> {error, Reason}
    end.

% test(E) -> 
%     E ! {self(),test},
%     receive 
%         Res -> Res
%     end.

loop(List)->
    ShortCodes = getShortCodes(List),
    receive
        % {Pid,test} ->
        %     Pid ! {List,ShortCodes},
        %     loop(List);
        {Pid,newshortcode,Short,Emoji} -> 
            case lists:member(Short,ShortCodes) of
                true -> 
                    Pid ! {self(),{error, "shortcode already had"}},
                    loop(List);
                false -> 
                    Pid ! {self(),ok},
                    loop([{Short,Emoji,[Short],[]}|List])
            end;
        {Pid,alias,Short,Alias} ->
            case lists:member(Short,ShortCodes) of
                true -> 
                    case lists:member(Alias,ShortCodes) of
                        true -> 
                            Pid ! {self(),{error, "alias already had"}},
                            loop(List);
                        false -> 
                            Pid ! {self(),ok},
                            RealName = getRealName(List,Short),
                            Newlist = addAlias(List,RealName,Alias),
                            loop(Newlist)
                    end;
                false -> 
                    Pid ! {self(),{error, "shortcode"++ Short ++" not found"}},
                    loop(List)
            end;
        {_,delete,Short} ->
            case lists:member(Short,ShortCodes) of
                true ->
                    RealName = getRealName(List,Short),
                    Aliases = getAliasList(List,RealName),
                    loop(deletehelper(Aliases,List));
                false->loop(List)
            end;
        {Pid,lookup,Short} ->
            case lists:member(Short,ShortCodes) of
                false->
                    Pid ! {self(),no_emoji},
                    loop(List);
                true->
                    RealName = getRealName(List,Short),
                    Emoji = findEmoji(RealName,List),
                    Pid ! {self(),{ok,Emoji}},
                    NewList = runFun(List,RealName,Short),
                    loop(NewList)
            end;
        {Pid,analytics,Short,Fun,Label,Init} ->
            case lists:member(Short,ShortCodes) of
                false->
                    Pid ! {self(),{error, "shortcode not found"}},
                    loop(List);
                true->
                    RealName = getRealName(List,Short),
                    case checkLabel(List,Label,Short) of
                        true ->
                            Pid ! {self(),{error, "label already had"}},
                            loop(List);
                        false ->
                            Pid ! {self(),ok},
                            Newlist = addAnalytics(List,RealName,Fun,Label,Init),
                            loop(Newlist)
                    end
            end;
        {Pid,get_analytics,Short} ->
            case lists:member(Short,ShortCodes) of
                false->
                    Pid ! {self(),{error, "shortcode not found"}},
                    loop(List);
                true->
                    RealName = getRealName(List,Short),
                    Stat = getAnalyticsStat(List,RealName),
                    Pid ! {self(),{ok,Stat}},
                    loop(List)
            end;
        {Pid,remove_analytics,Short,Label} ->
            case lists:member(Short,ShortCodes) of
                false->
                    loop(List);
                true->
                    RealName = getRealName(List,Short),
                    Newlist = removeAnalytics(List,RealName,Label),
                    Pid ! {self(),ok},
                    loop(Newlist)
            end;
        {Pid,stop} ->
            Pid ! {self(),ok};
        {Pid,_,_} ->
            Pid ! {self(),{error, "invalid command"}},
            loop(List)
    end.

% used for Initial list to adjust the input to the right format
addelement(Initial) ->
    case Initial of
        [] -> [];
        [{Short,Emoji}|Tail] -> [{Short,Emoji,[Short],[]}|addelement(Tail)]
    end.

% get all the shortcodes including the alias
getShortCodes(List) ->
    case List of
        [] -> [];
        [{_,_,Alias,_}|Tail] -> Alias ++ getShortCodes(Tail)
    end.

% remove all those with Shortcode in alias list
deletehelper(Aliases,List)-> 
    case List of
        []->[];
        [{Short,Emoji,Alias,Fun}|Tail]->
            case lists:member(Short,Aliases) of
                true->deletehelper(Aliases,Tail);
                false->[{Short,Emoji,Alias,Fun}|deletehelper(Aliases,Tail)]
            end
    end.

% find the emoji of the shortcode
findEmoji(Short,List)->
    case List of
        []->[];
        [{Short1,Emo,_,_}|Tail]->
            case Short1 == Short of
                true->Emo;
                false->findEmoji(Short,Tail)
            end
    end.

% finished
% find the original name of the shortcode
getRealName(List,Short)->
    case List of
        []->[];
        [{Short1,Emoji,Alias,_}|Tail]->
            case Short1 == Short of
                true ->
                    case Emoji of
                        none -> Alias;
                        _ -> Short1
                    end;
                false -> getRealName(Tail,Short)
            end
    end.

getAliasList(List,RealName) ->
    case List of
        []->[];
        [{Short,_,Alias,_}|Tail]->
            case Short == RealName of
                true -> Alias;
                false -> getAliasList(Tail,RealName)
            end
    end.
% add the alias to the list
addAlias(List,Short,Alias)->
    case List of
        []->[];
        [{S,Emoji,AliasList,Fun}|Tail]->
            case S == Short of
                true -> [{S,Emoji,[Alias|AliasList],Fun}|[{Alias,none,S,Fun}|Tail]];
                false -> [{S,Emoji,AliasList,Fun}|addAlias(Tail,Short,Alias)]
            end
    end.

% check if the label is already in the list
checkLabel(List,Label,Short)->
    case List of
        []->false;
        [{Short1,_,_,Fun}|Tail]->
            case Short1 == Short of
                true ->
                    case Fun of
                        []->false;
                        [{Label1,_,_}|_] ->
                            case Label1 == Label of
                                true->true;
                                false->checkLabel(Tail,Label,Short)
                            end
                    end;
                false->checkLabel(Tail,Label,Short)
            end
    end.

% add the analytics to the list
addAnalytics(List,Short,Fun,Label,Init)->
    case List of
        []->[];
        [{Short1,Emoji,Alias,FunList}|Tail]->
            case Short1 == Short of
                true -> [{Short1,Emoji,Alias,[{Label,Fun,Init}|FunList]}|Tail];
                false -> [{Short1,Emoji,Alias,FunList}|addAnalytics(Tail,Short,Fun,Label,Init)]
            end
    end.

% get the stat of analytics of the shortcode
getAnalyticsStat(List,Short)->
    case List of
        []->[];
        [{Short1,_,_,Fun}|Tail]->
            case Short1 == Short of
                true -> gethelper(Fun);
                false -> getAnalyticsStat(Tail,Short)
            end
    end.

gethelper(List) ->
    case List of
        [] -> [];
        [{Label,_,State}|Tail1] -> [{Label,State}| gethelper(Tail1)]
    end.

% remove the analytics with the shortcode and Label
removeAnalytics(List,RealName,Label)->
    case List of
        []->[];
        [{S,Emoji,Alias,Fun}|Tail]->
            case S == RealName of
                true -> [{S,Emoji,Alias,removehelper(Fun,Label)}|Tail];
                false -> [{S,Emoji,Alias,Fun}|removeAnalytics(Tail,RealName,Label)]
            end
    end.

removehelper(List,Label) ->
    case List of
        []->[];
        [{Label1,Func,State}|Tail]->
            case Label1 == Label of
                true->Tail;
                false->[{Label1,Func,State}|removehelper(Tail,Label)]
            end
    end.

runFun(List,RealName,Short) ->
    case List of
        []->[];
        [{Short1,Emoji,Alias,Fun}|Tail]->
            case Short1 == RealName of
                true -> 
                    NewFun = runhelper(Fun,Short),
                    [{Short1,Emoji,Alias,NewFun}|Tail];
                false -> 
                    [{Short1,Emoji,Alias,Fun}|runFun(Tail,RealName,Short)]
            end
    end.

runhelper(List,Short) ->
    case List of
        [] -> [];
        [{Label,Func,State}|Tail] ->
            try 
                NewState = Func(Short,State),
                [{Label,Func,NewState}|runhelper(Tail,Short)]
            catch
                _ -> [{Label,Func,State}|runhelper(Tail,Short)]
            end
    end.