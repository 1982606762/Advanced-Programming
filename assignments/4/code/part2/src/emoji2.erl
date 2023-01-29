-module(emoji2).

-compile(export_all).
% -export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
        %  analytics/5, get_analytics/2, remove_analytics/3,
        %  stop/1]).

% -type shortcode() :: string().
% -type emoji() :: binary().
% -type analytic_fun(State) :: fun((shortcode(), State) -> State).
-behaviour(gen_server).

% ---------------------User API---------------------
start(Init) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Init, []).

new_shortcode(E,Short,Emoji) ->
    gen_server:call(E,{new_shortcode,Short,Emoji}).

delete(E,Short) ->
    gen_server:cast(E,{delete,Short}),
    ok.

alias(E,Short,Alias) ->
    gen_server:call(E,{alias,Short,Alias}).

lookup(E,Short) ->
    gen_server:call(E,{lookup,Short}).

analytics(E, Short, Fun, Label, Init) ->
    gen_server:call(E, {analytics, Short, Fun, Label, Init}).

stop(E) ->
    gen_server:stop(E).

show(E) ->
    gen_server:call(E, show).

% -----------------------helper functions------------------------------------------

check_dup(List) ->
    case List of
        [] -> true;
        [_|[]] -> true;
        [H|T] -> case lists:member(H, T) of
            true -> false;
            false -> check_dup(T)
        end
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

deleteAliasList(AliasList,Status) ->
    case Status of
        [] -> [];
        [Short,Emoji,Alias,Fun|Tail] ->
            case lists:member(Short,AliasList) of
                true -> deleteAliasList(AliasList,Tail);
                false -> [Short,Emoji,Alias,Fun] ++ deleteAliasList(AliasList,Tail)
            end
    end.

addAlias(State,RealName,Alias) ->
    case State of
        [] -> [];
        [{Short,Emoji,Aliases,Fun}|Tail] ->
            case Short == RealName of
                true -> [{Short,Emoji,[Alias|Aliases],Fun}|[{Alias,none,RealName,Fun}|Tail]];
                false -> [{Short,Emoji,Aliases,Fun}| addAlias(Tail,RealName,Alias)]
            end
    end.

getEmoji(State,RealName) ->
    case State of
        [] -> [];
        [{Short,Emoji,_,_}|Tail] ->
            case Short == RealName of
                true -> Emoji;
                false -> getEmoji(Tail,RealName)
            end
    end.

addAnalytics(State,RealName,Fun,Label,Init) ->
    case State of
        [] -> [];
        [{Short,Emoji,Aliases,Analytics}|Tail] ->
            case Short == RealName of
                true -> [{Short,Emoji,Aliases,[[Fun,Label,Init]|Analytics]}|Tail];
                false -> [{Short,Emoji,Aliases,Analytics}|addAnalytics(Tail,RealName,Fun,Label,Init)]
            end
    end.

% -----------------------Server API-----------------------

init(Init) ->
    case check_dup(Init) of
        true -> 
            try addelement(Init) of
                Initial -> {ok, Initial}
            catch
                _:Reason -> {error, Reason}
            end;
        false -> Reason="Duplicate shortcode",{error, Reason}
    end.

handle_cast({delete,Short},Status) ->
    case lists:member(Short, getShortCodes(Status)) of
        true -> 
            RealName = getRealName(Status,Short),
            AliasList = getAliasList(Status,RealName),
            NewStatus = deleteAliasList(AliasList,Status),
            {noreply,NewStatus};
        false -> {noreply, Status}
    end.

handle_call({new_shortcode, Short, Emoji}, _From, State) ->
    case lists:member(Short, getShortCodes(State)) of
        true -> {reply, {error, "shortcode already exists"}, State};
        false -> {reply, ok, [{Short,Emoji,[Short],[]}|State]}
    end;
handle_call({alias, Short, Alias}, _From, State) ->
    case lists:member(Short, getShortCodes(State)) of
        true -> 
            case lists:member(Alias, getShortCodes(State)) of
                true -> {reply, {error, "The shortcode cannot be used as an alias because it is a shortcode"}, State};
                false -> 
                    RealName = getRealName(State,Short),
                    NewState = addAlias(State,RealName,Alias),
                    {reply, ok, NewState}
            end;
        false -> {reply, {error, "The shortcode cannot be an alias because it does not exist"}, State}
    end;
handle_call({lookup, Short}, _From, State) ->
    case lists:member(Short, getShortCodes(State)) of
        true -> 
            RealName = getRealName(State,Short),
            Emoji = getEmoji(State,RealName),
            {reply, Emoji, State};
        false -> {reply, {error, "The shortcode cannot be found in the system"}, State}
    end;
handle_call({analytics, Short, Fun, Label, Init}, _From, State) ->
    case lists:member(Short, getShortCodes(State)) of
        true -> 
            RealName = getRealName(State,Short),
            NewState = addAnalytics(State,RealName,Fun,Label,Init),
            {reply, ok, NewState};
        false -> {reply, {error, "The shortcode cannot be found in the system"}, State}
    end;
handle_call(show, _From, State) ->
    {reply, State, State}.

terminate(_Reason, _State) ->
    ok.