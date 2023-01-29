-module(sets).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

no_duplicates(Lst) -> 
    length(Lst) =:= length(lists:usort(Lst)).

prop_unique_keys() ->
    ?FORALL(D, dict(),
        no_duplicates(dict:fetch_keys(D))).





