-module(d).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").

key() -> 
    eqc_gen:oneof([atom(), eqc_gen:int(), eqc_gen:real()]). 
value() ->
    eqc_gen:oneof([eqc_gen:int(), atom()]). 
atom() ->
    eqc_gen:elements([a,b,c,d]). 
dict_0() ->
    ?LAZY(
        eqc_gen:oneof([
            dict:new(),
            ?LET({K,V,D},{key(),value(),dict_0()},
            dict:store(K,V,D))
        ])).      
no_duplicates(Lst) -> 
    length(Lst) =:= length(lists:usort(Lst)).  

prop_unique_keys() ->
    ?FORALL(D, dict_0(), 
        no_duplicates(dict:fetch_keys(D))).