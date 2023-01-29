-module(q).
-compile(export_all).
-include_lib("eqc/include/eqc.hrl").


key() -> 
    eqc_gen:oneof([atom(), eqc_gen:int(), eqc_gen:real()]). 
value() ->
    eqc_gen:oneof([eqc_gen:int(), atom()]). 
atom() ->
    eqc_gen:elements([a,b,c,d]). 
no_duplicates(Lst) -> 
    length(Lst) =:= length(lists:usort(Lst)).  

dict_1() ->
?LAZY( 
    eqc_gen:oneof([{call,dict,new,[]}, 
        ?LET(D, dict_1(), 
        {call,dict,store,[key(),value(),D]})])
).

dict_2() -> 
    ?LAZY( 
        eqc_gen:frequency([
            {1,{call,dict,new,[]}}, 
            {4,?LET(D, dict_2(), 
                {call,dict,store,[key(),value(),D]})
            }]) 
        ).

dict_3() ->
    ?LAZY( 
        eqc_gen:frequency([
            {1,{call,dict,new,[]}}, 
            {4,?LETSHRINK([D],[dict_3()], 
                {call,dict,store,[key(),value(),D]})
            }]) 
        ).

prop_unique_keys() ->
    ?FORALL(
    D,dict_1(), 
    no_duplicates(dict:fetch_keys(D))
).
model(Dict) -> dict:to_list(Dict).

model_store(K, V, M) -> [ {K,V} | M ].
prop_store() ->
    ?FORALL({K,V,D}, 
    {key(),value(),dict_3()}, 
    begin Dict = eval(D), 
    equals(model(dict:store(K,V,Dict)), 
    model_store(K,V,model(Dict))) 
    end).