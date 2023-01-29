-module(exercise).
-compile(export_all).

sum(A) -> lists:sum(A).
merge(A, B) -> 
    if A == [] -> B;
       B == [] -> A;
       true -> lists:merge(A,B)
    end.