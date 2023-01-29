-module(qwe).
-compile(export_all).

convert ([]) -> [];
convert ([H|T]) -> case T of
                        "C" -> [5/9*(H-32)|"F"];
                        "F" -> [9/5*H+32|"C"]
                    end.

reverse ([]) -> [];
reverse ([H|T]) -> [reverse(T)|H].