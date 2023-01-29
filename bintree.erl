-module(bintree).
-compile(export_all).

bst_create() -> {}.

bst_insert(Bst, N) ->
  case Bst of
    {} -> {N, {}, {}};
    {Root, Lhs, Rhs} ->
      if
        N == Root -> Bst;
        N < Root -> {Root, bst_insert(Lhs, N), Rhs};
        N > Root -> {Root, Lhs, bst_insert(Rhs, N)}
      end
  end.

bst_search(Bst, N) ->
  case Bst of
    {} -> false;
    {Root, Lhs, Rhs} ->
      if
        N == Root -> true;
        N < Root -> bst_search(Lhs, N);
        N > Root -> bst_search(Rhs, N)
      end
  end.

inorder(Bst) ->
  case Bst of
    {} -> {};
    {Root, Lhs, Rhs} -> inorder(Lhs) ++ [Root] ++ inorder(Rhs)
  end.

eq_tree(A,B) ->
    io:format("A: ~p~nB: ~p~n",[inorder(A),inorder(B)]),
    case {inorder(A),inorder(B)} of
        {S,S} -> true;
        _ -> false
    end.

eq_tree2(A,B) ->
    Me = self(),
    _Ch1 = spawn(fun() -> Me ! inorder(A) end),
    _Ch2 = spawn(fun() -> Me ! inorder(B) end),
        receive
            N1 ->
                receive
                    N2 -> case {N1,N2} of
                            {S,S} -> true;
                            _ -> false
                            end
                end
        end.
