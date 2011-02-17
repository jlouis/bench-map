-module(patricia).
%% @todo Go over all the ordering values and check them.

-export([insert/2, is_element/2, from_list/1, new/0]).

-define(BIT_MAX, 27). % Set by virtue of erlang:phash2
-type ptree(A) :: empty | {leaf, [A]} | {node, pos_integer(), ptree(A), ptree(A)}.



hash(X) ->
    erlang:phash2(X).

from_list(L) ->
    lists:foldl(
      fun insert/2,
      new(),
      L).

new() ->
    empty.

-spec insert(A, ptree(A)) -> ptree(A) | already.
insert(E, empty) ->
    {leaf, [E]};
insert(E, Tree) ->
    H = hash(E),
    {Bit, Lt} = find_bit(H, Tree),
    insert(H, E, Bit, Lt, Tree).

-spec find_bit(integer(), ptree(_)) -> {pos_integer(), boolean()}.
find_bit(H, {leaf, [A | _]}) ->
    H1 = hash(A),
    crit_bit(H, H1);
find_bit(H, {node, Bit, Left, Right}) ->
    case inspect_bit(H, Bit) of
	left -> find_bit(H, Left);
	right -> find_bit(H, Right)
    end.

crit_bit(I1, I2) ->
    crit_bit(I1, I2, ?BIT_MAX).

crit_bit(I1, I2, N) ->
    Bit = (1 bsl N),
    case (Bit band I1) bxor (Bit band I2) of
	0 ->
	    crit_bit(I1, I2, N-1);
	_ ->
	    {N, cmp_lt_bit(I1, I2, N)}
    end.

-spec cmp_lt_bit(integer(), integer(), pos_integer()) -> boolean().
cmp_lt_bit(I1, I2, N) ->
    Bit = (1 bsl N),
    (Bit band I1) < (Bit band I2).


inspect_bit(H, Bit) ->
    case H band (1 bsl Bit) of
	0 -> left;
	_ -> right
    end.


insert(_H, E, Bit, Lt, {leaf, Es} = Lf) ->
    case lists:member(E, Es) of
	true ->
	    {leaf, Es};
	false ->
	    case Lt of
		true ->
		    {node, Bit, {leaf, [E]}, Lf};
		false ->
		    {node, Bit, Lf, {leaf, [E]}}
	    end
    end;
insert(H, E, Bit, Lt, {node, CBit, Left, Right}) when Bit < CBit ->
    case inspect_bit(H, CBit) of
	left ->
	    {node, CBit, insert(H, E, Bit, Lt, Left), Right};
	right ->
	    {node, CBit, Left, insert(H, E, Bit, Lt, Right)}
    end;
insert(_H, E, Bit, Lt, {node, CBit, _Left, _Right} = N) when Bit > CBit ->
    case Lt of
	true ->
	    {node, Bit, {leaf, [E]}, N};
	false ->
	    {node, Bit, N, {leaf, [E]}}
    end.

is_element(Key, Tree) ->
    H = hash(Key),
    is_element(H, Key, Tree).

is_element(_, _, empty) -> false;
is_element(_H, Key, {leaf, Elems}) ->
    lists:member(Key, Elems);
is_element(H, Key, {node, Bit, L, R}) ->
    case inspect_bit(H, Bit) of
	left ->
	    is_element(H, Key, L);
	right ->
	    is_element(H, Key, R)
    end.






