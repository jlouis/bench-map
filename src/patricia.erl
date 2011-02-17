-module(patricia).
%% @todo Go over all the ordering values and check them.

-export([insert/2, is_element/2, from_list/1, new/0]).

-define(BIT_MAX, 27). % Set by virtue of erlang:phash2
-type ptree(A) :: empty | {leaf, [A]} | {node, pos_integer(), ptree(A), ptree(A)}.

-spec insert(A, ptree(A)) -> ptree(A) | already.

hash(X) ->
    erlang:phash2(X).

from_list(L) ->
    lists:foldl(
      fun insert/2,
      new(),
      L).

new() ->
    empty.

insert(E, empty) ->
    {leaf, [E]};
insert(E, Tree) ->
    H = hash(E),
    {Bit, Lt} = find_bit(H, Tree),
    insert(H, E, Bit, Lt, Tree).

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

cmp_lt_bit(I1, I2, N) ->
    Bit = (1 bsr N),
    (Bit band I1) < (Bit band I2).

crit_bit(I1, I2, N) ->
    Bit = (1 bsr N),
    case (Bit band I1) bxor (Bit band I2) of
	0 ->
	    crit_bit(I1, I2, N-1);
	_ ->
	    {N, cmp_lt_bit(I1, I2, N)}
    end.

inspect_bit(H, Bit) ->
    case H band (1 bsr Bit) of
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
	    {node, CBit, insert(H, E, Lt, Bit, Left), Right};
	right ->
	    {node, CBit, Left, insert(H, E, Lt, Bit, Right)}
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
    is_element1(H, Key, 0, Tree).

is_element1(_, _, _, empty) -> false;
is_element1(_H, Key, _Cnt, {leaf, Elems}) ->
    lists:member(Key, Elems);
is_element1(H, Key, Cnt, {node, Bit, L, R}) ->
    case H band (1 bsr (Cnt + Bit)) of
	0 -> is_element1(H, Key, Cnt+Bit, L);
	1 -> is_element1(H, Key, Cnt+Bit, R)
    end.










