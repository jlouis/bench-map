-module(hamt).

-export([from_list/1, is_element/2]).

-type hamt(A) :: {full, tuple()} | {partial, integer(), orddict:orddict()}
	       | {leaf, integer(), [A]} | empty.

-define(SZ, 4).
-define(SZ_Mask, 31).
-define(FULL_THRESH, 16).

hash(E) ->
    erlang:phash2(E).

from_list(L) ->
    lists:foldl(fun insert/2, new(), L).

-spec new() -> empty.
new() ->
    empty.

insert(E, T) ->
    H = hash(E),
    insert(32, E, H, T).

insert(_Bit, E, H, empty) ->
    {leaf, H, [E]};
insert(_Bit, E, H, {leaf, H, Es}) ->
    {leaf, H, [E | Es]};
insert(Bit, E, H, {leaf, H1, Es}) ->
    insert(Bit, E, H, mk_partial(Bit, H1, Es));
insert(Bit, E, H, {partial, Sz, D}) ->
    N = search_mask(Bit, H),
    case orddict:find(N, D) of
	{ok, Hamt} ->
	    {partial, Sz, orddict:store(N, insert(Bit - ?SZ, E, H,
						  Hamt), D)};
	error ->
	    if
		Sz+1 >= ?FULL_THRESH ->
		    Tuple = mk_tuple(D),
		    insert(Bit, E, H, {full, Tuple});
		true ->
		    insert(Bit, E, H, {partial, Sz+1, orddict:store(N, empty, D)})
	    end
    end;
insert(Bit, E, H, {full, Tuple}) ->
    N = search_mask(Bit, H),
    {full, setelement(N, Tuple, insert(Bit - ?SZ, E, H, element(N, Tuple)))}.




is_element(E, T) ->
    H = hash(E),
    is_element(32, E, H, T).

is_element(_Bit, _E, _H, empty) ->
    false;
is_element(_Bit, E, H, {leaf, H, Elems}) ->
    lists:member(E, Elems);
is_element(_Bit, _E, _H, {leaf, _, _}) ->
    false;
is_element(Bit, E, H, {partial, _, D}) ->
    N = search_mask(Bit, H),
    case orddict:find(N, D) of
	error ->
	    false;
	{ok, V} ->
	    is_element(Bit - ?SZ, E, H, V)
    end;
is_element(Bit, E, H, {full, Tuple}) ->
    N = search_mask(Bit, H),
    is_element(Bit - ?SZ, E, H, element(N, Tuple)).


mk_partial(Bit, Hash, Val) ->
    N = search_mask(Bit, Hash),
    {partial, 1, orddict:store(N, {leaf, Hash, [Val]}, orddict:new())}.

mk_tuple(D) ->
    list_to_tuple(mk_tuple(0, orddict:to_list(D))).

mk_tuple(32, _) -> [];
mk_tuple(N, [{N, V} | Rest]) ->
    [V | mk_tuple(N+1, Rest)];
mk_tuple(N, L) ->
    [empty | mk_tuple(N+1, L)].

search_mask(Bit, H) ->
    Mask = ?SZ_Mask bsl (Bit - ?SZ),
    (Mask band H) bsr (Bit - ?SZ).













