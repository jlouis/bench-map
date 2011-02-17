-module(h_rb_set).

-export([from_list/1, is_element/2]).

from_list(L) ->
    lists:foldl(
      fun(K, T) ->
	      H = erlang:phash2(K),
	      rbdict:append(H, K, T)
      end,
      rbdict:new(),
      L).

is_element(Key, Tree) ->
    H = erlang:phash2(Key),
    Elems = rbdict:fetch(H, Tree),
    lists:member(Key, Elems).
