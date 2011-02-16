-module(bench_map).

-export([foo/0]).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

foo() ->
    foo.


-ifdef(EUNIT).
-ifdef(EQC).

words() ->
    Words = "/usr/share/dict/words",
    {ok, Content} = file:read_file(Words),
    binary:split(Content, <<"\n">>, [global]).

list_shuffle(L) ->
    random:seed(), %% Reset Random function
    Laced = [{K, random:uniform()} || K <- L],
    Sorted = lists:keysort(2, Laced),
    [W || {W, _} <- Sorted].

test_sets_words(Words, Set) ->
    lists:foreach(
      fun(Word) ->
	      true = sets:is_element(Word, Set)
      end,
      Words).

test_gb_sets_words(Words, Set) ->
    lists:foreach(
      fun(Word) ->
	      true = gb_sets:is_element(Word, Set)
      end,
      Words).

test_dict_words(Words, Dict) ->
    lists:foreach(
      fun(W) ->
	      true = dict:fetch(W, Dict)
      end,
      Words).

test_gb_trees_words(Words, Tree) ->
    lists:foreach(
      fun(W) ->
	      true = gb_trees:is_defined(W, Tree)
      end,
      Words).

test_map(Generator, TestFun) ->
    Ws = words(),
    S = Generator(Ws),
    TestFun(Ws, S),
    TestFun(lists:reverse(Ws), S),
    TestFun(list_shuffle(Ws), S).

set_test() ->
    test_map(fun(Ws) ->
		     sets:from_list(Ws)
	     end,
	     fun test_sets_words/2).

dict_test() ->
    test_map(fun(Ws) ->
		     dict:from_list([{K, true} || K <- Ws])
	     end,
	     fun test_dict_words/2).

gb_sets_test() ->
    test_map(fun(Ws) ->
		     gb_sets:from_list(Ws)
	     end,
	     fun test_gb_sets_words/2).

gb_trees_test() ->
    test_map(fun(Ws) ->
		     lists:foldl(
		       fun(K, Tree) ->
			       gb_trees:enter(K, true, Tree)
		       end,
		       gb_trees:empty(),
		       Ws)
	     end,
	     fun test_gb_trees_words/2).

-endif.
-endif.















