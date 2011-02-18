-module(bench_map).

-export([run/0, run_prof/0, runs/1, list_shuffle/1]).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

run_prof() ->
    eprof:start(),
    eprof:start_profiling([self()]),
    run(),
    eprof:stop_profiling(),
    eprof:log("procs.profile"),
    eprof:analyze(procs),
    eprof:log("total.profile"),
    eprof:analyze(total),
    eprof:stop().

run() ->
    [
     {hamt, hamt_test()},
     {patricia, patricia_test()},
     {sets, set_test()},
     {dict, dict_test()},
     {gb_sets, gb_sets_test()},
     {gb_trees, gb_trees_test()},
     {h_rb_sets, h_rb_set_test()},
     {rb_sets, rb_sets_test()}
    ].

runs(F) ->
    [F() || _ <- lists:seq(1, 10)].

words() ->
    Words = "/usr/share/dict/words",
    {ok, Content} = file:read_file(Words),
    [binary_to_list(W) || W <- binary:split(Content, <<"\n">>, [global])].



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

test_h_rb_set_words(Words, Set) ->
    lists:foreach(
      fun(Word) ->
	      true = h_rb_set:is_element(Word, Set)
      end,
      Words).

test_rb_sets_words(Words, Set) ->
    lists:foreach(
      fun(Word) ->
	      true = rbsets:is_element(Word, Set)
      end,
      Words).

test_gb_sets_words(Words, Set) ->
    lists:foreach(
      fun(Word) ->
	      true = gb_sets:is_element(Word, Set)
      end,
      Words).

test_patricia_words(Words, Tree) ->
    lists:foreach(
      fun(Word) ->
	      true = patricia:is_element(Word, Tree)
      end,
      Words),
    false = patricia:is_element(notthere, Tree).

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
    timer:tc(fun () ->
		     TestFun(Ws, S),
		     TestFun(lists:reverse(Ws), S),
		     TestFun(list_shuffle(Ws), S),
		     ok
	     end, []).

h_rb_set_test() ->
    test_map(fun(Ws) ->
		     h_rb_set:from_list(Ws)
	     end,
	     fun test_h_rb_set_words/2).

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

rb_sets_test() ->
    test_map(fun(Ws) ->
		     rbsets:from_list(Ws)
	     end,
	     fun test_rb_sets_words/2).

patricia_test() ->
    test_map(fun(Ws) ->
		     patricia:from_list(Ws)
	     end,
	     fun test_patricia_words/2).

test_hamt_words(Words, Tree) ->
    lists:foreach(
      fun(Word) ->
	      true = hamt:is_element(Word, Tree)
      end, Words).

hamt_test() ->
    test_map(fun hamt:from_list/1,
	     fun test_hamt_words/2).

-ifdef(EUNIT).
-ifdef(EQC).

prop_ins_fetch() ->
    ?FORALL(L, non_empty(list(int())),
	    begin
		H = hamt:from_list(L),
		true = lists:all(
		  fun(X) -> X =:= true end,
		  [hamt:is_element(E, H) || E <- L]),
		true = lists:all(
		  fun(X) -> X =:= true end,
		  [hamt:is_element(E, H) || E <- lists:reverse(L)]),
		true = lists:all(
		  fun(X) -> X =:= true end,
		  [hamt:is_element(E, H) || E <- list_shuffle(L)])
	    end).

hamt_ins_fetch_test() ->
    ?assert(eqc:quickcheck(eqc:numtests(10000, prop_ins_fetch()))).

-endif.
-endif.















