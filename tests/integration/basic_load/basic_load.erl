-module(basic_load).

-export([run_tests/0]).

-nifs([odin_proc/2]).

-on_load(init/0).

init() ->
  ok = erlang:load_nif("./basic_load_nif", 0).

odin_proc(_X, _Y) ->
  exit(nif_library_not_loaded).

run_tests() ->
  Result = odin_proc(2, 3),
  io:format("Result: ~p~n", [Result]),
  ok.