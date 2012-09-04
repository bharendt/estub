-module(test_module).
-compile(nowarn_unused_function).

-ifdef(TEST).
-compile([{parse_transform, estub}]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("estub/include/estub.hrl").


assert_called_should_stub_local_function_test() ->
  ?assertMatch(local_function, local_function()),
  ?assert(is_function(fun local_function/0)),
  ?assertMatch(ok, ?assertCalled(fun test_module:local_function/0 ?once ?andReturn(stubbed_local_fun))),
  ?assertMatch(stubbed_local_fun, local_function()).


assert_called_should_succeed_for_local_function_called_onces_test() ->
  ?assert(is_function(fun local_function/0)),
  ?assertCalled(fun test_module:local_function/0 ?once),
  ?assertMatch(local_function, local_function()).
  
-endif.


local_function() ->
  local_function.


