
-include_lib("include/eunit.hrl").


assert_called_should_stub_local_function_test() ->
  ?assertMatch(local_function, local_function()),
  ?assert(is_function(fun local_function/0)),
  ok = ?assertCalled(fun test_module:local_function/0 ?once ?andReturn(stubbed_local_fun)),
  ?assertMatch(stubbed_local_fun, local_function()).
