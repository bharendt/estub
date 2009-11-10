-module(test_module).

-ifdef(TEST).
  -include("test/test_module_test.erl").
-endif.


local_function() ->
  local_function.


