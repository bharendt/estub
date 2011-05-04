-module(test_module).
-compile(nowarn_unused_function).

-ifdef(TEST).
  -include("test/test_module_test.erl").
-endif.


local_function() ->
  local_function.


