
-include_lib("include/eunit.hrl").


%% makes sure that a module is compiled
-define(assertCompiled(__Module__), fun(_Module__) -> 
  case code:ensure_loaded(_Module__) of
    {module, _Module__} -> ok;
    {error, _} -> ?assertMatch(ok, util:refresh(_Module__, [{source_dir, "test/fixtures/"}]))
  end
  end(__Module__)).


%% makes sure that a module is compiled without parse transform
-define(assertCompiledNoStub(__Module__), fun(_Module__) -> 
  ShouldCompile = case code:ensure_loaded(_Module__) of
    {module, _Module__} -> is_mocked(_Module__);
    {error, _} -> true
  end,
  if ShouldCompile ->
      ?assertMatch(ok, util:refresh(_Module__, [{source_dir, "test/fixtures/"}]));
    true -> ok
  end
  end(__Module__)).



apply_parse_transform_should_set_mocked_attribute_test() ->
  ?assertMatch(ok, util:refresh(mock_dummy, [{source_dir, "test/fixtures/"}, {parse_transform, eunit_mock}])),
  ?assertMatch({value,{mocked,[true]}}, lists:keysearch(mocked, 1, mock_dummy:module_info(attributes))).

standart_module_should_not_have_mocked_attribute_test() ->
  ?assertCompiledNoStub(mock_dummy),
  ?assertMatch(false, lists:keysearch(mocked, 1, mock_dummy:module_info(attributes))).

standart_module_should_return_normal_value_test() ->
  ?assertCompiledNoStub(mock_dummy),
  ?assertMatch(fun_with_arity_zero, mock_dummy:fun_with_arity_zero()).
  
recompile_with_mocking_parse_transform_test() ->  
  ?assertCompiledNoStub(mock_dummy),
  ?assertMatch(false, is_mocked(mock_dummy)),
  ?assertMatch(ok, recompile_for_mocking(mock_dummy)),
  ?assertMatch({value,{mocked,[true]}}, lists:keysearch(mocked, 1, mock_dummy:module_info(attributes))),
  ?assertMatch(true, is_mocked(mock_dummy)).
  
stubbed_module_should_be_recompiled_with_mock_parse_transform_test() ->  
  ?assertCompiledNoStub(mock_dummy),
  ?assertMatch(false, is_mocked(mock_dummy)),
  ?stub(_Fun = fun mock_dummy:fun_with_arity_zero/0, _Return = stubbed_value),
  ?assertMatch(true, is_mocked(mock_dummy)).
  
stubbed_module_should_return_fixed_stubbed_value_test() ->
  ?stub(_Fun = fun mock_dummy:fun_with_arity_zero/0, _Return = stubbed_value),
  ?assertMatch(stubbed_value, mock_dummy:fun_with_arity_zero()).

stubbed_module_should_return_stubbed_value_from_fun_test() ->
  ?stub(_Fun = fun mock_dummy:fun_with_arity_zero/0, _Return = fun() -> stubbed_value end),
  ?assertMatch(stubbed_value, mock_dummy:fun_with_arity_zero()).

fun_to_stub_record_test() ->
  ?assertMatch(#stub{ module_name = mock_dummy, fun_name = fun_with_arity_zero, arity = 0}, fun_to_stub_record(fun mock_dummy:fun_with_arity_zero/0)),
  ?assertMatch(#stub{ module_name = mock_dummy, fun_name = fun_with_arity_one, arity = 1}, fun_to_stub_record(fun mock_dummy:fun_with_arity_one/1)),
  ?assertMatch(#stub{ module_name = mock_dummy, fun_name = fun_with_arity_two, arity = 2}, fun_to_stub_record(fun mock_dummy:fun_with_arity_two/2)).

fun_to_stub_record_fail_test() ->
  LocalFun = fun() -> ok end,
  ?assertMatch({error, local_fun}, fun_to_stub_record(LocalFun)).
  
find_stub_test() ->
  Stubs = [Stub1 = #stub{ module_name = ModuleName1 = mock_dummy, fun_name = FunName1 = fun_with_arity_zero, arity = Arity1 = 0},  
           Stub2 = #stub{ module_name = ModuleName2 = mock_dummy, fun_name = FunName2 = fun_with_arity_one,  arity = Arity2 = 1},   
           Stub3 = #stub{ module_name = ModuleName3 = mock_dummy, fun_name = FunName3 = fun_with_arity_two,  arity = Arity3 = 2}],  
  NotStubbedFunName = fun_with_arity_xxx,
  NotStubbedModuleName = not_stubbed_module,
  NotStubbedArity = 5,
  ?assertMatch({value, Stub1}, find_stub(ModuleName1, FunName1, Arity1, Stubs)),
  ?assertMatch({value, Stub2}, find_stub(ModuleName2, FunName2, Arity2, Stubs)),
  ?assertMatch({value, Stub3}, find_stub(ModuleName3, FunName3, Arity3, Stubs)),
  ?assertMatch(false, find_stub(ModuleName1, NotStubbedFunName, Arity1, Stubs)),
  ?assertMatch(false, find_stub(NotStubbedModuleName, FunName2, Arity2, Stubs)),
  ?assertMatch(false, find_stub(ModuleName1, FunName1, NotStubbedArity, Stubs)).

set_stub_test() ->
  Stubs = [Stub1 = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_zero, arity = 0, returns = 0},  
           Stub2 = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_one,  arity = 1, returns = fun(_Arg) -> 1 end},   
           Stub3 = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_two,  arity = 2, returns = 2}],  
    
  Stub1Changed = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_zero, arity = 0, returns = a},  
  Stub2Changed = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_one,  arity = 1, returns = fun(_Arg) -> b end},   
  Stub3Changed = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_two,  arity = 2, returns = c},  

  NewStub1 = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_zero, arity = 1, returns = "A"},  % same fun but different arity
  NewStub2 = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_xxx,  arity = 1, returns = fun(_Arg) -> "B" end}, % new fun but same module
  NewStub3 = #stub{ module_name = mock_dummy_x, fun_name = fun_with_arity_two,  arity = 2, returns = "C"}, % same fun, but different module
      
  ?assertMatch([Stub1Changed, Stub2, Stub3], set_stub(Stub1Changed, Stubs)),
  ?assertMatch([Stub1, Stub2Changed, Stub3], set_stub(Stub2Changed, Stubs)),
  ?assertMatch([Stub1, Stub2, Stub3Changed], set_stub(Stub3Changed, Stubs)),
  ?assertMatch([Stub1Changed, Stub2Changed, Stub3Changed], set_stub(Stub3Changed, set_stub(Stub2Changed, set_stub(Stub1Changed, Stubs)))),

  ?assertMatch([NewStub1], set_stub(NewStub1, _Stubs = [])),
  ?assertMatch([NewStub1, Stub1, Stub2, Stub3], set_stub(NewStub1, Stubs)),
  ?assertMatch([NewStub2, Stub1, Stub2, Stub3], set_stub(NewStub2, Stubs)),
  ?assertMatch([NewStub3, Stub1, Stub2, Stub3], set_stub(NewStub3, Stubs)),
  ?assertMatch([NewStub3, NewStub2, NewStub1, Stub1, Stub2, Stub3], set_stub(NewStub3, set_stub(NewStub2, set_stub(NewStub1, Stubs)))).


update_assertion_test() ->
  erlang:throw("NOT IMPLEMENTED").
  % Assertions = [Assertion1 = #assert_call {required_call_count = ignore,  current_call_count = 0, expected = ignore, location = {my_test, 1}},  
  %               Assertion2 = #assert_call {required_call_count = 2,  current_call_count = 1, expected = {args, [a,b]}, location = {my_test, 2}},   
  %               Assertion3 = #assert_call {required_call_count = 1,  current_call_count = 0, expected = {return, foo}, location = {my_test, 3}},                
  %               Assertion4 = #assert_call {required_call_count = ignore,  current_call_count = 0, expected = fun(_A) -> ok end, location = {my_test, 4}}],  
  %   
  % Assertion1Changed = #assert_call {required_call_count = ignore,  current_call_count = 0, expected = ignore, location = {my_test, 1}},  
  % Assertion2Changed = #assert_call {required_call_count = ignore,  current_call_count = 0, expected = ignore, location = {my_test, 1}},   
  % Assertion3Changed = #assert_call {required_call_count = ignore,  current_call_count = 0, expected = ignore, location = {my_test, 1}},  
  % 
  % NewAssertion1 = #assert_call {required_call_count = ignore,  current_call_count = 0, expected = ignore, location = {my_test, 1}},  % same fun but different arity
  % NewAssertion2 = #assert_call {required_call_count = ignore,  current_call_count = 0, expected = ignore, location = {my_test, 1}}, % new fun but same module
  % NewAssertion3 = #assert_call {required_call_count = ignore,  current_call_count = 0, expected = ignore, location = {my_test, 1}}, % same fun, but different module
  %     
  % ?assertMatch([Assertion1Changed, Assertion2, Assertion3], set_assertion(Assertion1Changed, Assertions)),
  % ?assertMatch([Assertion1, Assertion2Changed, Assertion3], set_assertion(Assertion2Changed, Assertions)),
  % ?assertMatch([Assertion1, Assertion2, Assertion3Changed], set_assertion(Assertion3Changed, Assertions)),
  % ?assertMatch([Assertion1Changed, Assertion2Changed, Assertion3Changed], set_assertion(Assertion3Changed, set_assertion(Assertion2Changed, set_assertion(Assertion1Changed, Assertions)))),
  % 
  % ?assertMatch([NewAssertion1], set_assertion(NewAssertion1, _Assertions = [])),
  % ?assertMatch([NewAssertion1, Assertion1, Assertion2, Assertion3], set_assertion(NewAssertion1, Assertions)),
  % ?assertMatch([NewAssertion2, Assertion1, Assertion2, Assertion3], set_assertion(NewAssertion2, Assertions)),
  % ?assertMatch([NewAssertion3, Assertion1, Assertion2, Assertion3], set_assertion(NewAssertion3, Assertions)),
  % ?assertMatch([NewAssertion3, NewAssertion2, NewAssertion1, Assertion1, Assertion2, Assertion3], set_assertion(NewAssertion3, set_assertion(NewAssertion2, set_assertion(NewAssertion1, Assertions)))).


check_arity_test() ->
  ?assertMatch(ok, check_arity(fun mock_dummy:fun_with_arity_zero/0, fun() -> result end)),
  ?assertMatch(ok, check_arity(fun mock_dummy:fun_with_arity_one/1, fun(_) -> result end)),
  ?assertMatch(ok, check_arity(fun mock_dummy:fun_with_arity_two/2, fun(_,_) -> result end)),
  ?assertMatch(ok, check_arity(fun mock_dummy:fun_with_arity_zero/0, result)),
  ?assertMatch(ok, check_arity(fun mock_dummy:fun_with_arity_one/1, result)),
  ?assertMatch(ok, check_arity(fun mock_dummy:fun_with_arity_two/2, result)),

  ?assertMatch({error, arity_mismatch}, check_arity(fun mock_dummy:fun_with_arity_zero/0, fun(_,_) -> result end)),
  ?assertMatch({error, arity_mismatch}, check_arity(fun mock_dummy:fun_with_arity_one/1, fun() -> result end)),
  ?assertMatch({error, arity_mismatch}, check_arity(fun mock_dummy:fun_with_arity_two/2, fun(_) -> result end)).
 
eunit_test_should_fail_test() -> 
  TestFun = fun() -> ?assertMatch(true, false) end,
  ?assertMatch(error, eunit:test(TestFun)).

eunit_test_should_succeed_test() -> 
  TestFun = fun() -> ?assertMatch(true, true) end,
  ?assertMatch(ok, eunit:test(TestFun)).

mocked_module_should_be_recompiled_with_mock_parse_transform_test() ->  
  ?assertCompiledNoStub(mock_dummy),
  ?assertMatch(false, is_mocked(mock_dummy)),
  ?assertCalled(_Fun = fun mock_dummy:fun_with_arity_zero/0, _Times = 1, ignore),
  ?assertMatch(true, is_mocked(mock_dummy)).
  
assert_called_should_succeed_for_unstubbed_fun_and_ignored_arguments_test() -> 
  ?assertCompiled(mock_dummy),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 1, ignore),
    mock_dummy:fun_with_arity_one(foo)
  end,
  ?assertMatch(ok, eunit:test(TestFun)).

foo_test() ->
  ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 1, ignore).

bar_test() ->
  ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 1, _Args = {args, []}).

assert_called_should_fail_for_unstubbed_fun_and_ignored_arguments_not_called_once_test() -> 
  ?assertCompiled(mock_dummy),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 1, ignore)
  end,
  ?assertMatch(error, eunit:test(TestFun)).

assert_called_should_fail_for_unstubbed_fun_and_ignored_arguments_not_called_twice_test() -> 
  ?assertCompiled(mock_dummy),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 2, ignore),
    mock_dummy:fun_with_arity_one(foo)
  end,
  ?assertMatch(error, eunit:test(TestFun)).

assert_called_should_succeed_for_unstubbed_fun_and_expected_arguments_test() -> 
  ?assertCompiled(mock_dummy),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 1, _Arguments = {args, [foo]}),
    mock_dummy:fun_with_arity_one(foo)
  end,
  ?assertMatch(ok, eunit:test(TestFun)).

assert_called_should_succeed_for_stubbed_fun_and_expected_arguments_test() -> 
  ?assertCompiled(mock_dummy),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 1, _Stub = fun(Arg) -> Arg + 1000 end),
    ?assertMatch(1111, mock_dummy:fun_with_arity_one(111))
  end,
  ?assertMatch(ok, eunit:test(TestFun)).


assert_called_should_fail_for_unstubbed_fun_and_expected_arguments_not_called_once_test() -> 
  ?assertCompiled(mock_dummy),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 1, _Arguments = {args, [foo]}),
    mock_dummy:fun_with_arity_one(foo)
  end,
  ?assertMatch(error, eunit:test(TestFun)).

assert_called_should_fail_for_unstubbed_fun_and_expected_arguments_not_called_twice_test() -> 
  ?assertCompiled(mock_dummy),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 2, _Arguments ={args, [foo]}),
    mock_dummy:fun_with_arity_one(foo)
  end,
  ?assertMatch(error, eunit:test(TestFun)).

assert_called_should_fail_for_unstubbed_fun_and_expected_arguments_no_matching_arguments_test() -> 
  ?assertCompiled(mock_dummy),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1, _Times = 1, _Arguments = {args, [foo]}),
    mock_dummy:fun_with_arity_one(bar)
  end,
  ?assertMatch(error, eunit:test(TestFun)).

  
%%----------------------------------------------------
%% helpers
%%----------------------------------------------------

  
% replace_stub_test() ->
%   Stubs.


%% new mock syntax
% mock_test() ->
%   Pid = ?mock(),
%   ?assertCalled(Pid, _Event = {bar}, _Times = 3, _Return = ok),
%   ?assertMatch(ok, gen_server:call(Pid, {bar})),
%   ?assertMatch(ok, gen_server:call(Pid, {bar})),
%   ?assertMatch(ok, gen_server:call(Pid, {bar})),
%   ?assertCalled(Pid, _Times1 = twice, _Return1 = fun(_Event1 = foo, _Data) -> {reply, ok, _Data} end),
%   ?assertMatch(ok, gen_server:call(Pid, foo)),
%   ?assertMatch(ok, gen_server:call(Pid, foo)),
%   ?assertCasted(Pid, _Times2 = 3, _Event2 = {bar}),
%   gen_server:cast(Pid, {bar}),
%   ?assertCalled(emock:unmocked_fun/2, _Times5 = once),
%   emock:unmocked_fun(foo, bar),
%   ?assertCalled(_Module = emock, _Fun = unmocked_fun, _Arity = 2, _Times3 = once, _Return2 = true),
%   ?assertMatch(true, emock:unmocked_fun(x,y)),
%   ?assertCalled(_Module = emock, _Fun = unmocked_fun, _Arity = 1, _Times4 = once, _Return3 = fun(Arg) -> Arg end),
%   ?assertMatch(1, emock:unmocked_fun(1)),
%   ?stub(_Module = emock, _Fun = unmocked_fun, _Arity = 0, _Return4 = foo),
%   ?assertMatch(foo, emock:unmocked_fun()),
%   ?stub(_Fun = fun emock:unmocked_fun/0, _Return5 = false),
%   ?assertMatch(false, emock:unmocked_fun()),
%   ?stub(_Fun = fun emock:unmocked_fun/1, _Return6 = fun(Arg) -> ok end),
%   ?assertMatch(ok, emock:unmocked_fun(x)).

  
dummy1_test() ->
  ok.