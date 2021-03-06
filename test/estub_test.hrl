
-include_lib("eunit/include/eunit.hrl").
-include_lib("estub/include/estub.hrl").


%%----------------------------------------------------
%% tests for mocked functions
%%----------------------------------------------------

apply_parse_transform_should_set_mocked_attribute_test() ->
  ?assertMatch({value,{mocked,[true]}}, lists:keysearch(mocked, 1, mock_dummy:module_info(attributes))).

estub_mocked_gen_server_should_appear_as_mocked_module_test() ->  
  ?assertMatch(true, is_mocked(estub_mocked_gen_server)).

estub_mocked_gen_fsm_should_appear_as_mocked_module_test() ->  
  ?assertMatch(true, is_mocked(estub_mocked_gen_fsm)).
  
stubbed_module_should_be_recompiled_with_mock_parse_transform_test() ->  
  ?stub(_Fun = fun mock_dummy:fun_with_arity_zero/0, _Return = stubbed_value),
  ?assertMatch(true, is_mocked(mock_dummy)).
  
stubbed_module_should_return_fixed_stubbed_value_test() ->
  ?stub(_Fun = fun mock_dummy:fun_with_arity_zero/0, _Return = stubbed_value),
  ?assertMatch(stubbed_value, mock_dummy:fun_with_arity_zero()).

stubbed_module_should_return_stubbed_value_from_fun_test() ->
  ?stub(_Fun = fun mock_dummy:fun_with_arity_zero/0, _Return = fun() -> stubbed_value end),
  ?assertMatch(stubbed_value, mock_dummy:fun_with_arity_zero()).

standart_module_with_different_clauses_should_return_normal_value__test() ->
  ?assertMatch(fun_with_differenct_clauses_1, mock_dummy:fun_with_differenct_clauses(1)),
  ?assertMatch(fun_with_differenct_clauses_2, mock_dummy:fun_with_differenct_clauses(2)),
  ?assertMatch(fun_with_differenct_clauses_3, mock_dummy:fun_with_differenct_clauses(3)).  

stubbed_module_with_different_clauses_should_return_stubbed_value_from_fun_matching_argument_1_test() ->
  ?stub(_Fun = fun mock_dummy:fun_with_differenct_clauses/1, _Return = fun(1) -> stubbed_value_1 end),
  ?assertMatch(stubbed_value_1, mock_dummy:fun_with_differenct_clauses(1)),
  ?assertMatch(fun_with_differenct_clauses_2, mock_dummy:fun_with_differenct_clauses(2)),
  ?assertMatch(fun_with_differenct_clauses_3, mock_dummy:fun_with_differenct_clauses(3)).


stubbed_module_with_different_clauses_should_return_stubbed_value_from_fun_matching_argument_2_test() ->
  ?stub(_Fun = fun mock_dummy:fun_with_differenct_clauses/1, _Return = fun(2) -> stubbed_value_2 end),
  ?assertMatch(fun_with_differenct_clauses_1, mock_dummy:fun_with_differenct_clauses(1)),
  ?assertMatch(stubbed_value_2, mock_dummy:fun_with_differenct_clauses(2)),
  ?assertMatch(fun_with_differenct_clauses_3, mock_dummy:fun_with_differenct_clauses(3)).

  
stubbed_module_with_different_clauses_should_return_stubbed_value_from_fun_test() ->
  ?stub(_Fun = fun mock_dummy:fun_with_differenct_clauses/1, _Return = fun(_) -> stubbed_value end),
  ?assertMatch(stubbed_value, mock_dummy:fun_with_differenct_clauses(1)),
  ?assertMatch(stubbed_value, mock_dummy:fun_with_differenct_clauses(2)),
  ?assertMatch(stubbed_value, mock_dummy:fun_with_differenct_clauses(3)).


fun_to_stub_record_test() ->
  ?assertMatch(#stub{ module_name = mock_dummy, fun_name = fun_with_arity_zero, arity = 0}, fun_to_stub_record(fun mock_dummy:fun_with_arity_zero/0)),
  ?assertMatch(#stub{ module_name = mock_dummy, fun_name = fun_with_arity_one, arity = 1}, fun_to_stub_record(fun mock_dummy:fun_with_arity_one/1)),
  ?assertMatch(#stub{ module_name = mock_dummy, fun_name = fun_with_arity_two, arity = 2}, fun_to_stub_record(fun mock_dummy:fun_with_arity_two/2)).

fun_to_stub_record_fail_test() ->
  LocalFun = fun() -> ok end,
  ?assertMatch({error, local_fun}, fun_to_stub_record(LocalFun)).
  
get_expected_state_all_test() ->
  Options = [mock ?once ?inAnyState],  
  ?assertMatch({inState, all}, get_expected_state(Options)).

get_expected_state_idle_test() ->
  Options = [mock ?once ?inState(idle) ?with(foo)],  
  ?assertMatch({inState, idle}, get_expected_state(Options)).

get_expected_state_no_state_test() ->
  Options = [mock ?once ?with(foo)],  
  ?assertMatch(false, get_expected_state(Options)).

  
find_stub_test() ->
  Stubs = [Stub1 = #stub{ module_name = ModuleName1 = mock_dummy, fun_name = FunName1 = fun_with_arity_zero, arity = Arity1 = 0},  
           Stub2 = #stub{ module_name = ModuleName2 = mock_dummy, fun_name = FunName2 = fun_with_arity_one,  arity = Arity2 = 1},   
           Stub3 = #stub{ module_name = ModuleName3 = mock_dummy, fun_name = FunName3 = fun_with_arity_two,  arity = Arity3 = 2}],  
  NotStubbedFunName = fun_with_arity_xxx,
  NotStubbedModuleName = not_stubbed_module,
  NotStubbedArity = 5,
  ?assertMatch({value, Stub1}, find_stub(ModuleName1, FunName1, Arity1, ignore, Stubs)),
  ?assertMatch({value, Stub2}, find_stub(ModuleName2, FunName2, Arity2, ignore, Stubs)),
  ?assertMatch({value, Stub3}, find_stub(ModuleName3, FunName3, Arity3, ignore, Stubs)),
  ?assertMatch(false, find_stub(ModuleName1, NotStubbedFunName, Arity1, ignore, Stubs)),
  ?assertMatch(false, find_stub(NotStubbedModuleName, FunName2, Arity2, ignore, Stubs)),
  ?assertMatch(false, find_stub(ModuleName1, FunName1, NotStubbedArity, ignore, Stubs)).

find_stub_with_pid_test() ->
  Stubs = [ Stub1 = #stub{ module_name = ModuleName1 = mock_dummy, fun_name = FunName1 = fun_with_arity_zero,  arity = Arity1 = 0, pid = Pid1 = list_to_pid("<0.1.0>")},  
            Stub2 = #stub{ module_name = ModuleName2 = mock_dummy, fun_name = FunName2 = fun_with_arity_zero,  arity = Arity2 = 0, pid = Pid2 = list_to_pid("<0.2.0>")},   
            Stub3 = #stub{ module_name = ModuleName3 = mock_dummy, fun_name = FunName3 = fun_with_arity_zero,  arity = Arity3 = 0, pid = Pid3 = none}],  
  NotExistingPid = list_to_pid("<0.10.0>"),
  ?assertMatch({value, Stub1}, find_stub(ModuleName1, FunName1, Arity1, Pid1, Stubs)),
  ?assertMatch({value, Stub1}, find_stub(ModuleName1, FunName1, Arity1, ignore, Stubs)),
  ?assertMatch({value, Stub2}, find_stub(ModuleName2, FunName2, Arity2, Pid2, Stubs)),
  ?assertMatch({value, Stub3}, find_stub(ModuleName3, FunName3, Arity3, Pid3, Stubs)),
  ?assertMatch(false, find_stub(ModuleName1, FunName1, Arity1, NotExistingPid, Stubs)).
  


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

get_assertions_and_reset_test() ->
  _Assertions = [Assertion1 = #assert_call {required_call_count = at_least_once,  current_call_count = 0, location = {my_test, 1}},  
                 Assertion2 = #assert_call {required_call_count = 2,  current_call_count = 1, location = {my_test, 2}},   
                 Assertion3 = #assert_call {required_call_count = 1,  current_call_count = 0, location = {my_test, 3}}],  
  Stubs = [ Stub1 = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_zero, arity = 0, returns = 0, assertions = [Assertion1, Assertion2]},  
           _Stub2 = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_one,  arity = 1, returns = fun(_Arg) -> 1 end, assertions = []},   
            Stub3 = #stub{ module_name = mock_dummy, fun_name = fun_with_arity_two,  arity = 2, returns = 2, assertions = [Assertion3]}],  
  ?assertMatch({reply, _StubsWithAssertions = [Stub1, Stub3], #state {stubs = []}}, handle_call({get_assertions_and_reset, global}, self, #state{stubs = Stubs})). 

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

mocked_module_should_return_stubbed_value_test() ->  
  ?assertMatch(stubbed_value, mock_dummy:fun_with_arity_zero()),
  ?assertMatch(true, is_mocked(mock_dummy)).
  
get_mock_info_for_gen_server_test() ->
  StartResult = gen_server:start({local, dummy}, estub_mocked_gen_server, [dummy], []),
  ?assertMatch({ok, _}, StartResult),
  {ok, Pid} = StartResult,
  ?assertMatch({estub_mocked_gen_server, gen_server, true}, get_mock_info(Pid)),
  ?assertMatch(ok, estub_mocked_gen_server:stop(Pid)),
  ?assertMatch(undefined, whereis(dummy)).

get_mock_info_for_gen_fsm_test() ->
  StartResult = gen_fsm:start({local, dummy}, estub_mocked_gen_fsm, [dummy], []),
  ?assertMatch({ok, _}, StartResult),
  {ok, Pid} = StartResult,
  ?assertMatch({estub_mocked_gen_fsm, gen_fsm, true}, get_mock_info(Pid)),
  ?assertMatch(ok, estub_mocked_gen_fsm:stop(Pid)),
  ?assertMatch(undefined, whereis(dummy)).
  
assert_called_should_fail_for_unstubbed_fun_and_ignored_arguments_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy_no_parse_transform:fun_with_arity_one/1 ?once),
    mock_dummy_no_parse_transform:fun_with_arity_one(foo)
  end,
  ?assertError({missing_parse_transform, _}, TestFun()),
  ?assertMatch(error, eunit:test(TestFun)),  
  ?assertMatch(ok, check_assertions()). % no assertions to check

assert_called_should_succeed_for_stubbed_fun_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?once),
    mock_dummy:fun_with_arity_one(foo)
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).

assert_called_once_test() ->
  clean_assertions(),
  ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?once),
  ?assertMatch(fun_with_arity_one, mock_dummy:fun_with_arity_one(1)),
  ?assertMatch(ok, check_assertions()).

assert_called_one_times_test() ->
  clean_assertions(),
  ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?once ?with([1])),
  ?assertMatch(fun_with_arity_one, mock_dummy:fun_with_arity_one(1)),
  ?assertMatch(ok, check_assertions()).

assert_called_one_times_arguments_not_as_array_test() ->
  clean_assertions(),
  ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?once ?with(1)),
  ?assertMatch(fun_with_arity_one, mock_dummy:fun_with_arity_one(1)),
  ?assertMatch(ok, check_assertions()).

assert_called_one_times_with_two_arguments_test() ->
  clean_assertions(),
  ?assertCalled(fun mock_dummy:fun_with_arity_two/2 ?once ?with([1,2])),
  ?assertMatch(fun_with_arity_two, mock_dummy:fun_with_arity_two(1,2)),
  ?assertMatch(ok, check_assertions()).

assert_called_any_times_once_test() ->
  clean_assertions(),
  ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?atLeastOnce ?with([1])),
  ?assertMatch(fun_with_arity_one, mock_dummy:fun_with_arity_one(1)),
  ?assertMatch(ok, check_assertions()).

assert_called_any_times_more_calls_test() ->
  clean_assertions(),
  ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?atLeastOnce ?with([1])),
  ?assertMatch(fun_with_arity_one, mock_dummy:fun_with_arity_one(1)),
  ?assertMatch(fun_with_arity_one, mock_dummy:fun_with_arity_one(2)),
  ?assertMatch(fun_with_arity_one, mock_dummy:fun_with_arity_one(1)),
  ?assertMatch(ok, check_assertions()).

assert_called_should_fail_for_unstubbed_fun_and_ignored_arguments_not_called_once_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    % throws {missing_parse_transform, _}
    ?assertCalled(fun mock_dummy_no_parse_transform:fun_with_arity_one/1 ?once)
  end,
  ?assertError({missing_parse_transform, _}, TestFun()),
  ?assertMatch(error, eunit:test(TestFun)),    
  ?assertMatch(ok, check_assertions()). % no assertions to check

assert_called_should_fail_for_unstubbed_fun_and_ignored_arguments_not_called_twice_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    % throws {missing_parse_transform, _}
    ?assertCalled(fun mock_dummy_no_parse_transform:fun_with_arity_one/1 ?twice),
    mock_dummy_no_parse_transform:fun_with_arity_one(foo)
  end,
  ?assertError({missing_parse_transform, _}, TestFun()),
  ?assertMatch(error, eunit:test(TestFun)),  
  ?assertMatch(ok, check_assertions()). % no assertions to check

assert_called_should_succeed_for_unstubbed_fun_and_expected_arguments_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?once ?with([foo])),
    mock_dummy:fun_with_arity_one(foo)
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).

assert_called_should_succeed_for_stubbed_fun_and_expected_arguments_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?once ?andReturn(fun(Arg) -> Arg + 1000 end)),
    ?assertMatch(1111, mock_dummy:fun_with_arity_one(111))
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).


assert_called_should_fail_for_unstubbed_fun_and_expected_arguments_not_called_once_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy_no_parse_transform:fun_with_arity_one/1 ?once ?with([foo])),
    mock_dummy_no_parse_transform:fun_with_arity_one(bar)
  end,
  ?assertError({missing_parse_transform, _}, TestFun()),
  ?assertMatch(error, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).

assert_called_should_fail_for_unstubbed_fun_and_expected_arguments_not_called_twice_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy_no_parse_transform:fun_with_arity_one/1 ?twice ?with([foo])),
    mock_dummy_no_parse_transform:fun_with_arity_one(foo)
  end,
  ?assertError({missing_parse_transform, _}, TestFun()),
  ?assertMatch(error, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).

assert_called_should_fail_for_unstubbed_fun_and_expected_arguments_no_matching_arguments_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy_no_parse_transform:fun_with_arity_one/1 ?once ?with([foo])),
    mock_dummy_no_parse_transform:fun_with_arity_one(bar)
  end,
  ?assertError({missing_parse_transform, _}, TestFun()),
  ?assertMatch(error, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).

assert_called_should_fail_for_stubbed_fun_and_expected_arguments_in_fun_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?once ?andReturn(fun(_Arg = foo) -> ok end)),
    % stubbed fun should not be invoked here, because stub fun args will not match
    ?assertMatch(fun_with_arity_one, mock_dummy:fun_with_arity_one(bar)) 
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertError({assertCalled, _}, check_assertions()).

assert_called_should_succeed_for_stubbed_fun_and_expected_arguments_in_fun_test() -> 
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled(fun mock_dummy:fun_with_arity_one/1 ?once ?andReturn(fun(_Arg = foo) -> ok end)),
    ?assertMatch(ok, mock_dummy:fun_with_arity_one(foo)) 
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).


assert_called_start_mocked_local_gen_server_test() ->
  clean_assertions(),
  ?assertNot(is_pid(whereis(some_module))),
  ?assertCalled({local, some_module} ?never),
  ?assert(is_pid(whereis(some_module))),
  ?assertMatch(ok, check_assertions()).
  
assert_called_start_mocked_global_gen_server_test() ->
  clean_assertions(),
  ?assertNot(is_pid(global:whereis_name(some_module))),
  ?assertCalled({global, some_module} ?times(0)),
  ?assert(is_pid(global:whereis_name(some_module))),
  ?assertMatch(ok, check_assertions()).

assert_called_should_succeed_for_mocked_gen_server_that_returns_fun_value_test() ->
  clean_assertions(),
  ?assertCalled({local, some_module} ?once ?with({save_record, _}) ?andReturn(fun(_) -> ok end)),
  ?assert(is_pid(whereis(some_module))),
  ?assertMatch(ok, gen_server:call(some_module, {save_record, foo})),
  ?assertMatch(ok, check_assertions()).

assert_called_should_succeed_for_mocked_gen_server_that_returns_fixed_value_test() ->
  clean_assertions(),
  ?assertCalled({local, some_module} ?once ?with({save_record, _}) ?andReturn(ok)),
  ?assert(is_pid(whereis(some_module))),
  ?assertMatch(ok, gen_server:call(some_module, {save_record, foo})),
  ?assertMatch(ok, check_assertions()).
  

assert_called_should_succeed_for_mocked_local_gen_server_test() ->
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled({local, some_module} ?once ?with({save_record, _}) ?andReturn(ok)),
    ?assertMatch(ok, gen_server:call(some_module, {save_record, foo}))
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).


assert_called_should_fail_for_mocked_local_gen_server_not_called_once_test() ->
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled({local, some_module} ?once)
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertError({assertCalled, _}, check_assertions()).

assert_called_should_fail_for_mocked_local_gen_server_not_called_once_with_correct_arguements_test() ->
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled({local, some_module} ?once ?with({save_record, _}) ?andReturn(ok)),
    gen_server:call(some_module, {store_record, foo})
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertError({assertCalled, _}, check_assertions()).

assert_called_should_fail_for_mocked_local_gen_server_not_called_twice_test() ->
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled({local, storage} ?twice ?with({save_record, _}) ?andReturn(ok)),
    ?assertMatch(ok, gen_server:call(storage, {save_record, foo}))
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertError({assertCalled, _}, check_assertions()).


assert_called_should_succeed_for_mocked_global_gen_server_test() ->
  clean_assertions(),
  TestFun = fun() ->
    ?assertCalled({global, storage} ?once ?with({save_record, _}) ?andReturn(ok)),
    ?assertMatch(ok, gen_server:call({global, storage}, {save_record, foo}))
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertMatch(ok, estub:check_assertions()).

assert_called_should_fail_for_gen_server_with_different_pid_that_was_not_called_test() ->
  clean_assertions(),
  TestFun = fun() ->
    ?assertMatch(ok, gen_server_dummy:stop(dummy1)),
    ?assertMatch(ok, gen_server_dummy:stop(dummy2)),
    receive after 100 -> ok end,
    {ok, Pid1} = gen_server:start({local, dummy1}, gen_server_dummy, [], []),
    {ok, Pid2} = gen_server:start({local, dummy2}, gen_server_dummy, [], []),
    ?assertCalled(Pid1 ?once ?with([{save_record, _}, _From, _State]) ?andReturn({reply, ok, foo})),
    ?assertCalled(Pid2 ?once ?with([{save_record, _}, _From, _State]) ?andReturn({reply, ok, foo})),
    ?assertMatch(ok, gen_server:call(dummy1, {save_record, foo})),
    gen_server_dummy:stop(Pid1),
    gen_server_dummy:stop(Pid1)
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertError({assertCalled, _}, check_assertions()).
  
assert_called_should_succeed_for_gen_server_with_different_pids_called_test() ->
  clean_assertions(),
  TestFun = fun() ->
    ?assertMatch(ok, gen_server_dummy:stop(dummy1)),
    ?assertMatch(ok, gen_server_dummy:stop(dummy2)),
    receive after 100 -> ok end,
    {ok, Pid1} = gen_server:start({local, dummy1}, gen_server_dummy, [], []),
    {ok, Pid2} = gen_server:start({local, dummy2}, gen_server_dummy, [], []),
    ?assertCalled(Pid1 ?once ?with([{save_record, _}, _From, _State]) ?andReturn({reply, value1, foo})),
    ?assertCalled(Pid2 ?once ?with([{save_record, _}, _From, _State]) ?andReturn({reply, value1, foo})),
    ?assertMatch(value1, gen_server:call(dummy1, {save_record, foo})),
    ?assertMatch(value1, gen_server:call(dummy2, {save_record, foo})),
    gen_server_dummy:stop(Pid1),
    gen_server_dummy:stop(Pid1)
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).

assert_called_should_succeed_for_mocked_existing_gen_server_test() ->
  clean_assertions(),
  TestFun = fun() ->
    ?assertMatch(ok, gen_server_dummy:stop(dummy)),
    receive after 100 -> ok end,
    {ok, Pid} = gen_server:start({local, dummy}, gen_server_dummy, [], []),
    ?assertCalled(Pid ?once ?with([{save_record, _}, _From, _State]) ?andReturn({reply, ok, foo})),
    ?assertMatch(ok, gen_server:call(dummy, {save_record, foo})),
    gen_server_dummy:stop(Pid)
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).

assert_called_should_succeed_for_mocked_gen_server_pid_with_auto_corrected_gen_server_return_test() ->
  clean_assertions(),
  TestFun = fun() ->
    ?assertMatch(ok, gen_server_dummy:stop(dummy)),
    receive after 100 -> ok end,
    {ok, Pid} = gen_server:start({local, dummy}, gen_server_dummy, [], []),
    ?assertCalled(Pid ?once ?with([{save_record, _}, _From, _State]) ?andReturn(ok)),
    ?assertMatch(ok, gen_server:call(dummy, {save_record, foo})),
    gen_server_dummy:stop(Pid)
  end,
  ?assertMatch(ok, eunit:test(TestFun)),
  ?assertMatch(ok, check_assertions()).

assert_called_should_return_tuple_test() ->
  clean_assertions(),
  ?assertCalled(fun mock_dummy:fun_with_arity_three/3 ?atLeastOnce ?andReturn({atomic, ok})),
  ?assertMatch({atomic, ok}, mock_dummy:fun_with_arity_three(foo, foobar, {bar, baz})),
  ?assertMatch(ok, check_assertions()).

foo_test() ->
  clean_assertions(),
  ?assertMatch(ok, gen_server_dummy:stop(dummy1)),
  ?assertMatch(ok, gen_server_dummy:stop(dummy2)),
  receive after 100 -> ok end,
  {ok, Pid1} = gen_server:start({local, dummy1}, gen_server_dummy, [], []),
  {ok, Pid2} = gen_server:start({local, dummy2}, gen_server_dummy, [], []),
  ?assertCalled(Pid1 ?once ?with([{save_record, _}, _From, _State]) ?andReturn({reply, ok1, foo})),
  ?assertCalled(Pid2 ?once ?with([{save_record, _}, _From, _State]) ?andReturn({reply, ok2, foo})),
  ?assertMatch(ok1, gen_server:call(dummy1, {save_record, foo})),
  ?assertMatch(ok2, gen_server:call(dummy2, {save_record, foo})),
  gen_server_dummy:stop(Pid1),
  gen_server_dummy:stop(Pid1),
  check_assertions().

% assert_called_should_succeed_for_mocked_gen_server_with_existing_pid_test() ->
%   TestFun = fun() ->
%     Pid = mock_gen_server_dummy:start(),
%     ?assertCalled({mock_gen_server_dummy, Pid}, ?once ?with({save_record, _}) ?andReturn(ok)),
%     gen_server:call(storage, {save_record, foo})
%   end,
%   ?assertMatch(ok, eunit:test(TestFun)).

%%----------------------------------------------------
%% tests for mocked gen_server
%%----------------------------------------------------

% foo_test() ->
%   ?foo({add, _, _}).
% 
% mock_gen_server_test() ->
%   Pid = ?stub(gen_server, _HandleCallFun = fun(_Event = {add, A, B}) -> A + B end),
%   ?assertCalled(Pid, _Times = once, {args, {add, 1, 9}}),
%   ?assertCalledWith(Pid, _Times = once, {add, _, _}),
%   ?assertCalledWithAndReturns(Pid, _Times = once, {add, _, _}, _Return = ok),
%   ?assertCalledWithAndShouldReturn(Pid, _Times = once, {store_value, _, _}, _ShouldReturn = true),
%   ?assertCalledAndShouldReturn(Pid, _Times = once, _ShouldReturn = true),
%   ?assertCalled(Pid, ?once,  [{with, [10, 1]}, {shouldReturn, 11}]),
%   ?assertCalled(Pid, ?once,  [?with({store_value, _, _}), {andShouldReturn, 11}]),
%   ?assertCalled(Pid, ?once,  [?with({store_value, _, _}), ?andShouldReturn({stored, _UpdatedValue})]),
%   ?assertCalled(Pid, ?twice, [?with({store_value, _, _}), ?andReturn(ok)]),
%   ?assertCalled(Pid, _Times = once, [args]),
%   ?assertCalled(Pid, _Times = once, {returns, 10}),
%   ?assertCalled(Pid, _Times = once, {should_return, 10}).
%   
% 
% new_mock_test() ->
%   Pid = self(),
%   ?assertCalled_(Pid, ?once_),
%   ?assertCalled_(Pid, ?once_ ?with_({store_value, _, _})),
%   ?assertCalled_(Pid, ?twice_ ?with_({store_value, _, _}) ?andShouldReturn_({stored, _UpdatedValue})),
%   ?assertCalled_(Pid, 3 ?times_ ?with_({store_value, _, _}) ?andReturn_(ok)),
% 
%   ?assertCalled_(fun mock_dummy:fun_with_arity_one/1, ?once_),
%   ?assertCalled_(fun mock_dummy:fun_with_arity_one/1, ?once_ ?with_({store_value, _, _})),
%   ?assertCalled_(fun mock_dummy:fun_with_arity_one/1, ?twice_ ?with_({store_value, _, _}) ?andShouldReturn_({stored, _UpdatedValue})),
%   ?assertCalled_(fun mock_dummy:fun_with_arity_one/1, 3 ?times_ ?with_({store_value, _, _}) ?andReturn_(ok)),
%     
%   ?assertCalled_(gen_server_dummy, ?once_ ?with_({store_value, _, _})),
%   ?assertCalled_(gen_server_dummy, ?twice_ ?with_({store_value, _, _}) ?andShouldReturn_({stored, _UpdatedValue})),
%   ?assertCalled_(gen_server_dummy, 3 ?times_ ?with_({store_value, _, _}) ?andReturn_(ok)).

  
% bar_test() ->   
%   Pid = ?stub(gen_server, _HandleCallFun = fun(_Event = {add, A, B}) -> A + B end),
%   ?assertCalled(Pid, _Times = once, {add, _, _}).
  
  % Pid = ?stub(gen_server, _HandleCallFun = fun(_Event = {add_acc, A}, State) -> {_Return = A + State, NewState = A + State} end, _InititalState = 0),
  % Pid = ?stub(gen_server, _HandleCallFun = fun(_Event, _From, State) -> {reply, _Return = true, State} end, _InititalState = 0),
  % ?stub(_ExistingGenServerModule = gen_server_dummy, )

  
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