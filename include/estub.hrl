
-ifndef(ESTUB_HRL).
-define(ESTUB_HRL, true).

%% allow defining TEST to override NOTEST
-ifdef(TEST).
-undef(NOTEST).
-endif.

%% allow defining DEBUG to override NODEBUG
-ifdef(DEBUG).
-undef(NODEBUG).
-endif.

%% allow NODEBUG to imply NOASSERT, unless overridden below
-ifdef(NODEBUG).
-ifndef(NOASSERT).
-define(NOASSERT, true).
-endif.
-endif.

%% note that the main switch used within this file is NOTEST; however,
%% both TEST and EUNIT may be used to check whether testing is enabled
-ifndef(NOTEST).
-undef(NOASSERT).    % testing requires that assertions are enabled
-ifndef(TEST).
-define(TEST, true).
-endif.
-ifndef(EUNIT).
-define(EUNIT, true).
-endif.
-else.
-undef(EUNIT).
-endif.

%% allow ASSERT to override NOASSERT (regardless of TEST/NOTEST)
-ifdef(ASSERT).
-undef(NOASSERT).
-endif.

%% Macros for stubbing and mocking
-ifdef(NOASSERT).
-define(stub(Fun, ReturnValue), ok).
-else.
-define(stub(Fun, ReturnValue),
	((fun () ->
	    __FunAsModuleName = Fun, % to avoid compile error for guard expression is_atom
	    case (estub:stub(Fun, ReturnValue)) of
	  __Pid when is_atom(__FunAsModuleName), is_pid(__Pid) -> __Pid; % for stubbing gen_servers and gen_fsm    
		ok -> ok;
    % __V when is_atom(__FunAsModuleName) -> .erlang:error({stub_gen_process_failed,
    %           [{module, ?MODULE},
    %            {line, ?LINE},
    %            {gen_process, (??Fun)},
    %            {return_value, (??ReturnValue)},
    %            {value, __V}]});
		__V -> .erlang:error({stub_failed,
				      [{module, ?MODULE},
				       {line, ?LINE},
				       {function, (??Fun)},
				       {return_value, (??ReturnValue)},
				       {value, __V}]})
	    end
	  end)())).
-endif.
-define(_stub(Fun, ReturnValue), ?_test(?stub(Fun, ReturnValue))).

-ifdef(NOASSERT).
-define(assertCalled(_Options), ok).
-else.
-define(assertCalled(Options),
	((fun () ->
	  __ALL_OPTIONS__ = [Options],
	  [__FUN__ | __OPTIONS__] = __ALL_OPTIONS__,
	  case (estub:assert_called(__FUN__, __OPTIONS__, ?MODULE, ?LINE)) of
		  ok -> ok;
		  __V -> .erlang:error({assertCalled_failed,
				      [{module, ?MODULE},
				       {line, ?LINE},
				       {assertion, (??Options)},
				       {value, __V}]})
	    end
	  end)())).
-endif.
-define(_assertCalled(Options), ?_test(?assertCalled(Options))).



-define(never, ,{times, 0}).
-define(once, ,{times, 1}).
-define(twice, ,{times, 2}).
-define(atLeastOnce,  ,{times, at_least_once}).
-define(times(Times), ,{times, Times}).
-define(with(Args), ,{with, fun(__Value__) -> % this check is done by calling this fun with the arguments, the stubbed fun was called with as array
                              case __Value__ of 
                                  Args -> did___match;
                                  [Args] -> did___match; % if args are not given as array if fun is of arity one
                                  _ -> did___not__match 
                              end 
                          end, ??Args}).
-define(andShouldReturn(ExpectedResult), ,{andShouldReturn, fun(ExpectedResult) -> did___match end, ??ExpectedResult}).
-define(andReturn(ReturnValue), ,{andReturn, ReturnValue, ??ReturnValue}).
-define(inState(StateName), ,{inState, StateName}).
-define(inAnyState, ,{inState, all}).

-define(mock(GenServerModule, GenServerStartFun, GenServerStartFunArgs),ok).
-define(assertCalledWith(Fun, Times, Arguments), ?_test(?assertCalledWith(Fun, Times, Arguments))).
-define(assertCalledWithAndReturns(Fun, Times, Arguments, ReturnValue), ok).
-define(assertCalledWithAndShouldReturn(Fun, Times, Arguments, ReturnValue), ok).
-define(assertCalledAndShouldReturn(Fun, Times, ReturnValue), ok).


-endif. % ESTUB_HRL
