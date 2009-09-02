%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%


%% @author Bertram Harendt <bertram@sauspiel.de>
%%
%% @doc This module <em>transforms all functions of a module</em>  that is compiled
%% with the compile options <code>-compile({parse_transform, eunit_mock})</code> so
%% that the <em>function executes the mock</em> that is used in a eunit test case
%% <em>instead of the real function code.</em> If mocking is not used, the original
%% code of the function is executed.<br/>All functions of the compiled module are transformed
%% like this:<br/><br/>
%% <b>From:</b><br/>
%% <pre>
%%  fun_that_can_be_mocked(Arg1, [Arg2 | _]) ->
%%    io:format("executing unmocked_fun/2~n"),
%%    if Arg1 > Arg2 -> 'unmocked_fun/2:true';
%%    true -> 'unmocked_fun/2:false' end.
%% </pre>
%% <b>To:</b><br/>
%% <pre>
%%  fun_that_can_be_mocked(__Arg1__ = Arg1, __Arg2__ = [Arg2 | _]) ->
%%    case eunit_mock:execute__mock__(?MODULE, mocked_fun, 2, [__Arg1__, __Arg2__], self()) of
%%      no____mock ->
%%        io:format("executing unmocked_fun/2~n"),
%%        if Arg1 > Arg2 -> 'unmocked_fun/2:true';
%%        true -> 'unmocked_fun/2:false' end;
%%      __Mocked___Result__ -> __Mocked___Result__
%%    end.
%% </pre>
%% <code>eunit_mock:execute__mock__/5</code> returns the result of the mock if mocking is used, otherwise
%% it returns the atom <code>no____mock</code> and the original function code is executed.<br/><br/>
%% When transformed, the functions can be mocked like this:
%% <pre>
%%  % compile module 'coding' with option {parse_transform, eunit_mock}
%%  -module(coding).
%%  get_coolest_language(List) -> 
%%    hd(List).
%%
%%  % execute unmocked function
%%  Ruby = coding:get_coolest_language([ruby, erlang, java, cpp]). % = ruby
%%  % mock function
%%  eunit_mock:mock(_Module = coding, _FunName = get_coolest_language, _Arity = 1, _MockFun = fun(List) -> erlang end), 
%%  % no we get mocked result:
%%  Erlang = coding:get_coolest_language([ruby, erlang, java, cpp]). % = erlang
%% </pre><br/>
%% This module is based on the <code>erl_id_trans</code> module from the std erlang library that
%% traverses legal Erlang code. 
%% See <a href="http://erlang.org/doc/man/erl_id_trans.html">http://erlang.org/doc/man/erl_id_trans.html</a>.
-module(eunit_mock).
-behaviour(gen_server).
-author(bharendt).

-export([parse_transform/2]).
-export([execute__mock__/3, execute__mock__/5]).
-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).
-export([stub/2, assert_called/5, check_assertions/0]).

-ifdef(TRACE).
-define(trace, io:format).
-else.
-define(trace, no_trace).
-endif.

%% the current state of the mock server
-record(state, {
    stubs = [],
    gen_server_stubs = []
  }).

%% record holding information about stubbed function
-record(stub, {
    module_name,      %% atom(): the name of the module to stub the given function for
    fun_name,         %% atom(): the name of the (exported) function in the module to stub
    arity,            %% int(): the arity of the function to stub
    pid = none,        %% none | pid(): the process that should call the fun, or none, if any process
                      %%         can call that fun
    returns = ignore, %% fun() | term() | ignore: either a function or a fixed return value that should
                      %%    be returned instead of the real function. ignore is only valid when
                      %% assertions are set.
    assertions = []   %% [#assert_call{}]: the assertions that were made for this function
  }).

-record(gen_server_stub, {
    pid,              %% pid(): the process id of the mocked gen_server 
    type,             %% call | cast: the type of the expected event
    returns = ignore, %% fun() | term() | ignore: either a function or a fixed return value that should
                      %%    be returned by the mocked gen_server handle_call fun. ignore is only valid when
                      %% assertions are set.
    assertions = []   %% [#assert_call{}]: the assertions that were made for this gen_server
  }).

%% record holding information about for an ?assertCall assertion
-record(assert_call, {
    required_call_count,    %% int(): indicates hov often the function should be called 
    current_call_count = 0, %% int(): indicates how often the function was called (with matching arguments)
    expected_arguments,     %% {with, match(), MatchText::string()} where match() = fun() | term()
    expected_return,        %% {andShouldReturn, match(), MatchText::string()} where match() = fun() | term()
    returns,                %% {andReturn, return(), ReturnText::string()} where return() = fun() | term()
    location                %% tuple() = {MODULE, LINE}: the location of the assertion
  }).

-ifdef(TEST).
  -include("test/eunit_mock_test.erl").
-endif.


%%----------------------------------------------------
%% stub and mocking exports
%%----------------------------------------------------

stub(Fun, ReturnValue) when is_function(Fun) ->
  case check_arity(Fun, ReturnValue) of 
    ok ->
      case fun_to_stub_record(Fun) of 
        _Error = {error, Error} -> Error;
        Stub = #stub{module_name = ModuleName} ->
          WasCompiledWithMocking = case is_mocked(ModuleName) of
            false -> recompile_for_mocking(ModuleName);
            true -> ok
          end,
          case WasCompiledWithMocking of
            ok ->
              set_stub(Stub#stub { returns = ReturnValue}),
              ok;
            Error -> Error
          end
      end;
    {error, Reason} -> 
      Reason
  end;
stub(gen_server, _ReturnValue) ->
  self();
stub(gen_fsm, _ReturnValue) ->
  self();
stub(GenSomethingModule, _ReturnValue) when is_atom(GenSomethingModule) ->
  self().

%% @doc Adds an assertion for a test that checks that <code>Mock</code> is called
%% n times. 
%% <code>MODULE</code> and <code>LINE</code> are set by the <code>?assertCalled</code>
%% macro to be able to identify the assertion when it failed.
%% @spec assert_called(Mock::mock(), Times::times(), Options::expected(), MODULE::atom(), LINE::int()) -> ok | {error, Reason} 
%%  where
%%    mock() = fun() | pid() | {local, mockable_module()} | {global, mockable_module()},
%%    mockable_module() = gen_server_module() | gen_fsm_module(),
%%    gen_server_module() = atom(),
%%    gen_fsm_module = atom(),
%%    times() = once | twice | integer() | ignore, 
%%    options() = [option()],
%%    option() = {with, match(), MatchText::string()} | {andShouldReturn, match(), MatchText::string()} | {andReturn, return(), ReturnText::string()}, 
%%    match() = fun() | term(),
%%    return() = fun() | term(),
%%    arguments() = [term()]
assert_called({GlobalOrLocal, GenServerModuleName}, Times, Options, MODULE, LINE) when is_atom(GenServerModuleName), 
                                                       (GlobalOrLocal == global orelse GlobalOrLocal == local),
                                                       (is_integer(Times) orelse Times == at_least_once),
                                                       is_list(Options) ->
  case 
    case
      case GlobalOrLocal of
        global -> global:whereis_name(GenServerModuleName); 
        local  -> whereis(GenServerModuleName)
      end
    of
      undefined ->
        gen_server:start({GlobalOrLocal, GenServerModuleName}, eunit_mocked_gen_server, [GenServerModuleName], []);
      ExistingPid ->
        {ok, ExistingPid}                                                       
    end 
  of
    {ok, Pid} -> 
      add_assert_call({gen_server, GlobalOrLocal, Pid, _Type = call}, create_assertion(Times, Options, _Arity = 1), MODULE, LINE);
    Error -> Error
  end;
assert_called({GenServerPid, GenServerModuleName}, Times, Options, MODULE, LINE) when is_atom(GenServerModuleName), 
                                                       is_pid(GenServerPid),
                                                       (is_integer(Times) orelse Times == at_least_once),
                                                       is_list(Options) ->
  WasCompiledWithMocking = case is_mocked(GenServerModuleName) of
    false -> recompile_for_mocking(GenServerModuleName);
    true -> ok
  end,
  case WasCompiledWithMocking of
    ok ->
      case create_assertion(Times, Options, _Arity = 3) of
        Assertion = #assert_call{} ->
          add_assert_call(_Stub = #stub { module_name = GenServerModuleName, fun_name = handle_call, arity = 3, pid = GenServerPid} , Assertion, MODULE, LINE);
        Error -> Error
      end;
    Error -> Error
  end;
assert_called(Mock, Times, Options, MODULE, LINE) when is_function(Mock), 
                                                       (is_integer(Times) orelse Times == at_least_once),
                                                       is_list(Options) -> 
  case fun_to_stub_record(Mock) of 
    _Error = {error, Error} -> Error;
    Stub = #stub{module_name = ModuleName, arity = Arity} ->
      WasCompiledWithMocking = case is_mocked(ModuleName) of
        false -> recompile_for_mocking(ModuleName);
        true -> ok
      end,
      case WasCompiledWithMocking of
        ok ->
          case create_assertion(Times, Options, Arity) of
            Assertion = #assert_call{} ->
              add_assert_call(Stub, Assertion, MODULE, LINE);
            Error -> Error
          end;
        Error -> Error
      end
  end.

execute__mock__(Arguments, GenServerPid, Type = call) when is_pid(GenServerPid) -> 
  case get_stub(GenServerPid, Type) of
    false ->
      no____mock;
    {value, Stub = #gen_server_stub {assertions = Assertions = [_|_], returns = StubReturnValue}} -> 
      {ReturnValue, NewAssertions, AssertionMatched} = update_assertions(Assertions, _Arity = 1, Arguments),
      % only update assertion if a assertion matched
      if AssertionMatched -> set_stub(Stub#gen_server_stub { assertions = NewAssertions}); true -> ok end,
      case ReturnValue of
        % if assertion had no won stub value, return the value from a previous stub (if exist)
        ignore -> return_mock_result(StubReturnValue, _ModuleName = gen_server, _FunctionName = handle_call, _Arity = 1, Arguments, GenServerPid);
        % otherwise return value from assertion with stub
        _ -> return_mock_result(ReturnValue, _ModuleName = gen_server, _FunctionName = handle_call, _Arity = 1, Arguments, GenServerPid)
      end;
    {value, _Stub = #gen_server_stub {returns = StubbedReturn}} ->
      return_mock_result(StubbedReturn, _ModuleName = gen_server_stub, _FunctionName = handle_call, _Arity = 1, Arguments, GenServerPid)    
  end;
execute__mock__(_Arguments, GenServerPid, _Type = cast) when is_pid(GenServerPid) -> 
  no____mock.
  
execute__mock__(ModuleName, FunctionName, Arity, Arguments, Self) -> 
  ?trace("executing do_mock(~w, ~w, ~w, ~w, ~w)~n", [ModuleName, FunctionName, Arity, Arguments, Self]),
  case 
    case get_stub(ModuleName, FunctionName, Arity, Self) of
      false -> get_stub(ModuleName, FunctionName, Arity, _ExpectedPid = none);
      Found = {value, _} -> Found
    end
  of 
    false ->
      no____mock;
    % if assertion has stubbed value, return stubbed value from matched assertion and update assertion
    {value, Stub = #stub {assertions = Assertions = [_|_], returns = StubReturnValue}} -> 
      {ReturnValue, NewAssertions, AssertionMatched} = update_assertions(Assertions, Arity, Arguments),
      % only update assertion if a assertion matched
      if AssertionMatched -> set_stub(Stub#stub { assertions = NewAssertions}); true -> ok end,
      case ReturnValue of
        % if assertion had no won stub value, return the value from a previous stub (if exist)
        ignore -> return_mock_result(StubReturnValue, ModuleName, FunctionName, Arity, Arguments, Self);
        % otherwise return value from assertion with stub
        _ -> return_mock_result(ReturnValue, ModuleName, FunctionName, Arity, Arguments, Self)
      end;
    {value, _Stub = #stub {returns = StubbedReturn}} ->
      return_mock_result(StubbedReturn, ModuleName, FunctionName, Arity, Arguments, Self)    
  end.
  
  
%%----------------------------------------------------
%% mock server api
%%----------------------------------------------------

set_stub(Stub = #stub{}) ->
  cast_to_mock_server({set_stub, Stub});
set_stub(Stub = #gen_server_stub{}) ->
  cast_to_mock_server({set_stub, Stub}).

get_stub(GenServerPid, Type) when is_pid(GenServerPid), is_atom(Type) ->
  call_mock_server({get_stub, GenServerPid, Type}).
get_stub(ModuleName, FunName, Arity, Pid) when is_atom(ModuleName), is_atom(FunName), is_integer(Arity), is_pid(Pid) orelse Pid == ignore orelse Pid == none ->
  call_mock_server({get_stub, ModuleName, FunName, Arity, Pid}).

add_assert_call(Stub = #stub{}, Assertion, MODULE, LINE) ->
  cast_to_mock_server({add_assert_call, Stub, Assertion#assert_call{location = {MODULE, LINE}}});
add_assert_call(Stub = {gen_server, _GlobalOrLocal, _Pid, _CallOrCast}, Assertion, MODULE, LINE) ->
  cast_to_mock_server({add_assert_call, Stub, Assertion#assert_call{location = {MODULE, LINE}}}).


check_assertions() ->
  case global:whereis_name(?MODULE) of
    undefined -> ok; % assume that no assertions were made if server is not running
    Pid -> 
      case gen_server:call(Pid, {get_assertions_and_reset, global}) of
        [] -> ok; % no assertions were made
        [_|_] = Stubs -> check_assertions(Stubs, ok)          
      end
  end.

  
%%----------------------------------------------------
%% gen_server implementation
%%----------------------------------------------------

init([]) ->
  State = #state{},
  {ok, State}.

handle_cast({add_assert_call, Stub = #stub{module_name = ModuleName, fun_name = FunName, arity = Arity, pid = Pid}, Assertion = #assert_call{}}, State = #state { stubs = Stubs}) ->
  NewStub = case find_stub(ModuleName, FunName, Arity, Pid, Stubs) of 
    {value, ExistingStub = #stub {assertions = ExistingAssertions}} ->
      ExistingStub#stub {
        assertions = set_assertion(Assertion, ExistingAssertions)
      };
    false -> 
      Stub#stub {
        assertions = set_assertion(Assertion, [])
      }
  end,
  ?trace("~n~n!!!! new stubs = ~p~n~n~n", [set_stub(NewStub, Stubs)]),
  {noreply, State#state {stubs = set_stub(NewStub, Stubs)}};

handle_cast({add_assert_call, _Stub = {gen_server, _GlobalOrLocal, Pid, CallOrCast}, Assertion = #assert_call{}}, State = #state { gen_server_stubs = Stubs}) ->
  NewStub = case find_gen_server_stub(Pid, CallOrCast, Stubs) of 
    {value, ExistingStub = #gen_server_stub {assertions = ExistingAssertions}} ->
      ExistingStub#gen_server_stub {
        assertions = set_assertion(Assertion, ExistingAssertions)
      };
    false -> 
      #gen_server_stub {
        pid = Pid,
        type = CallOrCast,
        assertions = set_assertion(Assertion, [])
      }
  end,
  {noreply, State#state {gen_server_stubs = set_stub(NewStub, Stubs)}};


handle_cast({set_stub, NewStub = #stub{}}, State = #state { stubs = Stubs}) ->
  {noreply, State#state {stubs = set_stub(NewStub, Stubs)}};

handle_cast({set_stub, NewStub = #gen_server_stub{}}, State = #state { gen_server_stubs = Stubs}) ->
  {noreply, State#state {gen_server_stubs = set_stub(NewStub, Stubs)}};
  
handle_cast(Event, State) ->
  error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {self, self()}, 
                            {message, 'received unhandled event in handle_cast.'},
                            {event, Event}]),
  {noreply, State}.


handle_call({get_stub, ModuleName, FunName, Arity, Pid}, _From, State = #state{stubs = Stubs}) ->
  {reply, find_stub(ModuleName, FunName, Arity, Pid, Stubs), State};

handle_call({get_stub, GenServerPid, Type}, _From, State = #state{ gen_server_stubs = Stubs}) ->
  {reply, find_gen_server_stub(GenServerPid, Type, Stubs), State};

% gets all stubs with assertions and then deletes all local stubs (that were added for one test) 
handle_call({get_assertions_and_reset, local}, _From, State) ->
  % TODO: implement. (currently local does the same as global)
  handle_call({get_assertions_and_reset, global}, _From, State);

% gets all stubs with assertions and then deletes all stubs 
handle_call({get_assertions_and_reset, global}, _From, State = #state{stubs = FunStubs, gen_server_stubs = GenServerStubs}) ->
  FunStubsWithAssertions = [Stub || Stub = #stub{assertions = Assertions} <- FunStubs, Assertions /= []],
  GenServerStubsWithAssertions = [Stub || Stub = #gen_server_stub{assertions = Assertions} <- GenServerStubs, Assertions /= []],
  {reply, FunStubsWithAssertions ++ GenServerStubsWithAssertions, State#state {stubs = [], gen_server_stubs = []}};

handle_call(Event, _From, State) ->
  error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {self, self()}, 
                            {message, 'received unhandled event in handle_call.'},
                            {event, Event}]),
  {reply, error, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, Data, _Extra) ->
 ?trace("\t   received code change event in tcp server ~n"),
 {ok, Data}.  

handle_info(Event, Data) ->
  error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {self, self()}, 
                            {message, 'received unhandled event in handle_info.'},
                            {event, Event}]),
  {noreply, Data}.

call_mock_server(Event) ->
  assert_started(),
  gen_server:call({global, ?MODULE}, Event).

cast_to_mock_server(Event) ->
  assert_started(),
  gen_server:cast({global, ?MODULE}, Event).
  
assert_started() ->
  case global:whereis_name(?MODULE) of
    undefined ->
      {ok, _} = gen_server:start({global, ?MODULE}, ?MODULE, [], []);
    _Pid -> ok
  end.

%%----------------------------------------------------
%% helpers
%%----------------------------------------------------

fun_to_stub_record(Fun) when is_function(Fun) ->
  case erlang:fun_info(Fun, type) of
    {type, local} -> {error, local_fun};
    {type, external} ->
      {module, ModuleName} = erlang:fun_info(Fun, module),
      {name, FunName} = erlang:fun_info(Fun, name),
      {arity, Arity} = erlang:fun_info(Fun, arity),
      #stub { module_name = ModuleName, fun_name = FunName, arity = Arity}
  end.

find_gen_server_stub(Pid, Type, _Stubs = []) when is_pid(Pid), is_atom(Type) ->
  false;
find_gen_server_stub(Pid, Type, _Stubs = [Stub = #gen_server_stub{ pid = ExistingPid, type = ExistingType} | _]) when is_pid(Pid), is_atom(Type), Pid == ExistingPid, Type == ExistingType ->
  {value, Stub};
find_gen_server_stub(Pid, Type, _Stubs = [#gen_server_stub{} | Rest]) when is_pid(Pid), is_atom(Type) ->
  find_gen_server_stub(Pid, Type, Rest).

find_stub(ModuleName, FunName, Arity, Pid, _Stubs = []) when is_atom(ModuleName), is_atom(FunName), is_integer(Arity), is_pid(Pid) orelse Pid == ignore orelse Pid == none ->
  false;
find_stub(ModuleName, FunName, Arity, Pid, _Stubs = [Stub = #stub{ module_name = M, fun_name = F, arity = A, pid = P} | _]) 
      when ModuleName == M, FunName == F, Arity == A, Pid == P orelse Pid == ignore,
      is_atom(ModuleName), is_atom(FunName), is_integer(Arity), is_pid(Pid) orelse Pid == ignore orelse Pid == none ->
  {value, Stub};
find_stub(ModuleName, FunName, Arity, Pid, _Stubs = [#stub{ } | Rest]) when is_atom(ModuleName), is_atom(FunName), is_integer(Arity), is_pid(Pid) orelse Pid == ignore orelse Pid == none ->
  find_stub(ModuleName, FunName, Arity, Pid, Rest).
  
set_stub(NewStub = #stub {}, Stubs) when is_list(Stubs) ->
  set_stub(NewStub, Stubs, []);
set_stub(NewStub = #gen_server_stub {}, Stubs) when is_list(Stubs) ->
  set_stub(NewStub, Stubs, []).
  
set_stub(NewStub = #stub {}, _Stubs = [], CheckStubs) when is_list(CheckStubs) ->
  [NewStub | lists:reverse(CheckStubs)];
set_stub(NewStub = #gen_server_stub {}, _Stubs = [], CheckStubs) when is_list(CheckStubs) ->
  [NewStub | lists:reverse(CheckStubs)];
set_stub(NewStub = #stub {module_name = N1, fun_name = F1, arity = A1, pid = P1}, _Stubs = [#stub{module_name = N2, fun_name = F2, arity = A2, pid = P2}|Rest], CheckStubs ) 
                                        when is_list(CheckStubs), N1 == N2, F1 == F2, A1 == A2, P1 == P2 -> 
  lists:reverse(CheckStubs) ++ [NewStub | Rest];
set_stub(NewStub = #gen_server_stub {pid = Pid1}, _Stubs = [#gen_server_stub{pid = Pid2}|Rest], CheckStubs ) 
                                        when is_list(CheckStubs), Pid1 == Pid2 -> 
  lists:reverse(CheckStubs) ++ [NewStub | Rest];
set_stub(NewStub = #stub {}, _Stubs = [Stub = #stub{}|Rest], CheckStubs) when is_list(CheckStubs) -> 
  set_stub(NewStub, Rest, [Stub | CheckStubs]);
set_stub(NewStub = #gen_server_stub {}, _Stubs = [Stub = #gen_server_stub{}|Rest], CheckStubs) when is_list(CheckStubs) -> 
  set_stub(NewStub, Rest, [Stub | CheckStubs]).


set_assertion(Assertion = #assert_call{}, Assertions) when is_list(Assertions) ->
  [Assertion | Assertions].

check_arity(Fun, StubFun) when is_function(Fun), is_function(StubFun)->
  {arity, StubFunArity} = erlang:fun_info(StubFun, arity),
  case erlang:fun_info(Fun, arity) of
    {arity, StubFunArity} -> ok;
    _ -> {error, arity_mismatch}
  end;
check_arity(Fun, _ReturnValue) when is_function(Fun) ->
  ok.

create_assertion(Times, Options, Arity) when is_list(Options), is_integer(Arity) ->
  create_assertion(Options, #assert_call{required_call_count = Times}, Arity);
  
create_assertion(_Options = [], Assertion = #assert_call{}, _Arity) ->  
  Assertion;
create_assertion(_Options = [options___end | Rest], Assertion = #assert_call{}, Arity) ->  
  create_assertion(Rest, Assertion, Arity);
create_assertion(_Options = [Option = {with, Fun, _} | Rest], Assertion = #assert_call{}, Arity) when is_function(Fun, Arity) ->  
  create_assertion(Rest, Assertion#assert_call{expected_arguments = Option}, Arity);
create_assertion(_Options = [Option = {with, Fun, _} | Rest], Assertion = #assert_call{}, Arity) when is_function(Fun, 1) ->  % ?with() macro creates fun of arity one
  create_assertion(Rest, Assertion#assert_call{expected_arguments = Option}, Arity);
create_assertion(_Options = [_Option = {with, Fun, _} | _Rest], _Assertion = #assert_call{}, _Arity) when is_function(Fun) ->  
  {error, wrong_arity};
create_assertion(_Options = [Option = {with, Arguments, _} | Rest], Assertion = #assert_call{}, Arity) when is_list(Arguments), length(Arguments) == Arity ->  
  create_assertion(Rest, Assertion#assert_call{expected_arguments = Option}, Arity);
create_assertion(_Options = [_Option = {with, Arguments, _} | _Rest], _Assertion = #assert_call{}, Arity) when is_list(Arity), length(Arguments) /= Arity ->  
  {error, wrong_arity};
create_assertion(_Options = [_Option = {with, SingleArgument, ArgumentText} | Rest], Assertion = #assert_call{}, Arity) ->  
  create_assertion([{with, [SingleArgument], ArgumentText} | Rest], Assertion, Arity);

create_assertion(_Options = [Option = {andShouldReturn, Fun, _} | Rest], Assertion = #assert_call{}, Arity) when is_function(Fun, Arity) ->  
  create_assertion(Rest, Assertion#assert_call{expected_return = Option}, Arity);
create_assertion(_Options = [_Option = {andShouldReturn, Fun, _} | _Rest], _Assertion = #assert_call{}, _Arity) when is_function(Fun) ->  
  {error, wrong_arity};
create_assertion(_Options = [Option = {andShouldReturn, _, _} | Rest], Assertion = #assert_call{}, Arity) ->  
  create_assertion(Rest, Assertion#assert_call{expected_return = Option}, Arity);

create_assertion(_Options = [Option = {andReturn, Fun, _} | Rest], Assertion = #assert_call{}, Arity) when is_function(Fun, Arity) ->  
  create_assertion(Rest, Assertion#assert_call{returns = Option}, Arity);
create_assertion(_Options = [_Option = {andReturn, Fun, _} | _Rest], _Assertion = #assert_call{}, _Arity) when is_function(Fun) ->  
  {error, wrong_arity};
create_assertion(_Options = [Option = {andReturn, _, _} | Rest], Assertion = #assert_call{}, Arity) ->  
  create_assertion(Rest, Assertion#assert_call{returns = Option}, Arity).
  
is_mocked(ModuleName) when is_atom(ModuleName) ->
  case lists:keysearch(mocked, 1, ModuleName:module_info(attributes)) of
    false -> false;
    {value,{mocked,[true]}} -> true
  end.
  
recompile_for_mocking(ModuleName) when is_atom(ModuleName) ->
  case ModuleName:module_info(compile) of
    CompileInfo when is_list(CompileInfo) ->
      {value, {options, CompileOptions}} = lists:keysearch(options, 1, CompileInfo),
      {value, {source, SourcePath}} = lists:keysearch(source, 1, CompileInfo),
      case compile:file(SourcePath, [{parse_transform, eunit_mock} | CompileOptions]) of 
        {ok, ModuleName} ->
          code:purge(ModuleName),
          case code:load_file(ModuleName) of
            {module, ModuleName} ->
              ok;
            ReloadError -> {error, ReloadError}
          end;
        CompileError -> {error, CompileError}
      end;
    Error -> {error, Error}
  end.

checkMatch({_, Fun, _}, Found) when is_function(Fun) ->
  case catch Fun(Found) of
    {'EXIT', _E} -> ?trace("ERROR: ~p~n", [_E]), false;
    did___match -> true;
    did___not__match -> false;
    FunResult -> FunResult
  end;
checkMatch({_, Required, _}, Found) ->
  case Found of
    Required -> true;
    _ -> false
  end;
checkMatch(undefined, _) ->
  true.

update_assertions(Assertions, Arity, Arguments) -> 
  % TODO: test
  update_assertions(Assertions, Arity, Arguments, ignore, [], false).
  
update_assertions([], _Arity, _Arguments, ReturnValue, UpdatedAssertions, AssertionsMatched) ->
  {ReturnValue, lists:reverse(UpdatedAssertions), AssertionsMatched};

update_assertions([Assertion = #assert_call{ expected_arguments = ExpectedArguments, expected_return = ExpectedReturn, returns = Returns} | Rest], Arity, Arguments, ReturnValue, UpdatedAssertions, AssertionsMatched) ->
  Expected = checkMatch(ExpectedArguments, Arguments) andalso checkMatch(ExpectedReturn, Arguments),
  ?trace("Expected = ~p~n", [Expected]),
  ?trace("ExpectedArguments = ~p~n", [ExpectedArguments]),
  ?trace("ExpectedReturn = ~p~n", [ExpectedReturn]),
  ?trace("Arguments = ~p~n", [Arguments]),
  ?trace("checkMatch(ExpectedArguments, Arguments) = ~p~n", [checkMatch(ExpectedArguments, Arguments)]),
  ?trace("checkMatch(ExpectedReturn, Arguments) = ~p~n", [checkMatch(ExpectedReturn, Arguments)]),
  {NewReturn, ReturnFunMatched} = case Returns of
    _ when ReturnValue /= ignore -> ?trace("using old return value ~w~n", [ReturnValue]), {ReturnValue, true}; % keep first (= more recent) return value
    undefined -> ?trace("no return value specified ~n"), {ReturnValue, true};
    {andReturn, ReturnFun, _F} when is_function(ReturnFun, Arity) ->
      ?trace("returning value from function ~s~n", [_F]),
      case catch erlang:apply(ReturnFun, if is_list(Arguments) -> Arguments; true -> [Arguments] end) of
        {'EXIT', _R} -> ?trace("shit exit~n Arguments = ~w, ReturnFun = ~w ~n reason = ~p~n", [Arguments, ReturnFun, _R]), {ReturnValue, false};
        ExecResult -> ?trace("exec succeeded~n"), {ExecResult, true}
      end;
    {andReturn, FixedReturnValue, _} -> ?trace("fixed return value~n"), {FixedReturnValue, true}
  end,
  {NewAssertion, NewAssertionsMatched} = if Expected andalso ReturnFunMatched -> 
    {Assertion#assert_call{current_call_count = Assertion#assert_call.current_call_count + 1}, true};
  true ->
    {Assertion, AssertionsMatched}
  end,
  update_assertions(Rest, Arity, Arguments, NewReturn, [NewAssertion | UpdatedAssertions], NewAssertionsMatched).  

  
return_mock_result(ignore, _ModuleName, _FunctionName, _Arity, _Arguments, _Self) -> 
  no____mock;
return_mock_result({return, Value}, ModuleName, FunctionName, Arity, Arguments, Self) -> 
  return_mock_result(Value, ModuleName, FunctionName, Arity, Arguments, Self);
return_mock_result(Fun, _ModuleName, _FunctionName, Arity, Arguments, _Self) when is_function(Fun, Arity) -> 
  case catch erlang:apply(Fun, Arguments) of 
    {'EXIT', _E} -> ?trace("error ~p~n", [_E]), no____mock; % if arguments of fun does not match, ignore mocking
    ReturnValue -> ReturnValue
  end;
return_mock_result(Fun, ModuleName, FunctionName, Arity, _Arguments, _Self) when is_function(Fun) -> 
  .erlang:error({arity_mismatch, [{function, list_to_atom(atom_to_list(ModuleName) ++ ":" ++ 
                                             atom_to_list(FunctionName) ++ "/" ++ 
                                             integer_to_list(Arity))},
                                  {stub, Fun}]});
return_mock_result(Value, _ModuleName, _FunctionName, _Arity, _Arguments, _Self) -> 
  Value.
  
check_assertions([], Acc) ->
  Acc;
check_assertions([Stub = #gen_server_stub{ assertions = Assertions } | Rest], Acc) ->
  NewAcc = check_assertions(Stub, Assertions, Acc),
  check_assertions(Rest, NewAcc);
check_assertions([Stub = #stub{ assertions = Assertions } | Rest], Acc) ->
  NewAcc = check_assertions(Stub, Assertions, Acc),
  check_assertions(Rest, NewAcc).
  
check_assertions(_Stub, [], Acc) ->
  Acc;
check_assertions(Stub, [#assert_call{ required_call_count = at_least_once, current_call_count = Current} | Rest], _Acc) when Current > 0 ->
  check_assertions(Stub, Rest, ok);
check_assertions(Stub, [#assert_call{ required_call_count = Required, current_call_count = Current} | Rest], _Acc) when Required == Current ->
  check_assertions(Stub, Rest, ok);
check_assertions(Stub, [#assert_call{ required_call_count = Required, current_call_count = Current} | Rest], _Acc) when Required /= Current ->
  .erlang:error({assertCalled, [stub_error_info(Stub), {required, Required}, {current, Current}]}),
  check_assertions(Stub, Rest, error);
check_assertions(Stub, [UnknownAssertion | Rest], _Acc) ->
  .erlang:error({unknown_assertion, [stub_error_info(Stub),{assertion, UnknownAssertion}]}),
  check_assertions(Stub, Rest, error).
 
stub_error_info(#stub{module_name = ModuleName, fun_name = FunctionName, arity = Arity}) -> 
  {function, list_to_atom(atom_to_list(ModuleName) ++ ":" ++ atom_to_list(FunctionName) ++ "/" ++ integer_to_list(Arity))};
stub_error_info(#gen_server_stub{pid = Pid, type = Type}) -> 
  Name = case process_info(Pid, registered_name) of
    {registered_name, RegName} when is_atom(RegName) -> atom_to_list(RegName) ++ "/";
    _ -> pid_to_list(Pid) ++ "/"
  end,
  Info = case Type of
    call -> list_to_atom(Name ++ "handle_call");
    cast -> list_to_atom(Name ++ "handle_cast")
  end,
  {gen_server, Info}.
                                    
no_trace(_) -> ok.
no_trace(_, _) -> ok.
 
%%----------------------------------------------------
%% mocking parse transform
%%----------------------------------------------------


%% @doc transforms all function of a module that is compiled with the compile option
%% <code>-compile({parse_transform, eunit_mock})</code> so that they can be mocked
%% and return the result of the mock function instead of the real function.
parse_transform(Forms, _Options) ->
    forms(insert_mocked_atrribute(Forms), Forms).

insert_mocked_atrribute(Forms) ->
  lists:foldr(
    fun(Form = {attribute,_LineNumber,module,_ModuleName}, Acc) -> [Form, {attribute,1,mocked,true} | Acc];
       (Form, Acc) -> [Form | Acc]
  end, [], Forms).

%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0|Fs0], AllForms) ->
    F1 = form(F0, AllForms),
    Fs1 = forms(Fs0, AllForms),
    [F1|Fs1];
forms([], _) -> [].

%% -type form(Form) -> Form.
%%  Here we show every known form and valid internal structure. We do not
%%  that the ordering is correct!

%% First the various attributes.
form({attribute,Line,module,Mod}, _Forms) ->
    {attribute,Line,module,Mod};
form({attribute,Line,file,{File,Line}}, _Forms) ->	%This is valid anywhere.
    {attribute,Line,file,{File,Line}};
form({attribute,Line,export,Es0}, _Forms) ->
    Es1 = farity_list(Es0),
    {attribute,Line,export,Es1};
form({attribute,Line,import,{Mod,Is0}}, _Forms) ->
    Is1 = farity_list(Is0),
    {attribute,Line,import,{Mod,Is1}};
form({attribute,Line,compile,C}, _Forms) ->
    {attribute,Line,compile,C};
form({attribute,Line,record,{Name,Defs0}}, _Forms) ->
    Defs1 = record_defs(Defs0),
    {attribute,Line,record,{Name,Defs1}};
form({attribute,Line,asm,{function,N,A,Code}}, _Forms) ->
    {attribute,Line,asm,{function,N,A,Code}};
form({attribute,Line,Attr,Val}, _Forms) ->		%The general attribute.
    {attribute,Line,Attr,Val};
form({function,Line,Name0,Arity0,Clauses0}, Forms) ->
    NewClauses = case Clauses0 of
      FunClauses = [{clause, LineNumber, Args, Guards, _Content} | _] when is_integer(LineNumber), is_list(Args), is_list(Guards), Name0 =/= execute__mock__ -> 
        ?trace("mocking function ~w/~w~n", [Name0, Arity0]),
        ModuleName = get_module_name(Forms),
        [transform_mock_fun(CurLineNumber, ModuleName, Name0, CurArgs, CurGuards, CurContent) || {clause, CurLineNumber, CurArgs, CurGuards, CurContent} <- FunClauses];
      _ -> ?trace("not mocking function ~w/~w~n", [Name0, Arity0]), Clauses0
    end,
    {Name,Arity,Clauses} = function(Name0, Arity0, NewClauses),
    {function,Line,Name,Arity,Clauses};
% Mnemosyne, ignore...
form({rule,Line,Name,Arity,Body}, _Forms) ->
    {rule,Line,Name,Arity,Body}; % Dont dig into this
%% Extra forms from the parser.
form({error,E}, _Forms) -> {error,E};
form({warning,W}, _Forms) -> {warning,W};
form({eof,Line}, _Forms) -> {eof,Line}.


get_module_name([]) -> {error, module_name_not_found};
get_module_name([{attribute,_Line,module,ModuleName} | _]) -> ModuleName;
get_module_name([_ | Rest]) -> get_module_name(Rest).

transform_mock_fun(LineNumber, ModuleName, FunName, Args, Guards, Content) when is_integer(LineNumber), is_list(Args), is_list(Guards) -> 
  {clause, LineNumber, transform_mock_args(Args, LineNumber), Guards, trasform_mock_content(Content, LineNumber, ModuleName, FunName, length(Args))}.

transform_mock_args(Args, LineNumber) when is_list(Args) ->
  element(1, lists:foldr(fun(Arg, {Mapped, Index}) -> 
    {[{match,LineNumber,{var,LineNumber,list_to_atom("__Arg" ++ integer_to_list(Index) ++ "__")},Arg} | Mapped], Index - 1}
  end, {[], length(Args)}, Args)).
  
trasform_mock_content(Content, LineNumber, ModuleToMock, FunToMock, Arity) ->
  [{'case',LineNumber,
    {call,LineNumber,
     {remote,LineNumber,{atom,LineNumber,eunit_mock},{atom,LineNumber,execute__mock__}},
     [{atom,LineNumber,ModuleToMock},
      {atom,LineNumber,FunToMock},
      {integer,LineNumber,Arity},
      create_mock_execute_aguments(Arity, LineNumber),
      {call,LineNumber,{atom,LineNumber,self},[]}]},
    [{clause,LineNumber,
      [{atom,LineNumber,no____mock}],
      [],
      Content},
     {clause,LineNumber,
      [{var,LineNumber,'__Mocked___Result__'}],
      [],
      [{var,LineNumber,'__Mocked___Result__'}]}]}].

create_mock_execute_aguments(_NumArguments = 0, LineNumber) ->
  {nil,LineNumber};
create_mock_execute_aguments(NumArguments, LineNumber) ->
  lists:foldr(fun(Index, Acc) -> 
    {cons,LineNumber, {var,LineNumber,list_to_atom("__Arg" ++ integer_to_list(Index) ++ "__")},Acc}
  end, create_mock_execute_aguments(0,LineNumber), lists:seq(1,NumArguments)).

%% -type farity_list([Farity]) -> [Farity] when Farity <= {atom(),integer()}.

farity_list([{Name,Arity}|Fas]) ->
    [{Name,Arity}|farity_list(Fas)];
farity_list([]) -> [].

%% -type record_defs([RecDef]) -> [RecDef].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs([{record_field,Line,{atom,La,A},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Line,{atom,La,A},Val1}|record_defs(Is)];
record_defs([{record_field,Line,{atom,La,A}}|Is]) ->
    [{record_field,Line,{atom,La,A}}|record_defs(Is)];
record_defs([]) -> [].

%% -type function(atom(), integer(), [Clause]) -> {atom(),integer(),[Clause]}.

function(Name, Arity, Clauses0) ->
    Clauses1 = clauses(Clauses0),
    {Name,Arity,Clauses1}.

%% -type clauses([Clause]) -> [Clause].

clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|clauses(Cs)];
clauses([]) -> [].

%% -type clause(Clause) -> Clause.

clause({clause,Line,H0,G0,B0}) ->
    H1 = head(H0),
    G1 = guard(G0),
    B1 = exprs(B0),
    {clause,Line,H1,G1,B1}.

%% -type head([Pattern]) -> [Pattern].

head(Ps) -> patterns(Ps).

%% -type patterns([Pattern]) -> [Pattern].
%%  These patterns are processed "sequentially" for purposes of variable
%%  definition etc.

patterns([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|patterns(Ps)];
patterns([]) -> [].

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.

pattern({var,Line,V}) -> {var,Line,V};
pattern({match,Line,L0,R0}) ->
    L1 = pattern(L0),
    R1 = pattern(R0),
    {match,Line,L1,R1};
pattern({integer,Line,I}) -> {integer,Line,I};
pattern({char,Line,C}) -> {char,Line,C};
pattern({float,Line,F}) -> {float,Line,F};
pattern({atom,Line,A}) -> {atom,Line,A};
pattern({string,Line,S}) -> {string,Line,S};
pattern({nil,Line}) -> {nil,Line};
pattern({cons,Line,H0,T0}) ->
    H1 = pattern(H0),
    T1 = pattern(T0),
    {cons,Line,H1,T1};
pattern({tuple,Line,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {tuple,Line,Ps1};
%%pattern({struct,Line,Tag,Ps0}) ->
%%    Ps1 = pattern_list(Ps0),
%%    {struct,Line,Tag,Ps1};
pattern({record,Line,Name,Pfs0}) ->
    Pfs1 = pattern_fields(Pfs0),
    {record,Line,Name,Pfs1};
pattern({record_index,Line,Name,Field0}) ->
    Field1 = pattern(Field0),
    {record_index,Line,Name,Field1};
%% record_field occurs in query expressions
pattern({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Name,Field1};
pattern({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Field1};
pattern({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
pattern({op,Line,Op,A}) ->
    {op,Line,Op,A};
pattern({op,Line,Op,L,R}) ->
    {op,Line,Op,L,R}.

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs]) ->
    S2 = case S1 of
	     default ->
		 default;
	     _ ->
		 expr(S1)
	 end,
    T2 = case T1 of
	     default ->
		 default;
	     _ ->
		 bit_types(T1)
	 end,
    [{bin_element,L1,expr(E1),S2,T2} | pattern_grp(Fs)];
pattern_grp([]) ->
    [].

bit_types([]) ->
    [];
bit_types([Atom | Rest]) when is_atom(Atom) ->
    [Atom | bit_types(Rest)];
bit_types([{Atom, Integer} | Rest]) when is_atom(Atom), is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest)].



%% -type pattern_list([Pattern]) -> [Pattern].
%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|pattern_list(Ps)];
pattern_list([]) -> [].

%% -type pattern_fields([Field]) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields([{record_field,Lf,{atom,La,F},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Lf,{atom,La,F},P1}|pattern_fields(Pfs)];
pattern_fields([{record_field,Lf,{var,La,'_'},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Lf,{var,La,'_'},P1}|pattern_fields(Pfs)];
pattern_fields([]) -> [].

%% -type guard([GuardTest]) -> [GuardTest].

guard([G0|Gs]) when is_list(G0) ->
    [guard0(G0) | guard(Gs)];
guard(L) ->
    guard0(L).

guard0([G0|Gs]) ->
    G1 =  guard_test(G0),
    [G1|guard0(Gs)];
guard0([]) -> [].

guard_test(Expr={call,Line,{atom,La,F},As0}) ->
    case erl_internal:type_test(F, length(As0)) of
	true -> 
	    As1 = gexpr_list(As0),
	    {call,Line,{atom,La,F},As1};
	_ ->
	    gexpr(Expr)
    end;
guard_test(Any) ->
    gexpr(Any).

%% Before R9, there were special rules regarding the expressions on
%% top level in guards. Those limitations are now lifted - therefore
%% there is no need for a special clause for the toplevel expressions.
%% -type gexpr(GuardExpr) -> GuardExpr.

gexpr({var,Line,V}) -> {var,Line,V};
gexpr({integer,Line,I}) -> {integer,Line,I};
gexpr({char,Line,C}) -> {char,Line,C};
gexpr({float,Line,F}) -> {float,Line,F};
gexpr({atom,Line,A}) -> {atom,Line,A};
gexpr({string,Line,S}) -> {string,Line,S};
gexpr({nil,Line}) -> {nil,Line};
gexpr({cons,Line,H0,T0}) ->
    H1 = gexpr(H0),
    T1 = gexpr(T0),				%They see the same variables
    {cons,Line,H1,T1};
gexpr({tuple,Line,Es0}) ->
    Es1 = gexpr_list(Es0),
    {tuple,Line,Es1};
gexpr({record_index,Line,Name,Field0}) ->
    Field1 = gexpr(Field0),
    {record_index,Line,Name,Field1};
gexpr({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = gexpr(Rec0),
    Field1 = gexpr(Field0),
    {record_field,Line,Rec1,Name,Field1};
gexpr({record,Line,Name,Inits0}) ->
    Inits1 = grecord_inits(Inits0),
    {record,Line,Name,Inits1};
gexpr({call,Line,{atom,La,F},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) of
	true -> As1 = gexpr_list(As0),
		{call,Line,{atom,La,F},As1}
    end;
% Guard bif's can be remote, but only in the module erlang...
gexpr({call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or 
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0),
		{call,Line,{remote,La,{atom,Lb,erlang},{atom,Lc,F}},As1}
    end;
% Unfortunately, writing calls as {M,F}(...) is also allowed.
gexpr({call,Line,{tuple,La,[{atom,Lb,erlang},{atom,Lc,F}]},As0}) ->
    case erl_internal:guard_bif(F, length(As0)) or
	 erl_internal:arith_op(F, length(As0)) or 
	 erl_internal:comp_op(F, length(As0)) or
	 erl_internal:bool_op(F, length(As0)) of
	true -> As1 = gexpr_list(As0),
		{call,Line,{tuple,La,[{atom,Lb,erlang},{atom,Lc,F}]},As1}
    end;
gexpr({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
gexpr({op,Line,Op,A0}) ->
    case erl_internal:arith_op(Op, 1) or 
	 erl_internal:bool_op(Op, 1) of
	true -> A1 = gexpr(A0),
		{op,Line,Op,A1}
    end;
gexpr({op,Line,Op,L0,R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
    %% R11B: andalso/orelse are now allowed in guards.
    L1 = gexpr(L0),
    R1 = gexpr(R0),			%They see the same variables
    {op,Line,Op,L1,R1};
gexpr({op,Line,Op,L0,R0}) ->
    case erl_internal:arith_op(Op, 2) or
	  erl_internal:bool_op(Op, 2) or 
	  erl_internal:comp_op(Op, 2) of
	true ->
	    L1 = gexpr(L0),
	    R1 = gexpr(R0),			%They see the same variables
	    {op,Line,Op,L1,R1}
    end.

%% -type gexpr_list([GuardExpr]) -> [GuardExpr].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

gexpr_list([E0|Es]) ->
    E1 = gexpr(E0),
    [E1|gexpr_list(Es)];
gexpr_list([]) -> [].

grecord_inits([{record_field,Lf,{atom,La,F},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|grecord_inits(Is)];
grecord_inits([{record_field,Lf,{var,La,'_'},Val0}|Is]) ->
    Val1 = gexpr(Val0),
    [{record_field,Lf,{var,La,'_'},Val1}|grecord_inits(Is)];
grecord_inits([]) -> [].

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es]) ->
    E1 = expr(E0),
    [E1|exprs(Es)];
exprs([]) -> [].

%% -type expr(Expression) -> Expression.

expr({var,Line,V}) -> {var,Line,V};
expr({integer,Line,I}) -> {integer,Line,I};
expr({float,Line,F}) -> {float,Line,F};
expr({atom,Line,A}) -> {atom,Line,A};
expr({string,Line,S}) -> {string,Line,S};
expr({char,Line,C}) -> {char,Line,C};
expr({nil,Line}) -> {nil,Line};
expr({cons,Line,H0,T0}) ->
    H1 = expr(H0),
    T1 = expr(T0),				%They see the same variables
    {cons,Line,H1,T1};
expr({lc,Line,E0,Qs0}) ->
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    {lc,Line,E1,Qs1};
expr({bc,Line,E0,Qs0}) ->
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    {bc,Line,E1,Qs1};
expr({tuple,Line,Es0}) ->
    Es1 = expr_list(Es0),
    {tuple,Line,Es1};
%%expr({struct,Line,Tag,Es0}) ->
%%    Es1 = pattern_list(Es0),
%%    {struct,Line,Tag,Es1};
expr({record_index,Line,Name,Field0}) ->
    Field1 = expr(Field0),
    {record_index,Line,Name,Field1};
expr({record,Line,Name,Inits0}) ->
    Inits1 = record_inits(Inits0),
    {record,Line,Name,Inits1};
expr({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Name,Field1};
expr({record,Line,Rec0,Name,Upds0}) ->
    Rec1 = expr(Rec0),
    Upds1 = record_updates(Upds0),
    {record,Line,Rec1,Name,Upds1};
expr({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Field1};
expr({block,Line,Es0}) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0),
    {block,Line,Es1};
expr({'if',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'if',Line,Cs1};
expr({'case',Line,E0,Cs0}) ->
    E1 = expr(E0),
    Cs1 = icr_clauses(Cs0),
    {'case',Line,E1,Cs1};
expr({'receive',Line,Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1};
expr({'receive',Line,Cs0,To0,ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Cs1 = icr_clauses(Cs0),
    {'receive',Line,Cs1,To1,ToEs1};
expr({'try',Line,Es0,Scs0,Ccs0,As0}) ->
    Es1 = exprs(Es0),
    Scs1 = icr_clauses(Scs0),
    Ccs1 = icr_clauses(Ccs0),
    As1 = exprs(As0),
    {'try',Line,Es1,Scs1,Ccs1,As1};
expr({'fun',Line,Body}) ->
    case Body of
	{clauses,Cs0} ->
	    Cs1 = fun_clauses(Cs0),
	    {'fun',Line,{clauses,Cs1}};
	{function,F,A} ->
	    {'fun',Line,{function,F,A}};
	{function,M,F,A} ->			%R10B-6: fun M:F/A.
	    {'fun',Line,{function,M,F,A}}
    end;
expr({call,Line,F0,As0}) ->
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    F1 = expr(F0),
    As1 = expr_list(As0),
    {call,Line,F1,As1};
expr({'catch',Line,E0}) ->
    %% No new variables added.
    E1 = expr(E0),
    {'catch',Line,E1};
expr({'query', Line, E0}) ->
    %% lc expression
    E = expr(E0),
    {'query', Line, E};
expr({match,Line,P0,E0}) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    {match,Line,P1,E1};
expr({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
expr({op,Line,Op,A0}) ->
    A1 = expr(A0),
    {op,Line,Op,A1};
expr({op,Line,Op,L0,R0}) ->
    L1 = expr(L0),
    R1 = expr(R0),				%They see the same variables
    {op,Line,Op,L1,R1};
%% The following are not allowed to occur anywhere!
expr({remote,Line,M0,F0}) ->
    M1 = expr(M0),
    F1 = expr(F0),
    {remote,Line,M1,F1}.

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es]) ->
    E1 = expr(E0),
    [E1|expr_list(Es)];
expr_list([]) -> [].

%% -type record_inits([RecordInit]) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits([{record_field,Lf,{atom,La,F},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|record_inits(Is)];
record_inits([{record_field,Lf,{var,La,'_'},Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{var,La,'_'},Val1}|record_inits(Is)];
record_inits([]) -> [].

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field,Lf,{atom,La,F},Val0}|Us]) ->
    Val1 = expr(Val0),
    [{record_field,Lf,{atom,La,F},Val1}|record_updates(Us)];
record_updates([]) -> [].

%% -type icr_clauses([Clause]) -> [Clause].

icr_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|icr_clauses(Cs)];
icr_clauses([]) -> [].

%% -type lc_bc_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals([{generate,Line,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{generate,Line,P1,E1}|lc_bc_quals(Qs)];
lc_bc_quals([{b_generate,Line,P0,E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{b_generate,Line,P1,E1}|lc_bc_quals(Qs)];
lc_bc_quals([E0|Qs]) ->
    E1 = expr(E0),
    [E1|lc_bc_quals(Qs)];
lc_bc_quals([]) -> [].

%% -type fun_clauses([Clause]) -> [Clause].

fun_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|fun_clauses(Cs)];
fun_clauses([]) -> [].
