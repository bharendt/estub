
# estub #

language: *erlang*  
license:  *LGPL*  
version:  *0.2*  
state:    *alpha* (api may change, not all features implemented yet)  
compatibility: *eunit 2.1.1*  


###__estub__ adds stubbing and mocking features to [eunit](http://svn.process-one.net/contribs/trunk/eunit/doc/overview-summary.html).###
With __estub__ you will be able to __mock__ existing __functions__ __and__ gen\_server __processes__.

*estub* provides the following features in detail:

  - __mocking functions__ of modules that are called somewhere in your code you want to test
    - mocking existing __exported__ or __not exported__ functions
    - mocking __not (yet) existing__ functions (stubbing) 
    - mocking functions only __for a specific process__ / that are called from a specific process
  - __mocking gen\_server__ processes your code (that you test) sends gen\_server *call* and *cast* messages to.
    - mocking handling of *call* and *cast* messages of __existing__, __running__ gen\_server processes
    - mocking / __faking__ whole __gen\_server processes__
  - __mocking gen\_fsm__ processes or __any other process__ by using the feature of mocking functions for
    a specific process

You will be able to __make assertions__ ...

  - __how often__ a mocked function is called, e.g. *?once*, *?twice*, *?atLeastOnce* or *10 ?times*
  - __which which arguments__ a mocked function is called, e.g. *?with({my\_arg, _})*
  - __what return value is expected__, e.g. *?andShouldReturn(ok)*

If your test requires that the mocked function/process returns a given value, __you can also return a
fixed/fake value for your mock__ by using the *?andReturn(value)* macro.

### examples: ###

    -ifef(TEST).

    mocking_test() ->
      ?assertCalled(fun calc:add/2, ?once ?with([1,9]) ?andShouldReturn(10)),
      calc:add(1,9), 
      
      ?assertCalled(fun calc:add/2, ?once ?with([1,9]) ?andReturn(ten)),
      ten = calc:add(1,9),

      ?assertCalled(fun calc:add/2, ?once ?with([1,9]) ?andReturn(fun(A,B) -> A + B end)),
      100 = calc:add(1,99),
      
      {ok, Pid} = gen_server:start({local, storage}, storage_module, [], []),
      ?assertCalled(Pid, ?once ?with({save_tuple, _}) ?andReturn(saved)),
      saved = gen_server:call(Pid, {save_tuple, foo}),
      
      ?assertCalled({global, login}, ?once ?with({login, "Me"}) ?andShouldReturn(true)),
      gen_server:call({global, login}, {login, "Me"}),

      {ok, FsmPid} = gen_fsm:start({local, fsm}, fsm_module, [], []),
      ?assertCalled(FsmPid, ?twice ?inState(idle) ?with(get_state) ?andShouldReturn(idle)),
      idle = gen_fsm:sync_send_event(FsmPid, get_state).

    -endif.


### mocking functions: ###

Let's assume you want to test the function *user\_preferences:load/1* and check whether the loading
of user preferences woks correctly. Unfortunately this function validates whether the user is logged in
and only then returns the prefereces (for security reasons). With *estub* it is quite easy. You simply
__mock the function__ *login:is_logged_in/1*, give it an __assertion__ with __which parameters it should be called__,
that it should be __called once__, and that it __should return true__ to fake, that the user is logged in, and
to be able to concentrate on your actual test.

    -module(user_preferences).
    -export([load/1]).

    load(User = #user { user_name = UserName}) ->
      case login:is_logged_in(UserName) of
        true ->
          Peferences = #preferences{},
          % do some loading here that you actually want to test ...
          % ...
          % ...
          {loaded, User#user { preferences = Peferences}};
        false ->
          {error, not_logged_in}
      end.
      
    -ifef(TEST).

    load_preferences_with_mocking_test() ->
      ?assertCalled(fun login:is_logged_in/1, ?once ?with("j.armstrong") ?andReturn(true)),
      ?assertMatch({loaded, _}, load(#user { user_name = "j.armstrong"})).
    
    -endif.

Without *estub* you would first need to login the user to be able to run your actual test

    -ifef(TEST).

    load_preferences_without_mocking_test() ->
      % first find out how the encrypted password for "somepassword" is ...
      EncryptedPassword = registration:encrypt_password("somepassword"),
      % ... if your login function has the precondition, that in checks the password 
      % againts an encrypted password that was stored in a database.
      UserToLogin = #user { user_name = "j.armstrong", encrypted_password = EncryptedPassword}, 
      % then login the user ...
      LoginResult = login:login_user(UserToLogin, _Pwd = "somepassword"),
      % ... and make sure that it worked ...
      ?assertMatch({logged_in, #user{}}, LoginResult),
      % ... get the logged in user ...
      {logged_in, LoggedInUser} = LoginResult,
      % ... and finally test the code you actually wanted to test
      ?assertMatch({loaded, _}, load(LoggedInUser)).
    -endif.

You can see that with estub your test code is shorter, better to read, and even ensures, that the function you
test calls the *login:is_logged\_in/1* function once with the correct arguments.

You can __even__ mock the *login:is_logged\_in/1* function __when it is not yet implemented__ and neither is exported in the
*login* module nor even exists as stub. This is useful for prototyping or if you first write your tests and then
start implementing all requirements (what people say it's recommended ;)

__You can also use the mocking feature__, not to fake the return value, but __to check that the function returns the
expected result__. For this example we would check, that the *login:is_logged\_in/1* function, that is called 
from the code we want to test, returns the correct (not faked) result (*true*) for a logged in user:

    -ifef(TEST).

    load_preferences_with_mocking_and_check_return_test() ->
      % create logged in user 
      EncryptedPassword = registration:encrypt_password("somepassword"),
      UserToLogin = #user { user_name = "j.armstrong", encrypted_password = EncryptedPassword}, 
      {logged_in, LoggedInUser}  = login:login_user(UserToLogin, _Pwd = "somepassword"),
      
      % assert that the fun login:is_logged_in/1 is called correctly 
      % and returns expected (real/unfaked) result: true      
      ?assertCalled(fun login:is_logged_in/1, ?once ?with("j.armstrong") ?andShouldReturn(true)),
      ?assertMatch({loaded, _}, load(LoggedInUser)).
    -endif.


### mocking gen_server processes: ###


You can also use __mocking__ for __*calls*__ or *casts* to __gen\_server processes__. E.g. if you login function wants 
to call a storage process to write the last login time, but in your test you only want to check, whether
the authentication works:

    -module(login).
    -export([login/1]).

    login(User = #user { user_name = UserName, encrypted_password = CryptedPwd}, Password) ->
      case crypto:md5(Password) of
        CryptedPwd ->
          case gen_server:call(storage, {set_last_login_date, now()}) of
            true ->
              {logged_in, User#user { session_id = make_session_id(User)}};
            false ->
              {error, database_error}
          end;
        _ ->
          {error, wrong_password}
      end.
      
    -ifef(TEST).

    load_preferences_with_mocking_test() ->
      ?assertCalled({local, storage}, ?once ?with({set_last_login_date, _}) ?andReturn(true)),
      ?assertMatch({logged_in, _}, login(#user { user_name = "j.armstrong"}, _Pwd = "somepassword")).
    
    -endif.
    
`{local, storage}` as first parameter for the `?assertCalled` macro means, that you want to mock a gen\_server
process that is regitstered with the local name `storage`. For globally registered gen\_server processes use
`{global, Name::term()}`. 
    
You can also mock an existing running real gen_server process:

    -ifef(TEST).

    load_preferences_with_mocking_test() ->
      % start a real gen_server process
      {ok, Pid} = gen_server:start({local, storage}, storage_module, [], []),
      ?assertCalled(Pid, ?once ?with({set_last_login_date, _}) ?andReturn(true)),
      ?assertMatch({logged_in, _}, login(#user { user_name = "j.armstrong"}, _Pwd = "somepassword")).
    
    -endif.

`{Pid, storage_module}` as first parameter for the `?assertCalled` macro means, that you want to mock a gen\_server
process that was started with the process id `Pid` and has the gen\_server callback module `storage_module`. 



### mocking gen_fsm or other processes: ###

It is possible to mock any function of any module and only for a given process. So you are able to mock almost everything.
This example mockes the function `fun my_gen_fsm_module:handle_sync_event/4` for an exsiting gen\_fsm process to return
and validate the current state.

    -ifef(TEST).

    check_gen_fsm_state_with_mocking_test() ->
      {ok, Pid} = my_gen_fsm_module:start(),
      ?assertCalled({Pid, fun my_gen_fsm_module:handle_sync_event/4}, 
          ?once 
          ?with(get_state) 
          ?andReturn(fun(_Event = get_state, _From, StateName, StateData) -> 
            {reply, _Reply = StateName, _NextStateName = StateName, _NewStateData = StateData } end)
          ?andShouldReturn(my_expected_state)
      ),
      my_gen_fsm_module:sync_send_all_state_event(Pid, get_state).
    
    -endif.

For mocking gen\_fsm modules there exist special macros for common tasks like making assertions about the current state of the gen\_fsm module. Otherwise it behaves like mocking gen\_server processes, but you must specify the expected state, in that
the gen\_fsm module should receive the call by using either the `?inState(StateName)` or the `?inAnyState` macro.

    -ifef(TEST).

    mock_gen_fsm_calls_test() ->
      {ok, Pid} = gen_fsm:start({local, registered_name}, gen_fsm_module, [], []),
      ?assertCalled(Pid, ?once ?inState(idle) ?with({set_last_login_date, _}) ?andReturn(true)),
      ?assertCalled({local, registered_name}, ?once ?inAnyState ?with({set_last_login_date, _}) ?andReturn(true)).

    special_assertions_for_gen_fsm_test() ->
      ?assertState(Pid, _State = idle).
    
    -endif.


### requirements: ###

It is required that the __soucecode__ for the modules that are mocked __is available__ because *estub* uses erlang *parse
transform* to recompile and modify the modules for mocking.

To be able to use *estub* in your tests, you must __include__ the *eunit.hrl* header file and __compile__ your code with *TEST* __defined__, as for common *eunit* tests.


------------------------------------------------------------------------------------------------
     *
     *
     *

------------------------------------------------------------------------------------------------

### macros: ###

#### ?assertCalled(\_,\_): ####

    ?assertCalled(what(), howOften() inWhichState() ?with(arguments()) ?andReturn(return()) ?andShouldReturn(expectedReturn()))
    
where

    what() =                                           % to assert that ...
          fun()                                        % this function is called
        | {pid(), fun()}                               % this function is called from the given pid
        | pid()                                        % the gen_server module received a call message as pid or                                          
                                                       % the gen_fsm module received a call message as pid 
                                                       %     in the given state. in that case 
                                                       %     inWhichState() must then be used as option
        | {local | global, registered_name()}          % a created faked gen_server process with registered name 
                                                       %     received a call or
                                                       % a created faked gen_fsm with registered name received 
                                                       %     a call in the given state. in that case
                                                       %     inWhichState() must then be used as option
                                                       % If a process with the given name is already started, 
                                                       % this process is mocked and no fake process is started
        
      where registered_name() = atom() | term()        % global or local registered name of gen_server or 
                                                       % gen_fsm process. for locally registered names only
                                                       % atom() is valid according to OTP restrictions
            
    howOften() =                                       % to assert that what() is called ...
          ?once                                        % 1 times
        | ?twice                                       % 2 times
        | ?atLeastOnce                                 % more than 0 times
        | N ?times                                     % N times 
                                                       % This option is MANDATORY
      where N = integer() >= 0
      
    inWhichState() =                                   % to assert that a gen_fsm module received the call 
        ?inAnyState                                    % in that state. This is mandatory if what() is a 
      | ?inState(StateName)                            % mocked gen_fsm module, otherwise it is not 
                                                       % allowed / ignored
      
      where StateName = all | atom()                                  
            
    arguments() =         % to assert that what() is called with that arguments (optional)
          [argument()] 
        | argument()      % if what() is of arity 1 or gen_server call
      where argument() = term()
      
    expectedReturn() =    % to assert that what() returns the expected result (optional)
          fun()           % where fun must be same arity as what(), or arity 1 for gen_server calls
        | term()
    return()              % to return a fixed / faked return value for what() (optional)
          fun()           % where fun must be same arity as what(), or arity 1 for gen_server calls
        | term()
        
        
Please note, that there is no comma between the options, but one after the `what()` argument.

#### ?once, ?twice, ?atLeastOnce, *N* ?times: ####

One of these macros is the arbitrary first argument for the `?assertCalled` macro assertions list. It ensures that
the mocked function of the `?assertCalled` macro is called as often as specified. There is no `?anyTimes` macro because
it does not make sense of an `?assertCalled` assertion if the mock can be called any times and even never. Use the `?stub` macro if you only want to mock, stub or fake the return value of a given function without making assertions how often and whether the fun is called.

#### ?with(\_): ####

The `?with` macro is an optional argument for the `?assertCalled` macro and ensures, that the mocked function is called with the given arguments. If not, the `?assertCalled` macro  invocation counter is not increased for that call and that call is ignored respectivly. The arguments should be passed as list and the number of arguments should match the arity of the mocked function. 

    ?assertCalled(fun foo:bar/3, ?once ?with([1,2,3])), % is correct
    ?assertCalled(fun foo:bar/3, ?once ?with([1])),     % will always fail, because fun can never be called with one argument
    ?assertCalled(fun foo:bar/3, ?once ?with([1,_,_]))  % use _ "wildcards" instead

For functions with arity 1 you can specify the expected value directly and omit the brackets for the argument list. 

    ?assertCalled(fun foo:bar/1, ?once ?with(1)), % is the same as
    ?assertCalled(fun foo:bar/1, ?once ?with([1]))

This also works for mocked gen\_server calls, because then only the received event is checked, not the `From` and `State`
parameter of the `gen_server:handle_call/3` function:

    ?assertCalled({local, storage}, ?once ?with({save, _})),         % is the same as
    ?assertCalled({local, storage}, ?once ?with([{save, _}, _, _])), % or
    ?assertCalled({local, storage}, ?once ?with([_Event = {save, _}, _From, _State]))


It is also possible to use `_` "wildcards" as you can do in common erlang pattern matching.

    ?assertCalled(fun foo:bar/3, ?once ?with([1,_,_])), 
    ?assertCalled(fun foo:bar/3, ?once ?with([#user{ name = "Me"},_,{foo,_}]))

#### ?andShouldReturn(\_): ####

The `?andShouldReturn` macro is an optional argument for the `?assertCalled` macro and ensures, that the mocked function returns the given result when it is called. If not, the `?assertCalled` macro  invocation counter is not increased for that call and that call is ignored respectivly. The argument can be any term, but no (inline) function.

    ?assertCalled(fun foo:bar/3, ?once ?andShouldReturn(ok)), 
    ?assertCalled(fun foo:bar/3, ?once ?andShouldReturn({ok, Value}),
    ?assertCalled(fun foo:get_function/0, ?once ?andShouldReturn(fun foo:bar/3), % this can be checked
    ?assertCalled(fun foo:bar/3, ?once ?andShouldReturn(fun(_) -> ok end)), % but this not and will never be true respectively
    % because the fun is not evaluated but it is checked whether the return value of fun foo:bar/3 matches
    % the inline fun fun(_) -> ok end
    

It is also possible to use `_` "wildcards" as you can do in common erlang pattern matching.

    ?assertCalled(fun foo:bar/3, ?once ?andShouldReturn({ok, _}), 
    ?assertCalled(fun foo:bar/3, ?once ?andShouldReturn(#user{ name = "Me", password = _}))

#### ?andReturn(\_): ####

The `?andReturn` macro is an optional argument for the `?assertCalled` macro and returns a faked value, for the mocked function.  The argument can be either a fixed value of a fun with the same arity as the mocked fun.

    ?assertCalled(fun foo:bar/3, ?once ?andReturn(ok)), 
    ?assertCalled(fun foo:bar/3, ?once ?andReturn(fun(_) -> ok end))

For mocked gen\_server calls the arity can be also one instead of three, because then only the received event is passed to return function, not the `From` and `State` parameter of the `gen_server:handle_call/3` function:

    ?assertCalled({local, storage}, ?once ?andReturn(fun({save, _}) -> ok end)), % is the same as
    ?assertCalled({local, storage}, ?once ?with(fun(_Event = {save, _}, _From, _State) -> ok end))

Also the return value for mocked gen\_server calls is auto compleated, so the functction can return a simple value instead of returning a valid `gen_server:handle_call/3` return value. 

    ?assertCalled({local, storage}, ?once ?with(fun(_Event = {save, _}, _From, _State) -> ok end)), % is the same as
    ?assertCalled({local, storage}, ?once ?with(fun(_Event = {save, _}, _From, State) -> {reply, ok, State} end))


You can also use pattern matching in the arguments of the return function. In that case the `?assertCalled` macro invocation counter is not increased if the arguments, with that the mocked fun is called, do not match the arguments of the return function  and that call is ignored respectivly. So pattern matching in the return function behaves like the `?with` macro assertion for the `?assertCalled` macro.

    ?assertCalled(fun storage:do/2, ?once ?with([save,_])), % is the same as
    ?assertCalled(fun storage:do/2, ?once ?andReturn(fun(_Action = save, _Value) -> ok end)) % and
    storage:do(load, something) % will not increase the invocation counter because arguments do not match

#### ?stub(\_,\_): ####

The `?stub` macro mocks functions or gen\_server processes like the `?assertCalled` macro but without making assertions how they are called or what they should return but you are able to return faked values. So it behaves like an `?assertCalled` macro only with the `?andReturn` option. With the `?stub` macro you can do things like that:

      5 = calc:sub(10,5),
      ?stub(fun calc:sub/2, ?andReturn(five)),
      five = calc:sub(10,5)
      ?stub(fun calc:sub/2, ?andReturn(fun(A,B) -> B - A end)),
      -5 = calc:sub(10,5),
      
      ?stub({local, user_1}, ?andReturn(fun(_Event = get_id, _From, _State) -> 1 end)),
      1 = gen_server:call(user_1, get_id),
      
      {ok, Pid} = gen_server:start({local, user_2}, user_module, [_Id = 2], []),
      ?stub({pid, user_module}, ?andReturn(fun(_Event = get_id, _From, _State = #state { id = Id}) -> 100 + Id end)),
      102 = gen_server:call(user_2, get_id),

      ?stub({Pid, fun user_module:handle_cast/2}, ?andReturn(fun(_Event = {set_id, Id}, State) -> {reply, State#state { id = Id}} end)),
      gen_server:cast(Pid, {set_id, 100}),
      200 = gen_server:call(Pid, get_id)

#### ?assertState(\_,\_): ####

Asserts that a gen\_fsm process is in the given state.

#### ?getState(\_): ####

Returns the current state object / record that is passed to all callback functions of a gen\_server or gen\_fsm module.


