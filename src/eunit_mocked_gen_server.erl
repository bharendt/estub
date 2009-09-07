%% @author Bertram Harendt <bertram@sauspiel.de>
%%
%% @doc This module represents that a mocked gen_server.
-module(eunit_mocked_gen_server).
-behaviour(gen_server).
-author(bharendt).
-mocked(true).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).
-export([stop/0, stop/1]).

-record(state, {
    mocked_module_name
  }).

%%----------------------------------------------------
%% gen_server implementation
%%----------------------------------------------------

init([ModuleName]) ->
  State = #state{
    mocked_module_name = ModuleName
  },
  {ok, State}.
  
stop() ->
  stop(?MODULE).

stop(Process) ->
  gen_server:call(Process, '__$stop__').
  
handle_cast(Event, State) ->
  error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {self, self()}, 
                            {message, 'received unhandled event in handle_cast.'},
                            {event, Event}]),
  {noreply, State}.

handle_call('__$stop__', _From, State) ->
  {stop, normal, ok, State};
  
handle_call(Event, From, State = #state {mocked_module_name = ModuleName}) ->
  case eunit_mock:execute__mock__(?MODULE, handle_call, 3, [Event, From, State], self()) of
    no____mock ->
      error_logger:info_report([{message, 'received unhandled event in handle_call.'},
                                {module, ModuleName}, {pid, self()}, {event, Event}]),
      {reply, error, State};
    Reply ->
      Reply
  end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, Data, _Extra) ->
 io:format("\t   received code change event in tcp server ~n"),
 {ok, Data}.  

handle_info(Event, Data) ->
  error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {self, self()}, 
                            {message, 'received unhandled event in handle_info.'},
                            {event, Event}]),
  {noreply, Data}.