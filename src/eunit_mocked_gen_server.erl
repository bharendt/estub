%% @author Bertram Harendt <bertram@sauspiel.de>
%%
%% @doc This module represents that a mocked gen_server.
-module(eunit_mocked_gen_server).
-behaviour(gen_server).
-author(bharendt).

-export([init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

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

handle_cast({'$add_assert_call', Assertion}, State) ->
  io:format("adding assertion to gen_server process: ~w~n", [Assertion]),
  {noreply, State};

handle_cast(Event, State) ->
  error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {self, self()}, 
                            {message, 'received unhandled event in handle_cast.'},
                            {event, Event}]),
  {noreply, State}.

handle_call(Event, _From, State = #state {mocked_module_name = ModuleName}) ->
  case eunit_mock:execute__mock__(Event, self(), call) of
    no____mock ->
      error_logger:info_report([{message, 'received unhandled event in handle_call.'},
                                {module, ModuleName}, {pid, self()}, {event, Event}]),
      {reply, error, State};
    Reply ->
      {reply, Reply, State}
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