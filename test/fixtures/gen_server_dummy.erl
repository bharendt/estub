
-module(gen_server_dummy).
-behaviour(gen_server).
-compile([{parse_transform, estub}]).
-export([init/1, handle_call/3, handle_cast/2, stop/0, stop/1,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).


init([]) ->
  State = #state{},
  {ok, State}.

stop() ->
  stop(?MODULE).

stop(Process) ->
  gen_server:cast(Process, stop).

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(Event, State) ->
  error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {self, self()}, 
                            {message, 'received unhandled event in handle_cast.'},
                            {event, Event}]),
  {noreply, State}.


handle_call({save_record, _}, _From, State) ->
  {reply, saved, State};

handle_call(Event, _From, State) ->
  error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {self, self()}, 
                            {message, 'received unhandled event in handle_call.'},
                            {event, Event}]),
  {reply, error, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, Data, _Extra) ->
 {ok, Data}.  

handle_info(Event, Data) ->
  error_logger:info_report([{module, ?MODULE}, {line, ?LINE}, {self, self()}, 
                            {message, 'received unhandled event in handle_info.'},
                            {event, Event}]),
  {noreply, Data}.