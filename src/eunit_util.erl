-module(eunit_util).
-author(bharendt).

-export([refresh/1, refresh/2, refresh_test/1, refresh_trace/1]).
-export([test/1, test_module/1]).
-export([parse/1, eval/1, is_process_alive/1]).

test(ModuleName) when is_atom(ModuleName) ->
  case compile:file("src/" ++ atom_to_list(ModuleName) ++ ".erl", [report_errors,{i, "./include"}, {i, "./include/eunit"}, {outdir, "./ebin"}, {d,'TEST', true}]) of
   {ok, ModuleName} -> io:format("ok.~n"), true;
   Error -> io:format("~n  ****** FAILED ****** ~p~n",[Error]), false
  end.

test_module(Mod) ->
  eunit:test(Mod, [{verbose, true}]).
  
parse(String) ->
  {ok, Scanned, _} = erl_scan:string(String),
  {ok, Parsed} = erl_parse:parse_exprs(Scanned),
  Parsed.

eval(String) ->
  erl_eval:exprs(parse(String), erl_eval:bindings(erl_eval:new_bindings())).

refresh(Module) when is_atom(Module) ->
  refresh(Module, get_processes(Module), []).
  
refresh_test(Module) ->
  refresh(Module, get_processes(Module), [{d,'TEST',true}, export_all]).

refresh_trace(Module) ->
  refresh(Module, get_processes(Module), [{d,'TEST',true}, {d, 'TRACE', true}, export_all]).
  
refresh(Module, CompileOptions) ->
  refresh(Module, get_processes(Module), CompileOptions).

%% refreshes a single module
refresh(Module, Processes, ExtraOptions) when is_atom(Module) ->
  io:format("~ncompiling module '~w':~n",[Module]),
  SourceDir = case lists:keysearch(source_dir, 1, ExtraOptions) of 
    {value, {source_dir, Dir}} -> Dir;
    _ -> "src/"
  end,
  case compile:file(SourceDir ++ atom_to_list(Module) ++ ".erl", [verbose,report_errors,report_warnings, {i, "./include"}, {outdir, "./ebin"}] ++ ExtraOptions) of
    {ok, Module} ->
      [sys:suspend(Pid) || Pid <- Processes],
      code:purge(Module),
      case code:load_file(Module) of
        {module, Module} ->
          io:format("~nSUCCESSFULLY compiled and reloaded module '~w'",[Module]),
          if length(Processes) > 0 ->
            io:format("~nSending code change event to ~B processes~n",[length(Processes)]),
            [sys:change_code(Pid, Module, foo, foo) || Pid <- Processes];
          true -> ok
          end;
        ReloadError ->
          io:format("~nFAILED to reload module '~w': ~w",[Module, ReloadError])
      end,
      [sys:resume(Pid) || Pid <- Processes],
      ok;
    CompileError ->
      io:format("~nFAILED to compile module '~w': ~w",[Module, CompileError]),
      error
  end.

get_processes(_Module) ->
  [].
  
is_process_alive(Pid) when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]);

is_process_alive(_Pid) ->
    false.

  