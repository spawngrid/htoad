-module(htoad_app).
-behaviour(application).

-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, {Args, Cmds}} = getopt:parse(optspec(), init:get_plain_arguments()),
    case Cmds of
        [] ->
            getopt:usage(optspec(), "htoad","<command>",
                         [{"command", "Command to run"}]),
            init:stop(),
            {ok, self()};
        ["start"] ->
            process_args(Args),
            Result = htoad_sup:start_link(Args),
            init_engine(Args),
            case proplists:get_value(shell, Args) of
                true ->
                    shell:start();
                _ ->
                    skip
            end,
            Result;
        ["apply", Filename] ->
            Result = htoad_sup:start_link(Args),
            htoad_file_server:redirect("/$htoad/master", filename:absname(os:getenv("HTOAD_CWD")), original_file_server),
            Hosts = htoad_hosts(),
            io:format("Applying ~s to ~p~n", [Filename, Hosts]),
            rpc:multicall(Hosts, htoad_file_server, redirect, ["/$htoad/master", "/$htoad/master", {file_server_2, node()}]),
            rpc:multicall(Hosts, htoad, retract, [{load, Filename}]),
            rpc:multicall(Hosts, htoad, assert, [{load, Filename}]),
            rpc:multicall(Hosts, htoad, retract, [{htoad_command, apply}]),
            rpc:multicall(Hosts, htoad, assert, [{htoad_command, apply}]),
            init:stop(),
            Result;
        _ ->
            io:format("Unknown command, terminating~n"),
            init:stop(),
            {ok, self()}
    end.


stop(_State) ->
    ok.

%% private

to_node(Host) when is_atom(Host) ->
    to_node(atom_to_list(Host));
to_node(Host) when is_list(Host) ->
    list_to_atom("htoad@" ++ Host).

htoad_hosts() ->
    {ok, Hosts} = file:consult(htoad_utils:file(".htoad.hosts")),
    lists:map(fun to_node/1, Hosts).

init_engine(Args) ->
    {ok, Modules} = application:get_env(htoad, modules),
    [ ok = htoad:add_rules(Module) || Module <- Modules ],
    htoad:assert([{htoad_argument, Arg} || Arg <- Args ]),
    Signals = [ {htoad_toadie_server_ready, Pid} || {_, Pid, _, _} <- supervisor:which_children(htoad_toadies) ],
    htoad:assert(htoad_utils:on(Signals, #init{})).

        
process_args([]) ->
    ok;
process_args([{log, LevelName}|T]) ->
    Level =
        case LevelName of
            "debug" -> debug;
            "info" -> info;
            "error" -> error;
            _ -> error
        end,
    lager:set_loglevel(lager_console_backend, Level),
    process_args(T);
process_args([_|T]) ->
    process_args(T).



optspec() ->
    [
     {host, undefined, "host", string, "Override host name"},
     {log, $l, "log", {string, "error"}, "Log output level (debug, info, error)"},
     {shell, undefined, "shell", {boolean, false}, "Enables Erlang shell"}
    ].
