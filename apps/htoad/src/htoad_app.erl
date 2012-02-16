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
    {ok, {Args, Files}} = getopt:parse(optspec(), init:get_plain_arguments()),
    case Files of
        [] ->
            getopt:usage(optspec(), "htoad","[file ...]",
                         [{"file", "Toadies to process"}]),
            init:stop(),
            {ok, self()};
        _ ->
            ets:new(htoad_trace, [named_table, public, bag]),
            process_args(Args),
            Result = htoad_sup:start_link(Args),
            [ htoad:assert({load, File}) || File <- Files ],
            init_engine(Args),
            htoad:assert({htoad_command, apply}),
            Result
    end.


stop(_State) ->
    ok.

%% private

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
     {log, $l, "log", {string, "error"}, "Log output level (debug, info, error)"}
    ].
