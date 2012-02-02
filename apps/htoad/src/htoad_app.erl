-module(htoad_app).
-behaviour(application).

-include_lib("htoad/include/htoad.hrl").

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
                         [{"file", "Instructions file to process"}]),
            init:stop(),
            {ok, self()};
        _ ->
            process_args(Args),
            htoad_sup:start_link(Args, Files)
    end.


stop(_State) ->
    ok.

%% private
        
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
