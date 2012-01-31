-module(htoad_app).

-behaviour(application).

-include_lib("htoad/include/stdlib.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, {Args, Rest}} = getopt:parse(optspec(), init:get_plain_arguments()),
    case proplists:get_value(command, Args) of
        undefined ->
            getopt:usage(optspec(), "htoad"),
            init:stop(),
            {ok, self()};
        _ ->
            {ok, Pid} = htoad_sup:start_link(Args, Rest),
            init(),
            {ok, Pid}
    end.


stop(_State) ->
    ok.

%% private

init() ->
    {ok, Modules} = application:get_env(htoad, modules),
    [ ok = seresye:add_rules(htoad_engine, Module) || Module <- Modules ],
    seresye:assert(htoad_engine, #init{}).


optspec() ->
    [
     {command, undefined, undefined, string, "Command to run"}
    ].
