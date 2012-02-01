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
                         [{"file", "Instructions file to process"}]),
            init:stop(),
            {ok, self()};
        _ ->
            {ok, Pid} = htoad_sup:start_link(Args, Files),
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
    ].
