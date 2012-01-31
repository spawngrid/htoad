-module(htoad_app).

-behaviour(application).

-include_lib("htoad/include/stdlib.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = htoad_sup:start_link(),
    init(),
    {ok, Pid}.

stop(_State) ->
    ok.

%% private

init() ->
    {ok, Modules} = application:get_env(htoad, modules),
    [ ok = seresye:add_rules(htoad_engine, Module) || Module <- Modules ],
    seresye:assert(htoad_engine, #init{}).
