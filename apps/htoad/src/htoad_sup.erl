-module(htoad_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

%% API
-export([start_link/2, start_seresye/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(_Args, Files) ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, [Files]).

start_seresye() ->
    {ok, Pid} = seresye:start(htoad_engine),
    init(),
    {ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([Files]) ->
    #one_for_one{
      children = [
                  #worker{
                     id = htoad_engine,
                     restart = permanent,
                     start_func = {htoad_sup, start_seresye, []}
                    },
                  #one_for_one {
                           id = htoad_modules,
                           registered = htoad_modules,
                           children =
                               [ 
                                 #worker {
                                    id = File,
                                    modules = dynamic,
                                    restart = permanent,
                                    start_func = {htoad_module_server, start_link, [File]}
                                   } || File <- Files ]
                          }
                 ]
     }.


%% private

init() ->
    {ok, Modules} = application:get_env(htoad, modules),
    [ ok = seresye:add_rules(htoad_engine, Module) || Module <- Modules ],
    seresye:assert(htoad_engine, #init{}).
