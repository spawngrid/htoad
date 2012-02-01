-module(htoad_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").


%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(_Args, Files) ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, [Files]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([Files]) ->
    #one_for_one{
      children = [
                  #worker{
                     id = htoad_engine,
                     restart = permanent,
                     start_func = {seresye, start, [htoad_engine]}
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
