-module(htoad_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").


%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->
    #one_for_one{
      children = [
                  #worker{
                     id = htoad_engine,
                     restart = permanent,
                     start_func = {seresye, start, [htoad_engine]}
                    }
                 ]
     }.
