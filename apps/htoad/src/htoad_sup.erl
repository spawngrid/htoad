-module(htoad_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

-include_lib("htoad/include/htoad.hrl").

%% API
-export([start_link/1, start_seresye/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

start_seresye() ->
    {ok, Pid} = htoad_engine:start(?ENGINE),
    htoad_engine:set_hooks(?ENGINE,[{before_rule, fun htoad_trace:rule/3}]),
    {ok, Pid}.
       

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([_Args]) ->
    #one_for_one{
      children = [
                  #worker{
                     id = ?ENGINE,
                     restart = permanent,
                     start_func = {htoad_sup, start_seresye, []}
                    },
                  #one_for_one {
                           id = htoad_toadies,
                           registered = htoad_toadies
                          }
                 ]
     }.
