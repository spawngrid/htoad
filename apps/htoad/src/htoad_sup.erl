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

start_link(Args, Files) ->
    Result = esupervisor:start_link({local, ?MODULE}, ?MODULE, [Args, Files]),
    init_engine(Args),
    Result.


start_seresye() ->
    seresye:start(?ENGINE).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([_Args, Files]) ->
    #one_for_one{
      children = [
                  #worker{
                     id = ?ENGINE,
                     restart = permanent,
                     start_func = {htoad_sup, start_seresye, []}
                    },
                  #one_for_one {
                           id = htoad_toadies,
                           registered = htoad_toadies,
                           children =
                               [ 
                                 #worker {
                                    id = File,
                                    modules = dynamic,
                                    restart = permanent,
                                    start_func = {htoad_toadie_server, start_link, [File]}
                                   } || File <- Files ]
                          }
                 ]
     }.


%% private

init_engine(Args) ->
    {ok, Modules} = application:get_env(htoad, modules),
    [ ok = seresye:add_rules(?ENGINE, Module) || Module <- Modules ],
    htoad:assert([{htoad_argument, Arg} || Arg <- Args ]),
    Signals = [ {htoad_toadie_server_ready, Pid} || {_, Pid, _, _} <- supervisor:which_children(htoad_toadies) ],
    htoad:assert(htoad_utils:on(Signals, #init{})).
