-module(htoad_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

%% API
-export([start_link/2, start_seresye/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args, Files) ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, [Args, Files]).

start_seresye(Args) ->
    {ok, Pid} = seresye:start(?ENGINE),
    init_engine(Args),
    {ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([Args, Files]) ->
    #one_for_one{
      children = [
                  #worker{
                     id = ?ENGINE,
                     restart = permanent,
                     start_func = {htoad_sup, start_seresye, [Args]}
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

init_engine(Args) ->
    {ok, Modules} = application:get_env(htoad, modules),
    [ ok = seresye:add_rules(?ENGINE, Module) || Module <- Modules ],
    seresye:assert(?ENGINE, [{htoad_argument, Arg} || Arg <- Args ]),
    seresye:assert(?ENGINE, #init{}).
