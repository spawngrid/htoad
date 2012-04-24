-module(htoad_sup).
-behaviour(esupervisor).
-include_lib("esupervisor/include/esupervisor.hrl").

-include_lib("htoad/include/htoad.hrl").

%% API
-export([start_link/1, start_seresye/0, start_trace/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
    esupervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

start_seresye() ->
    {ok, EngineDump} = application:get_env(htoad, engine_dump), 
    EngineFilename = EngineDump ++ "." ++ atom_to_list(node()),
    case filelib:is_regular(EngineFilename) of
      true ->
         lager:debug("Restoring engine from ~s", [EngineFilename]),
         {ok, B} = file:read_file(EngineFilename),
         {ok, Pid} = seresye:start(?ENGINE, binary_to_term(B));
      false ->
          {ok, Pid} = seresye:start(?ENGINE)
    end,
    seresye:set_hooks(?ENGINE,[{before_rule, fun htoad_trace:rule/3}]),
    {ok, Pid}.

start_trace() ->
    {ok, Pid} = gen_event:start_link({local, htoad_trace}),
    case application:get_env(htoad, trace) of
        {ok, TraceModules} ->
            ok;
        _ ->
            TraceModules = []
    end,
    [ gen_event:add_handler(htoad_trace, Module, Args) || {Module, Args} <- TraceModules ],
    {ok, Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([_Args]) ->
    #one_for_one{
      children = [
                  #worker{
                     id = htoad_file_server,
                     restart = permanent
                  },
                  #worker{
                     id = htoad_trace,
                     restart = permanent,
                     start_func = {?MODULE, start_trace, []},
                     modules = dynamic
                    },
                  #worker{
                     id = ?ENGINE,
                     restart = permanent,
                     start_func = {?MODULE, start_seresye, []}
                    },
                  #simple_one_for_one {
                           id = htoad_shell,
                           registered = htoad_shell,
                           children = [
                                       #worker{
                                          id = htoad_shell_server,
                                          restart = transient,
                                          start_func = {htoad_shell_server, start_link, []},
                                          modules = [htoad_shell_server]
                                         }
                                      ]
                          },
                  #one_for_one {
                           id = htoad_toadies,
                           registered = htoad_toadies
                          }
                 ]
     }.
