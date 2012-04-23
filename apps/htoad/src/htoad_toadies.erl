-module(htoad_toadies).

-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("esupervisor/include/esupervisor.hrl").

-export([init/2, load_file/2, apply_command/4]).
-rules([init, load_file, apply_command]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_toadies"),
    Engine.

load_file(Engine, {load, File}) ->
    F = htoad_utils:file(File),
    case filelib:wildcard(F) of
      [] ->
          lager:error("File(s) ~p not found",[F]);
      Files ->
          [ load(Fi) || Fi <- Files ]
    end,
    Engine.

apply_command(Engine, #'htoad.toadie'{ server = Pid }, {htoad_command, apply}, #init{}) ->
    gen_server:cast(Pid, apply),
    Engine.

%% private

load(File) ->
    Spec = esupervisor:spec(#worker{
                               id = File,
                               modules = dynamic,
                               restart = transient,
                               start_func = {htoad_toadie_server,
                                             start_link,
                                             [File]}}),
    supervisor:start_child(htoad_toadies, Spec).
    
