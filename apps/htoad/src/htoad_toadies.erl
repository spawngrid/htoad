-module(htoad_toadies).

-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("esupervisor/include/esupervisor.hrl").

-export([init/2, load_file/2, apply_command/3]).
-rules([init, load_file, apply_command]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_toadies"),
    Engine.

load_file(Engine, {load, File}) ->
    [ load(F) || F <- filelib:wildcard(htoad_utils:file(File)) ],
    Engine.


apply_command(Engine, #'htoad.toadie'{ server = Pid }, {htoad_command, apply}) ->
    gen_server:cast(Pid, apply),
    Engine.

%% private

load(File) ->
    lager:debug("Loading toadie ~s as requested",[File]),
    Spec = esupervisor:spec(#worker{
                               id = File,
                               modules = dynamic,
                               restart = permanent,
                               start_func = {htoad_toadie_server,
                                             start_link,
                                             [File]}}),
    supervisor:start_child(htoad_toadies, Spec).
    
