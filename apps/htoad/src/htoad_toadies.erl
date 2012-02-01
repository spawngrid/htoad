-module(htoad_toadies).

-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("esupervisor/include/esupervisor.hrl").

-export([init/2, load_file/2]).
-rules([init, load_file]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_toadies"),
    Engine.

load_file(Engine, {load, File}) ->
    [ load(F) || F <- filelib:wildcard(filename:join([os:getenv("HTOAD_CWD"), File])) ],
    Engine.


load(File) ->
    lager:debug("Starting worker for ~s module",[File]),
    Spec = esupervisor:spec(#worker{
                               id = File,
                               modules = dynamic,
                               restart = permanent,
                               start_func = {htoad_module_server,
                                             start_link,
                                             [File]}}),
    supervisor:start_child(htoad_modules, Spec).
    
