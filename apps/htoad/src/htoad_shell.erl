-module(htoad_shell).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/3, command/2]).
-rules([init, command]).

init(Engine, #init{}, {operating_system_type, unix}) ->
    lager:debug("Initialized htoad_shell"),
    Engine.

command(Engine, #shell{ cmd = Cmd } = Shell) ->
    lager:debug("Executing shell command: ~s", [Cmd]),
    Result = os:cmd(Cmd),
    lager:debug("Shell output for `~s`: ~s", [Cmd, Result]),
    htoad:assert(Engine, {output, Shell, Result}).

