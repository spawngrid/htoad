-module(htoad_shell).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-rules([init, user, superuser, command_run_in_superuser,
        command_run_as_superuser, command]).

init(Engine, #init{}, {operating_system_type, unix}) ->
    lager:debug("Initialized htoad_shell"),
    htoad:assert(Engine, #shell{ cmd = "whoami" }).

user(Engine, {output, #shell{ cmd = "whoami"}, User}) ->
    lager:debug("Current user: ~s", [User]),
    htoad:assert(Engine, {user, User}).

superuser(Engine, #init{}, {operating_system_type, unix},
          {user, "root"}) ->
    htoad:assert(Engine, {?MODULE, superuser}).

command_run_in_superuser(Engine, #shell{ run_as = superuser } = Shell,
                         {?MODULE, superuser}) ->
    command(Engine, Shell).

command_run_as_superuser(Engine, #shell{ cmd = Cmd, run_as = superuser } = Shell,
                         #file{ path = "/usr/bin/sudo", producer = fs,
                                content = dontread }) when not {rule, [{?MODULE, superuser}]} ->
    lager:debug("Executing shell command as a super user (via sudo): ~s", [Cmd]),
    Result = string:strip(os:cmd("/usr/bin/sudo -n " ++ Cmd), right, $\n),
    lager:debug("Shell output for `~s` (sudo): ~s", [Cmd,Result]),
    htoad:assert(Engine, {output, Shell, Result}).


command(Engine, #shell{ cmd = Cmd, run_as = undefined } = Shell) ->
    lager:debug("Executing shell command: ~s", [Cmd]),
    Result = string:strip(os:cmd(Cmd), right, $\n),
    lager:debug("Shell output for `~s`: ~s", [Cmd, Result]),
    htoad:assert(Engine, {output, Shell, Result}).

