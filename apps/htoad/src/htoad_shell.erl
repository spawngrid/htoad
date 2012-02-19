-module(htoad_shell).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-define(REGEX, ".*uid=([^\(]*)\\(([^\)]*).*gid=([^\\(]*)\\(([^\)]*).*").

-rules([init, user, superuser, command_run_in_superuser,
        command_run_as_superuser, command]).

init(Engine, #init{}, {operating_system_type, unix}) ->
    lager:debug("Initialized htoad_shell"),
    htoad:assert(Engine, #shell{ cmd = "id" }).

user(Engine, {output, #shell{ cmd = "id" }, Result}, #user{ uid = Uid0 } = User0) ->
    {match, [Uid, User, Gid, Group]} = re:run(Result, ?REGEX, [{capture,[1,2,3,4],list}]),
    case list_to_integer(Uid) of
        Uid0 ->
            lager:debug("Current user: ~s, uid: ~s, group: ~s, gid: ~s", [User, Uid, Group, Gid]),
            htoad:assert(Engine, {current_user, User0});
        _ ->
            Engine
    end.

superuser(Engine, #init{}, {operating_system_type, unix},
          {current_user, #user{ name = "root" }}) ->
    htoad:assert(Engine, {?MODULE, superuser}).

command_run_in_superuser(Engine, #shell{ run_as = superuser } = Shell,
                         {?MODULE, superuser}) ->
    command(Engine, Shell).

command_run_as_superuser(Engine, #shell{ cmd = Cmd, run_as = superuser } = Shell,
                         #file{ path = "/usr/bin/sudo",
                                content = dontread }) when not {rule, [{?MODULE, superuser}]} ->
    lager:debug("Executing shell command as a super user (via sudo): ~s", [Cmd]),
    {Code, Output} = htoad_shell_server:start(Shell#shell{ cmd = "/usr/bin/sudo -n " ++ Cmd }),
    Result = adjust_output(Output, Shell),
    lager:debug("Shell output for `~s` (sudo) (exit code ~w): ~s", [Cmd,Code,Result]),
    htoad:assert(Engine, [{exit_status, Shell, Code}, {output, Shell, Result}]).


command(Engine, #shell{ cmd = Cmd, run_as = _ } = Shell) ->
    lager:debug("Executing shell command: ~s", [Cmd]),
    {Code, Output} = htoad_shell_server:start(Shell),
    Result = adjust_output(Output, Shell),
    lager:debug("Shell output for `~s` (exit code ~w): ~s", [Cmd,Code,Result]),
    htoad:assert(Engine, [{exit_status, Shell, Code}, {output, Shell, Result}]).


%% private

adjust_output(Output, #shell{ strip_newline = true }) ->
    string:strip(Output, right, $\n);
adjust_output(Output, _) ->
    Output.
