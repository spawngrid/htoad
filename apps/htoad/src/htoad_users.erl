-module(htoad_users).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("kernel/include/file.hrl").

-rules([init, etc_passwd, etc_group, 
        ensure_user_present, ensure_user_absent,
        ensure_group_present, ensure_group_absent]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_users"),
    Engine.

etc_passwd(Engine, #file{ path = "/etc/passwd", content = Content }, {operating_system_type, unix})
  when not {rule, [{operating_system_name, darwin}]} ->
    lager:debug("Using /etc/passwd to fetch accounts"),
    Lines = re:split(Content,"\n",[{return, list}]),
    parse_lines(Engine, Lines).

etc_group(Engine, #file{ path = "/etc/group", content = Content }, {operating_system_type, unix})
  when not {rule, [{operating_system_name, darwin}]} ->
    lager:debug("Using /etc/groups to fetch groups"),
    Lines = re:split(Content,"\n",[{return, list}]),
    parse_lines(Engine, Lines).

ensure_user_absent(Engine,
                    #file{ path = "/usr/sbin/userdel", content = dontread },
                    {ensure, absent, #user{ name = Name } = User}, 
                    {operating_system_name, linux}) ->
    lager:debug("Deleting user ~s",[Name]),
    Command = #shell{ cmd = "/usr/sbin/userdel " ++ Name },
    Retractions = [ Rule || {file_request, #file{ path = "/etc/passwd" }} = Rule <- seresye_engine:get_kb(Engine) ],
    UserRetractions = [ Rule || #user{ name = UserName } = Rule <- seresye_engine:get_kb(Engine), UserName == Name ],
    REngine = htoad:retract(Engine, Retractions),
    htoad:assert(REngine, [Command,
                          htoad_utils:on({exit_status, Command, 0},
                                         [{retract, UserRetractions},
                                          {file_request, #file{ path = "/etc/passwd" }}]),
                          htoad_utils:on({exit_status, Command, 1},
                                         #error_report{ fact = {ensure, absent, User},
                                                        rule = {?MODULE, ensure_user_absent},
                                                        reason = "Can't update password file"
                                                      }),
                          htoad_utils:on({exit_status, Command, 2},
                                         #error_report{ fact = {ensure, absent, User},
                                                        rule = {?MODULE, ensure_user_absent},
                                                        reason = "Invalid command syntax"
                                                      }),
                          %% we skip 6 since it is "Specified user doesn't exist", which is what we want
                          htoad_utils:on({exit_status, Command, 8},
                                         #error_report{ fact = {ensure, absent, User},
                                                        rule = {?MODULE, ensure_user_absent},
                                                        reason = "User currently logged in"
                                                      }),
                          htoad_utils:on({exit_status, Command, 10},
                                         #error_report{ fact = {ensure, absent, User},
                                                        rule = {?MODULE, ensure_user_absent},
                                                        reason = "Can't update group file"
                                                      }),
                          htoad_utils:on({exit_status, Command, 12},
                                         #error_report{ fact = {ensure, absent, User},
                                                        rule = {?MODULE, ensure_user_absent},
                                                        reason = "Can't remove home directory"
                                                      })
                           ]).

ensure_user_present(Engine, 
                    #file{ path = "/usr/sbin/useradd", content = dontread },
                    {ensure, present, #user{ name = Name } = User}, 
                    {operating_system_name, linux}) ->
    lager:debug("Creating user ~s",[Name]),
    Options = useradd_options(User),
    Command = #shell{ cmd = "/usr/sbin/useradd " ++ Options ++ " " ++ Name },
    Retractions = [ Rule || {file_request, #file{ path = "/etc/passwd" }} = Rule <- seresye_engine:get_kb(Engine) ],
    REngine = htoad:retract(Engine, Retractions),
    htoad:assert(REngine, [Command,
                          htoad_utils:on({exit_status, Command, 0},
                                         {file_request, #file{ path = "/etc/passwd" }}),
                          htoad_utils:on({exit_status, Command, 1},
                                         #error_report{ fact = {ensure, present, User},
                                                        rule = {?MODULE, ensure_user_present},
                                                        reason = "Can't update password file"
                                                      }),
                          htoad_utils:on({exit_status, Command, 2},
                                         #error_report{ fact = {ensure, present, User},
                                                        rule = {?MODULE, ensure_user_present},
                                                        reason = "Invalid command syntax"
                                                      }),
                          htoad_utils:on({exit_status, Command, 3},
                                         #error_report{ fact = {ensure, present, User},
                                                        rule = {?MODULE, ensure_user_present},
                                                        reason = "Invalid command option"
                                                      }),
                          htoad_utils:on({exit_status, Command, 4},
                                         #error_report{ fact = {ensure, present, User},
                                                        rule = {?MODULE, ensure_user_present},
                                                        reason = "UID already in use"
                                                      }),
                          htoad_utils:on({exit_status, Command, 6},
                                         #error_report{ fact = {ensure, present, User},
                                                        rule = {?MODULE, ensure_user_present},
                                                        reason = "Specified group does not exist"
                                                      }),
                          %% skip 9 because it is 'username already in use', which is fine
                          htoad_utils:on({exit_status, Command, 10},
                                         #error_report{ fact = {ensure, present, User},
                                                        rule = {?MODULE, ensure_user_present},
                                                        reason = "Can't update group file"
                                                      }),
                          htoad_utils:on({exit_status, Command, 12},
                                         #error_report{ fact = {ensure, present, User},
                                                        rule = {?MODULE, ensure_user_present},
                                                        reason = "Can't create home directory"
                                                      }),
                          htoad_utils:on({exit_status, Command, 13},
                                         #error_report{ fact = {ensure, present, User},
                                                        rule = {?MODULE, ensure_user_present},
                                                        reason = "Can't create mail spool"
                                                      })

                          ]).

ensure_group_absent(Engine,
                    #file{ path = "/usr/sbin/groupdel", content = dontread },
                    {ensure, absent, #group{ name = Name } = Group}, 
                    {operating_system_name, linux}) ->
    lager:debug("Deleting group ~s",[Name]),
    Command = #shell{ cmd = "/usr/sbin/groupdel " ++ Name },
    Retractions = [ Rule || {file_request, #file{ path = "/etc/group" }} = Rule <- seresye_engine:get_kb(Engine) ],
    GroupRetractions = [ Rule || #group{ name = GroupName } = Rule <- seresye_engine:get_kb(Engine), GroupName == Name ],
    REngine = htoad:retract(Engine, Retractions),
    htoad:assert(REngine, [Command,
                          htoad_utils:on({exit_status, Command, 0},
                                         [{retract, GroupRetractions},
                                          {file_request, #file{ path = "/etc/group" }}]),
                          htoad_utils:on({exit_status, Command, 2},
                                         #error_report{ fact = {ensure, absent, Group},
                                                        rule = {?MODULE, ensure_group_absent},
                                                        reason = "Invalid command syntax"
                                                      }),
                          %% we skip 6 since it is "Specified group doesn't exist", which is what we want
                          htoad_utils:on({exit_status, Command, 8},
                                         #error_report{ fact = {ensure, absent, Group},
                                                        rule = {?MODULE, ensure_group_absent},
                                                        reason = "Can't remove user's primary group"
                                                      }),
                          htoad_utils:on({exit_status, Command, 10},
                                         #error_report{ fact = {ensure, absent, Group},
                                                        rule = {?MODULE, ensure_group_absent},
                                                        reason = "Can't update group file"
                                                      })
                           ]).


ensure_group_present(Engine, 
                    #file{ path = "/usr/sbin/groupadd", content = dontread },
                    {ensure, present, #group{ name = Name } = Group}, 
                    {operating_system_name, linux}) ->
    lager:debug("Creating group ~s",[Name]),
    Options = groupadd_options(Group),
    Command = #shell{ cmd = "/usr/sbin/groupadd " ++ Options ++ " " ++ Name },
    Retractions = [ Rule || {file_request, #file{ path = "/etc/group" }} = Rule <- seresye_engine:get_kb(Engine) ],
    REngine = htoad:retract(Engine, Retractions),
    htoad:assert(REngine, [Command,
                          htoad_utils:on({exit_status, Command, 0},
                                         {file_request, #file{ path = "/etc/group" }}),
                          htoad_utils:on({exit_status, Command, 2},
                                         #error_report{ fact = {ensure, present, Group},
                                                        rule = {?MODULE, ensure_group_present},
                                                        reason = "Invalid command syntax"
                                                      }),
                          htoad_utils:on({exit_status, Command, 3},
                                         #error_report{ fact = {ensure, present, Group},
                                                        rule = {?MODULE, ensure_group_present},
                                                        reason = "Invalid command option"
                                                      }),
                          htoad_utils:on({exit_status, Command, 4},
                                         #error_report{ fact = {ensure, present, Group},
                                                        rule = {?MODULE, ensure_group_present},
                                                        reason = "GID already in use"
                                                      }),
                          %% skip 9 because it is 'group name is not unique', which is fine
                          htoad_utils:on({exit_status, Command, 10},
                                         #error_report{ fact = {ensure, present, Group},
                                                        rule = {?MODULE, ensure_group_present},
                                                        reason = "Can't update group file"
                                                      })
                          ]).

%% private

useradd_options(#user{} = User) ->
    re:replace(string:join([
    case User#user.home of undefined -> []; Home -> "--home " ++ Home end,
    case User#user.shell of undefined -> []; Shell -> "--shell " ++ Shell end,
    case User#user.gid of 
        undefined -> []; 
        Gid when is_integer(Gid) -> "--gid " ++ integer_to_list(Gid);
        Gid when is_list(Gid) -> "--gid " ++ Gid 
    end,
    case User#user.uid of undefined -> []; Uid -> "--uid " ++ integer_to_list(Uid) end,
    case User#user.password of undefined -> []; Password -> 
            lager:warning("Use of --password option for useradd is not recommended"), 
            "--password " ++ Password end,
    case User#user.comment of undefined -> []; Comment -> "--comment '" ++ Comment ++ "'" end
                ], " "),"\s+"," ",[{return,list},global]).

groupadd_options(#group{} = Group) ->
    re:replace(string:join([
    case Group#group.gid of undefined -> []; Gid -> "--gid " ++ integer_to_list(Gid) end,
    case Group#group.password of undefined -> []; Password -> 
            lager:warning("Use of --password option for groupadd is not recommended"), 
            "--password " ++ Password end
    ], " "),"\s+"," ",[{return,list},global]).

parse_lines(Engine, Lines) ->
    lists:foldl(fun(Line, NewEngine) ->
                        case parse_line(Line) of
                            {comment, ignore} ->
                                NewEngine;
                            {data, [Name, Password, Gid, Users]} ->
                                htoad:assert(NewEngine,
                                             #group{
                                               name = Name,
                                               password = Password,
                                               gid = list_to_integer(Gid),
                                               users = string:tokens(Users, ",")
                                              });
                            {data, [Name, Password, Uid, Gid, Gecos, Home, Shell]} ->
                                htoad:assert(NewEngine,
                                             #user{
                                               name = Name,
                                               password = Password,
                                               uid = list_to_integer(Uid),
                                               gid = list_to_integer(Gid),
                                               gecos = Gecos,
                                               home = Home,
                                               shell = Shell
                                              });
                            {data, [Name, Password, Uid, Gid, Quota, Comment, Gecos, Home, Shell]} ->
                                htoad:assert(NewEngine,
                                             #user{
                                               name = Name,
                                               password = Password,
                                               uid = list_to_integer(Uid),
                                               gid = list_to_integer(Gid),
                                               quota = Quota,
                                               comment = Comment,
                                               gecos = Gecos,
                                               home = Home,
                                               shell = Shell
                                              });
                            _ ->
                                NewEngine
                        end
                end, Engine, Lines).
                                               
parse_line("#" ++ _) ->
    {comment, ignore};
parse_line(Line) -> 
    {data, re:split(Line, ":", [{return, list}])}.


