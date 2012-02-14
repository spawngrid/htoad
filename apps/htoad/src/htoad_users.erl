-module(htoad_users).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("kernel/include/file.hrl").

-rules([init, etc_passwd, etc_group,
        osx, osx_users, osx_groups]).

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

%% private

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


