-module(htoad_lfs).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("kernel/include/file.hrl").

-rules([init, 
        ensure_file_present, ensure_dir_present, ensure_path_access,
        fs_file_present, fs_dir_present]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_lfs"),
    Engine.

ensure_file_present(Engine, {ensure, present, 
                             #file{
                                    type = file
                                  } = File}) ->

    case file:write_file(File#file.path, File#file.content) of
        ok ->
            lager:debug("Ensured file ~s",[File#file.path]),
            chmod(File#file.path, File#file.mode),
            htoad:assert(Engine, File);
        {error, Reason} ->
            htoad:assert(Engine, #error_report{ rule = {?MODULE, ensure_file_present}, 
                                                fact = {ensure, present, File}, 
                                                reason = {error, Reason} })
    end.


ensure_dir_present(Engine, {ensure, present, 
                            #file{ 
                                   type = dir
                                 } = Dir}) ->
    case filelib:ensure_dir(Dir#file.path ++ "/") of
        ok ->
            lager:debug("Ensured directory ~s",[Dir#file.path]),
            chmod(Dir#file.path, Dir#file.mode),
            htoad:assert(Engine, Dir);
        {error, Reason} ->
            htoad:assert(Engine, #error_report{ rule = {?MODULE, ensure_dir_present}, 
                                                fact = {ensure, present, Dir}, 
                                                reason = {error, Reason} })
    end.


ensure_path_access(Engine, {ensure, present, 
                             #file{
                                    user = FileUser,
                                    group = FileGroup
                                  } = File}, #user{}=User, 
                                             #group{}=Group) ->
    lager:debug("Ensured access ~s",[File#file.path]),
    chown(File#file.path, FileUser, User),
    chgrp(File#file.path, FileGroup, Group),
    htoad:assert(Engine, File).

%% file system
fs_file_present(Engine, {file_request, #file{
                                              type = file
                                            } = File}) ->
    case filelib:is_regular(File#file.path) of
        true ->
            lager:debug("File ~s exists",[File#file.path]),
            htoad:assert(Engine, load_content(load_mode(File)));
        false ->
            Engine
    end.

fs_dir_present(Engine, {file_request, #file{
                                             type = dir
                                            } = File}) ->
    case filelib:is_dir(File#file.path) of
        true ->
            lager:debug("Directory ~s exists",[File#file.path]),
            htoad:assert(Engine, load_mode(File));
        false ->
            Engine
    end.


%% private

chown(Path, FileUser, User) when is_list(FileUser), is_record(User, user) ->
    chown(Path, #user{ id = get_uid(FileUser) }, User);
chown(Path, FileUser, User) when is_integer(FileUser), is_record(User, user) ->
    chown(Path, #user{ id = FileUser }, User);
chown(_, #user{ id = Uid }, #user{ id = Uid }) ->
    ok;
chown(Path, #user{ id = Uid }=_FileUser, #user{ id = _ }) ->
    lager:debug("Changed file ~s to uid ~w", [Path, Uid]),
    file:change_owner(Path, Uid).

chgrp(Path, FileGroup, Group) when is_list(FileGroup), is_record(Group, group) ->
    chgrp(Path, #group{ id = get_gid(FileGroup) }, Group);
chgrp(Path, FileGroup, Group) when is_integer(FileGroup), is_record(Group, group) ->
    chgrp(Path, #group{ id = FileGroup }, Group);
chgrp(_, #group{ id = Gid }, #group{ id = Gid }) ->
    ok;
chgrp(Path, #group{ id = Gid }=_FileGroup, #group{ id = _ }) ->
    lager:debug("Changed file ~s to gid ~w", [Path, Gid]),
    file:change_group(Path, Gid).

get_uid(User) when is_list(User) ->
    Uid = string:strip(os:cmd("id -u " ++ User), right, $\n),
    list_to_integer(Uid).

get_gid(Group) when is_list(Group) ->
    Gid = string:strip(os:cmd("id -g " ++ Group), right, $\n),
    list_to_integer(Gid).

chmod(Path, Mode) ->
    case Mode of
        undefined ->
            ok;
        _ ->
            file:change_mode(Path, Mode),
            lager:debug("Ensured ~s mode ~w",[Path, Mode])
    end.

load_mode(#file{} = File) ->
    {ok, #file_info{ mode = Mode, gid = Gid, uid = Uid }} = file:read_file_info(File#file.path),
    File#file{ mode = Mode, user = #user{ id = Uid }, group = #group{ id = Gid } }.
        
load_content(#file{ content = dontread } = File) ->
    File;
load_content(#file{ content = "" } = File) ->
    {ok, B} = file:read_file(File#file.path),
    File#file{ content = B };
load_content(#file{ content = Content } = File) when is_list(Content);
                                                     is_binary(Content) ->
    {ok, B} = file:read_file(File#file.path),
    case iolist_to_binary(Content) of
        B ->
            File;
        _ ->
            []
    end.


