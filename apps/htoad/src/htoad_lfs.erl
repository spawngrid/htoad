-module(htoad_lfs).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("kernel/include/file.hrl").

-rules([init, 
        ensure_file_present, ensure_file_absent,
        ensure_dir_present, ensure_dir_absent,
        ensure_user_access, ensure_group_access,
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

ensure_file_absent(Engine, {ensure, absent,
                            #file{
                                   type = file
                                 } = File}) ->
    case filelib:is_dir(File#file.path) of
        true ->
            htoad:assert(Engine, #error_report{ rule = {?MODULE, ensure_file_absent},
                                                fact = {ensure, absent, File},
                                                reason = {error, is_dir}
                                                });
        false ->
            case file:delete(File#file.path) of
                ok ->
                    lager:debug("Deleted file ~s",[File#file.path]),
                    htoad:retract(Engine, File);
                {error, enoent} ->
                    lager:debug("File ~s is absent, no deletion necessary", [File#file.path]),
                    Engine;
                {error, Reason} ->
                    htoad:assert(Engine, #error_report{ rule = {?MODULE, ensure_file_absent},
                                                        fact = {ensure, absent, File},
                                                        reason = {error, Reason}
                                                      })
            end
    end.

ensure_dir_absent(Engine, {ensure, absent,
                            #file{
                                   type = dir
                                 } = File}) ->
    case filelib:is_regular(File#file.path) of
        true ->
            htoad:assert(Engine, #error_report{ rule = {?MODULE, ensure_dir_absent},
                                                fact = {ensure, absent, File},
                                                reason = {error, is_regular}
                                                });
        false ->
            case del_dir(File#file.path) of
                ok ->
                    lager:debug("Deleted directory ~s",[File#file.path]),
                    htoad:retract(Engine, File);
                {error, enoent} ->
                    lager:debug("Directory ~s is absent, no deletion necessary", [File#file.path]),
                    Engine;
                {error, Reason} ->
                    htoad:assert(Engine, #error_report{ rule = {?MODULE, ensure_dir_absent},
                                                        fact = {ensure, absent, File},
                                                        reason = {error, Reason}
                                                      })
            end
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


ensure_user_access(Engine, {ensure, present, 
                             #file{
                                    user = FileUser
                                  } = File}, #user{}=User, #group{}) ->
    case chown(File#file.path, FileUser, User) of
        ok -> Engine;
        {error, Reason} ->
            htoad:assert(Engine, #error_report{ rule = {?MODULE, ensure_user_access}, 
                                                fact = {ensure, present, File}, 
                                                reason = {error, Reason} })
    end.


ensure_group_access(Engine, {ensure, present, 
                             #file{
                                    group = FileGroup
                                  } = File}, #user{}, #group{}=Group) ->
    case chgrp(File#file.path, FileGroup, Group) of
        ok -> Engine;
        {error, Reason} ->
            htoad:assert(Engine, #error_report{ rule = {?MODULE, ensure_group_access}, 
                                                fact = {ensure, present, File}, 
                                                reason = {error, Reason} })
    end.


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
    file:change_owner(Path, Uid);
chown(_, _, #user{ id = _ }) ->
    ok.

chgrp(Path, FileGroup, Group) when is_list(FileGroup), is_record(Group, group) ->
    chgrp(Path, #group{ id = get_gid(FileGroup) }, Group);
chgrp(Path, FileGroup, Group) when is_integer(FileGroup), is_record(Group, group) ->
    chgrp(Path, #group{ id = FileGroup }, Group);
chgrp(_, #group{ id = Gid }, #group{ id = Gid }) ->
    ok;
chgrp(Path, #group{ id = Gid }=_FileGroup, #group{ id = _ }) ->
    lager:debug("Changed file ~s to gid ~w", [Path, Gid]),
    file:change_group(Path, Gid);
chgrp(_, _, #group{ id = _ }) ->
    ok.

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

del_dir(Path) ->
    case file:list_dir(Path) of
        {ok, Fs} ->
            [ begin
                  File = filename:join([Path, F]),
                  case filelib:is_regular(File) of
                      true ->
                          file:delete(File);
                      false ->
                          del_dir(File)
                  end
              end || F <- Fs ],
            file:del_dir(Path);
        Error ->
            Error
    end.
