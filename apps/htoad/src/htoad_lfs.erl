-module(htoad_lfs).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("kernel/include/file.hrl").

-rules([init, 
        ensure_file_present, ensure_file_absent,
        ensure_dir_present, ensure_dir_absent,
        ensure_user_access_by_name, ensure_user_access_by_uid,
        ensure_group_access_by_name, ensure_group_access_by_gid,
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


ensure_user_access_by_name(Engine, {ensure, present, 
                                    #file{
                                           path = Path,
                                           type = Type,
                                           user = FileUserName
                                         } = File},
                           {current_user, User}, 
                           #file{
                                  path = Path,
                                  type = Type
                                },
                           #user{ name = FileUserName } = FU) ->
    ensure_user_access(Engine, File, FU, User).

ensure_user_access_by_uid(Engine, {ensure, present, 
                                   #file{
                                          path = Path,
                                          type = Type,
                                          user = FileUserId
                                        } = File}, 
                          {current_user, User},
                          #file{
                                  path = Path,
                                  type = Type
                                },
                          #user{ uid = FileUserId } = FU) ->
    ensure_user_access(Engine, File, FU, User).


ensure_user_access(Engine, File, FileUser, User) ->
    case chown(File#file.path, FileUser, User) of
        ok -> Engine;
        {error, Reason} ->
            htoad:assert(Engine, #error_report{ rule = {?MODULE, ensure_user_access}, 
                                                fact = {ensure, present, File}, 
                                                reason = {error, Reason} })
    end.


ensure_group_access_by_name(Engine, {ensure, present, 
                                     #file{
                                            path = Path,
                                            type = Type,
                                            group = FileGroup
                                          } = File}, 
                            {current_user, #user{ group = Gid }}, 
                            #file{
                                   path = Path,
                                   type = Type
                                 },
                            #group{ gid = Gid } = Group, 
                            #group{ name = FileGroup } = FG) ->
    ensure_group_access(Engine, File, FG, Group).

ensure_group_access_by_gid(Engine, {ensure, present, 
                                    #file{
                                          path = Path,
                                          type = Type,
                                           group = FileGroup
                                         } = File}, 
                           {current_user, #user{ group = Gid }}, 
                           #file{
                             path = Path,
                             type = Type
                            },
                           #group{ gid = Gid } = Group, 
                           #group{ gid = FileGroup } = FG) ->
    ensure_group_access(Engine, File, FG, Group).


ensure_group_access(Engine, File, FileGroup, Group) ->
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

chown(_, #user{ uid = Uid }, #user{ uid = Uid }) ->
    ok;
chown(Path, #user{ uid = Uid }=_FileUser, #user{ uid = _ }) ->
    lager:debug("Changed file ~s to uid ~w", [Path, Uid]),
    file:change_owner(Path, Uid);
chown(_, _, #user{ uid = _ }) ->
    ok.

chgrp(_, #group{ gid = Gid }, #group{ gid = Gid }) ->
    ok;
chgrp(Path, #group{ gid = Gid }=_FileGroup, #group{ gid = _ }) ->
    lager:debug("Changed file ~s to gid ~w", [Path, Gid]),
    file:change_group(Path, Gid);
chgrp(_, _, #group{ gid = _ }) ->
    ok.

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
    File#file{ mode = Mode, user = #user{ uid = Uid }, group = #group{ gid = Gid } }.
        
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
