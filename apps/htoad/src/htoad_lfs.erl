-module(htoad_lfs).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("kernel/include/file.hrl").

-rules([init, ensure_file_present, ensure_dir_present,
        fs_file_present, fs_file_absent, fs_dir_present, fs_dir_absent]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_lfs"),
    Engine.

ensure_file_present(Engine, #file{ ensure = present, 
                                   type = file,
                                   producer = undefined
                                 } = File) ->
    filelib:ensure_dir(File#file.path),
    file:write_file(File#file.path, File#file.content),
    lager:debug("Ensured file ~s",[File#file.path]),
    chmod(File#file.path, File#file.mode),
    Engine.

ensure_dir_present(Engine, #file{ ensure = present,
                                  type = dir,
                                  producer = undefined
                               } = Dir) ->
    ok = filelib:ensure_dir(Dir#file.path ++ "/"),
    lager:debug("Ensured directory ~s",[Dir#file.path]),
    chmod(Dir#file.path, Dir#file.mode),
    Engine.

%% file system
fs_file_present(Engine, {file_request, #file{ ensure = present,
                                              type = file,
                                              producer = fs
                                            } = File}) ->
    case filelib:is_regular(File#file.path) of
        true ->
            lager:debug("File ~s exists",[File#file.path]),
            htoad:assert(Engine, load_content(load_mode(File)));
        false ->
            Engine
    end.

fs_file_absent(Engine, {file_request, #file{ ensure = absent,
                                             type = file,
                                             producer = fs
                                           } = File}) ->
    case filelib:is_regular(File#file.path) of
        false ->
            lager:debug("File ~s does not exist",[File#file.path]),
            htoad:assert(Engine, File);
        true ->
            Engine
    end.

fs_dir_present(Engine, {file_request, #file{ ensure = present,
                                             type = dir,
                                             producer = fs
                                            } = File}) ->
    case filelib:is_dir(File#file.path) of
        true ->
            lager:debug("Directory ~s exists",[File#file.path]),
            htoad:assert(Engine, load_mode(File));
        false ->
            Engine
    end.

fs_dir_absent(Engine, {file_request, #file{ ensure = absent,
                                            type = dir,
                                            producer = fs
                                           } = File}) ->
    case filelib:is_dir(File#file.path) of
        false ->
            lager:debug("Directory ~s does not exist",[File#file.path]),
            htoad:assert(Engine, File);
        true ->
            Engine
    end.


%% private
chmod(Path, Mode) ->
    case Mode of
        undefined ->
            ok;
        _ ->
            file:change_mode(Path, Mode),
            lager:debug("Ensured ~s mode ~w",[Path, Mode])
    end.

load_mode(#file{} = File) ->
    {ok, #file_info{ mode = Mode }} = file:read_file_info(File#file.path),
    File#file{ mode = Mode }.
        
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


