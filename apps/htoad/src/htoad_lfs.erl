-module(htoad_lfs).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-include_lib("kernel/include/file.hrl").

-rules([init, ensure_file_present, ensure_dir_present,
        fs_file_present, fs_dir_present]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_lfs"),
    Engine.

ensure_file_present(Engine, {ensure, present, 
                             #file{
                                    type = file
                                  } = File}) ->
    filelib:ensure_dir(File#file.path),
    file:write_file(File#file.path, File#file.content),
    lager:debug("Ensured file ~s",[File#file.path]),
    chmod(File#file.path, File#file.mode),
    htoad:assert(Engine, File).

ensure_dir_present(Engine, {ensure, present, 
                            #file{ 
                                   type = dir
                                 } = Dir}) ->
    ok = filelib:ensure_dir(Dir#file.path ++ "/"),
    lager:debug("Ensured directory ~s",[Dir#file.path]),
    chmod(Dir#file.path, Dir#file.mode),
    htoad:assert(Engine, Dir).

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


