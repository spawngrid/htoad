-module(htoad_lfs).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/2, ensure_file_present/2, 
        ensure_dir_present/2]).
-rules([init, ensure_file_present,
       ensure_dir_present]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_lfs"),
    Engine.

ensure_file_present(Engine, #file{ ensure = present, 
                                   type = file 
                                 } = File) ->
    file:write_file(File#file.path, File#file.content),
    lager:debug("Ensured file ~s",[File#file.path]),
    chmod(File#file.path, File#file.mode),
    Engine.

ensure_dir_present(Engine, #file{ ensure = present,
                                 type = dir
                               } = Dir) ->
    ok = filelib:ensure_dir(Dir#file.path ++ "/"),
    lager:debug("Ensured directory ~s",[Dir#file.path]),
    chmod(Dir#file.path, Dir#file.mode),
    Engine.

%% private
chmod(Path, Mode) ->
    case Mode of
        undefined ->
            ok;
        _ ->
            file:change_mode(Path, Mode),
            lager:debug("Ensured ~s mode ~w",[Path, Mode])
    end.
