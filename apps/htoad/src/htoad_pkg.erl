-module(htoad_pkg).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/3]).
-rules([init]).

init(Engine, #init{}, {operating_system_name, OsName}) ->
    PkgManager = pick_pkg_manager(OsName),
    lager:debug("Initialized htoad_pkg"),
    seresye_engine:assert(Engine, {package_manager, PkgManager}).
    

%% private
pick_pkg_manager(darwin) ->
    hd([ Cmd || Cmd <- ["brew","port"],
                os:find_executable(Cmd) /= false ]).
