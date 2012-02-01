-module(htoad_pkg).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/3, ensure_package/3]).
-rules([init, ensure_package]).

init(Engine, #init{}, {operating_system_name, OsName}) ->
    PkgManager = pick_pkg_manager(OsName),
    lager:debug("Initialized htoad_pkg"),
    seresye_engine:assert(Engine, {package_manager, PkgManager}).
    

ensure_package(Engine, #package{ ensure = present } = Package, {package_manager, PkgManager}) ->
    Engine1 = pkg_manager_install(Engine, PkgManager, Package),
    lager:debug("Package ~p has been scheduled for installation",[Package]),
    Engine1.


%% private
pick_pkg_manager(darwin) ->
    hd([ Cmd || Cmd <- ["brew","port"],
                os:find_executable(Cmd) /= false ]).

pkg_manager_install(Engine, "brew", #package{} = Package) ->
    seresye_engine:assert(Engine, #shell{ cmd = "brew install " ++ Package#package.name }).
