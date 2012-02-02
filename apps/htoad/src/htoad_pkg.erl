-module(htoad_pkg).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/3, ensure_package/3, 
         package_not_present/3, package_present/2]).
-rules([init, ensure_package,
       package_not_present, package_present]).
                                         

init(Engine, #init{}, {operating_system_name, OsName}) ->
    PkgManager = pick_pkg_manager(OsName),
    lager:debug("Initialized htoad_pkg"),
    seresye_engine:assert(Engine, {package_manager, PkgManager}).
    

ensure_package(Engine, #package{ ensure = present } = Package, {package_manager, PkgManager}) ->
    pkg_manager_check(Engine, PkgManager, Package).

package_not_present(Engine, {package_check, 
                             #package{ ensure = present } = Package,
                             "missing"}, {package_manager, PkgManager}) ->
    lager:debug("Package ~s is not present",[format_package(Package)]),
    pkg_manager_install(Engine, PkgManager, Package).

package_present(Engine, {package_check, 
                         #package{ ensure = present } = Package,
                         "present"}) ->
    lager:debug("Package ~s is present, no action needed",[format_package(Package)]),
    Engine.

%% private
pick_pkg_manager(darwin) ->
    hd([ list_to_atom(Cmd) || Cmd <- ["brew","port"],
                              os:find_executable(Cmd) /= false ]).

-define(BREW_SHELL_CHECK(Package),
        #shell{ cmd = "([ -d `brew --prefix`/Cellar/" ++ Package ++ " ] "
                      "&& printf present) || printf missing" }).

-define(BREW_SHELL_INSTALL(Package),
        #shell{ cmd = "(brew install " ++ Package ++ " && printf installed) "
                      "|| printf not_installed" }).

pkg_manager_check(Engine, brew, #package{ version = undefined } = Package) ->
    lager:debug("Checking if package ~s has been already installed",[format_package(Package)]),
    Shell = ?BREW_SHELL_CHECK(Package#package.name),
    seresye_engine:assert(Engine, 
                          [
                           Shell,
                           htoad_utils:on({match, 
                                           [{{output, Shell, '$1'},
                                             [],
                                             ['$1']}]},
                                          {package_check, Package, '_'})
                           ]).

pkg_manager_install(Engine, brew, #package{ version = undefined } = Package) ->
    lager:debug("Installing package ~s",[format_package(Package)]),
    Shell = ?BREW_SHELL_INSTALL(Package#package.name),
    seresye_engine:assert(Engine, Shell).

format_package(#package{ name = Name, version = undefined }) ->                                      
    Name;
format_package(#package{ name = Name, version = Version}) ->
    io_lib:format("~s-~s",[Name, Version]).
