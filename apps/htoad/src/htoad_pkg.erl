-module(htoad_pkg).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/3, init_linux/4, ensure_package/3, 
         package_not_present/3, package_present/2]).
-rules([init, init_linux, ensure_package,
        package_not_present, package_present]).

-neg_rule({init, [{linux_distribution, '__IGNORE_UNDERSCORE__'}]}).

init(Engine, #init{}, {operating_system_name, OsName}) when not {rule, [{linux_distribution, _}]} ->
    initialize(Engine, OsName).

init_linux(Engine, #init{}, {operating_system_name, linux}, {linux_distribution, Linux}) ->
    initialize(Engine, {linux, Linux}).

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
initialize(Engine, System) ->
    PkgManager = pick_pkg_manager(System),
    case PkgManager of
        unknown ->
            lager:debug("Initialized htoad_pkg (package manager has not been detected yet)", [PkgManager]),
            Engine;
        _ ->
            lager:debug("Initialized htoad_pkg (package manager: ~w)", [PkgManager]),
            htoad:assert(Engine, {package_manager, PkgManager})
    end.

pick_pkg_manager(darwin) ->
    hd([ list_to_atom(Cmd) || Cmd <- ["brew","port"],
                              os:find_executable(Cmd) /= false ]);
pick_pkg_manager({linux, "Ubuntu"}) ->
    apt;
pick_pkg_manager({linux, "RedHat"}) ->
    yum;
pick_pkg_manager({linux, "CentOS"}) ->
    yum;
pick_pkg_manager(_) ->
    unknown.


-define(BREW_SHELL_CHECK(Package),
        case Package of
            #package{ name = Name, version = undefined } ->
                #shell{ cmd = "([ -d `brew --prefix`/Cellar/" ++ Name ++ " ] "
                              "&& printf present) || printf missing" };
            #package{ name = Name, version = Version } ->
                #shell{ cmd = "([ -d `brew --prefix`/Cellar/" ++ Name ++ "/" ++ Version ++ " ] "
                        "&& printf present) || printf missing" }
        end).
                

-define(BREW_SHELL_INSTALL(Package),
        case Package of
            #package{ name = Name, version = undefined } ->
                #shell{ cmd = "(brew install " ++ Name ++ " && printf installed) "
                        "|| printf not_installed" };
            #package{ name = Name, version = _Version } ->
                #shell{ cmd = "(brew install --HEAD " ++ Name ++ " && printf installed) "
                        "|| printf not_installed" }
        end).

pkg_manager_check(Engine, brew, #package{} = Package) ->
    lager:debug("Checking if package ~s has been already installed",[format_package(Package)]),
    Shell = ?BREW_SHELL_CHECK(Package),
    htoad:assert(Engine, 
                 [
                  Shell,
                  htoad_utils:on({match, 
                                  [{{output, Shell, '$1'},
                                    [],
                                    ['$1']}]},
                                 {package_check, Package, '_'})
                 ]);
pkg_manager_check(Engine, _UnsupportedPkgMgr, _Package) ->
    Engine.

pkg_manager_install(Engine, brew, #package{} = Package) ->
    lager:debug("Installing package ~s",[format_package(Package)]),
    Shell = ?BREW_SHELL_INSTALL(Package),
    htoad:assert(Engine, Shell);
pkg_manager_install(Engine, _UnsupportedPkgMgr, _Package) ->
    Engine.

format_package(#package{ name = Name, version = undefined }) ->                                      
    Name;
format_package(#package{ name = Name, version = Version}) ->
    io_lib:format("~s-~s",[Name, Version]).
