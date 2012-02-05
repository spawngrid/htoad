-module(htoad_pkg_brew).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-rules([init, ensure_package, package_not_present]).

init(Engine, #init{}, {operating_system_name, darwin}) ->
    case os:find_executable("brew") of
        false ->
            Engine;
        _Brew ->
            lager:debug("Homebrew has been selected as a package manager"),
            htoad:assert(Engine, {package_manager, brew})
    end.

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
                #shell{ cmd = "(brew install " ++ Name ++ " 1>/dev/null 2>/dev/null && printf installed) "
                        "|| printf not_installed" };
            #package{ name = Name, version = _Version } ->
                #shell{ cmd = "(brew install --HEAD " ++ Name ++ " 1>/dev/null 2>/dev/null && printf installed) "
                        "|| printf not_installed" }
        end).

ensure_package(Engine, {ensure, present, #package{} = Package}, {package_manager, brew}) ->
    htoad_pkg:ensure_package(Engine, Package, ?BREW_SHELL_CHECK(Package)).

package_not_present(Engine, {package_check, 
                             #package{} = Package,
                             "missing"}, {package_manager, brew}) ->
    htoad_pkg:package_not_present(Engine, Package, ?BREW_SHELL_INSTALL(Package)).
