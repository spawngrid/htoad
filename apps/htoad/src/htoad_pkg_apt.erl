-module(htoad_pkg_apt).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-rules([init, ensure_package, package_not_present]).

init(Engine, #init{}, {operating_system_name, linux}, {linux_distribution, "Ubuntu"}) ->
    initialize(Engine);
init(Engine, #init{}, {operating_system_name, linux}, {linux_distribution, "Debian"}) ->
    initialize(Engine).

initialize(Engine) ->
    lager:debug("APT has been selected as a package manager"),
    htoad:assert(Engine, {package_manager, apt}).

-define(APT_SHELL_CHECK(Package),
        case Package of
            #package{ name = Name, version = undefined } ->
                #shell{ cmd = "([ `dpkg -s " ++ Name ++ " 1>/dev/null 2>/dev/null | grep Status | wc -l` = 0 ] && printf missing) || printf present"};
            #package{ name = Name, version = Version } ->
                #shell{ cmd = "([ `dpkg -s " ++ Name ++ " 1>/dev/null 2>/dev/null | grep Status | wc -l` = 0 ] && printf missing) || (([ -z `dpkg -s " ++ Name ++ " | grep Version | grep " ++ Version ++ " | wc -l` ] && printf missing) || printf present) "}
        end).

-define(APT_SHELL_INSTALL(Package),
        case Package of
            #package{ name = Name, version = undefined } ->
                #shell{ cmd = "(apt-get -y install " ++ Name ++ " && printf installed) "
                        "|| printf not_installed",
                        run_as = superuser };
            #package{ name = Name, version = Version } ->
                #shell{ cmd = "(apt-get -y install " ++ Name ++ " =" ++ Version ++ " && printf installed) "
                        "|| printf not_installed",
                        run_as = superuser }
        end).

ensure_package(Engine, #package{ ensure = present } = Package, {package_manager, apt}) ->
    htoad_pkg:ensure_package(Engine, Package, ?APT_SHELL_CHECK(Package)).

package_not_present(Engine, {package_check, 
                             #package{ ensure = present } = Package,
                             "missing"}, {package_manager, apt}) ->
    htoad_pkg:package_not_present(Engine, Package, ?APT_SHELL_INSTALL(Package)).
