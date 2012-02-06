-module(htoad_pkg_yum).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-rules([init, ensure_package, remove_package, package_not_present, package_present]).

init(Engine, #init{}, {operating_system_name, linux}, {linux_distribution, "RedHat"}) ->
    initialize(Engine);
init(Engine, #init{}, {operating_system_name, linux}, {linux_distribution, "CentOS"}) ->
    initialize(Engine).

initialize(Engine) ->
    lager:info("YUM has been selected as a package manager"),
    htoad:assert(Engine, {package_manager, yum}).


-define(YUM_SHELL_CHECK(Package),
        case Package of
            #package{ name = Name, version = undefined } ->
                #shell{ cmd = "yum -q list installed " ++ Name ++ 
                              " 1>/dev/null 2>/dev/null && printf present || printf missing"};
            #package{ name = Name, version = Version } ->
                #shell{ cmd = "yum -q list installed " ++ Name ++ "-" ++ Version ++ 
                              " 1>/dev/null 2>/dev/null && printf present || printf missing"}
        end).

-define(YUM_SHELL_INSTALL(Package),
        case Package of
            #package{ name = Name, version = undefined } ->
                #shell{ cmd = "yum -y install " ++ Name ++ 
                              " 1>/dev/null 2>/dev/null && printf installed || printf not_installed",
                        run_as = superuser };
            #package{ name = Name, version = Version } ->
                #shell{ cmd = "yum -y install " ++ Name ++ "-" ++ Version ++ 
                              " 1>/dev/null 2>/dev/null && printf installed || printf not_installed",
                        run_as = superuser }
        end).

-define(YUM_SHELL_REMOVE(Package),
        case Package of
            #package{ name = Name, version = undefined } ->
                #shell{ cmd = "yum -y remove " ++ Name ++
                              " 1>/dev/null 2>/dev/null && printf removed || printf not_removed",
                        run_as = superuser };
            #package{ name = Name, version = Version } ->
                #shell{ cmd = "yum -y remove " ++ Name ++ "-" ++ Version ++
                              " 1>/dev/null 2>/dev/null && printf removed || printf not_removed",
                        run_as = superuser }
        end).

ensure_package(Engine, {ensure, present, #package{} = Package}, {package_manager, yum}) ->
    htoad_pkg:ensure_package(Engine, Package, ?YUM_SHELL_CHECK(Package)).

remove_package(Engine, {ensure, absent, #package{} = Package}, {package_manager, yum}) ->
    htoad_pkg:remove_package(Engine, Package, ?YUM_SHELL_CHECK(Package)).

package_not_present(Engine, {package_check, 
                             #package{} = Package,
                             "missing"}, {package_manager, yum}) ->
    htoad_pkg:package_not_present(Engine, Package, ?YUM_SHELL_INSTALL(Package)).

package_present(Engine, {package_remove,
                             #package{} = Package,
                             "present"}, {package_manager, yum}) ->
    htoad_pkg:package_present(Engine, Package, ?YUM_SHELL_REMOVE(Package)).
