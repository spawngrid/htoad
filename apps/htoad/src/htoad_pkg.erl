-module(htoad_pkg).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([ensure_package/3, remove_package/3, package_not_present/3, package_present/3]).
-export([format_package/1]).

-rules([package_present]).

package_present(Engine, {package_check, 
                         #package{} = Package,
                         "present"}) ->
    lager:debug("Package ~s is present, no action needed",[format_package(Package)]),
    Engine.

ensure_package(Engine, #package{} = Package, Command) ->
    lager:debug("Checking if package ~s is present",[format_package(Package)]),
    htoad:assert(Engine, 
                 [
                  Command,
                  htoad_utils:on({match, 
                                  [{{output, Command, '$1'},
                                    [],
                                    ['$1']}]},
                                 {package_check, Package, '_'})
                 ]).

remove_package(Engine, #package{} = Package, Command) ->
    lager:debug("Checking if package ~s should be removed",[format_package(Package)]),
    htoad:assert(Engine,
                 [
                  Command,
                  htoad_utils:on({match,
                                  [{{output, Command, '$1'},
                                    [],
                                    ['$1']}]},
                                 {package_remove, Package, '_'})
                 ]).

package_not_present(Engine, #package{} = Package, Command) ->
    lager:debug("Package ~s is absent, installing",[format_package(Package)]),
    htoad:assert(Engine, [Command, htoad_utils:on({match, 
                                                   [{{output, Command, "installed"},
                                                     [],
                                                     [true]}]}, Package)]).

package_present(Engine, #package{} = Package, Command) ->
    lager:debug("Package ~s is present, removing",[format_package(Package)]),
    htoad:assert(Engine, [Command, htoad_utils:on({match,
                                                   [{{output, Command, "removed"},
                                                     [],
                                                     [true]}]}, Package)]).

format_package(#package{ name = Name, version = undefined }) ->                                      
    Name;
format_package(#package{ name = Name, version = Version}) ->
    io_lib:format("~s-~s",[Name, Version]).
