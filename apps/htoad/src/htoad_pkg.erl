-module(htoad_pkg).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([ensure_package/3, 
         package_not_present/3, package_present/2]).
-export([format_package/1]).

-rules([package_present]).

package_present(Engine, {package_check, 
                         #package{ ensure = present } = Package,
                         "present"}) ->
    lager:debug("Package ~s is present, no action needed",[format_package(Package)]),
    Engine.

ensure_package(Engine, #package{ ensure = present } = Package, Command) ->
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

package_not_present(Engine, #package{} = Package, Command) ->
    lager:debug("Package ~s is absent, installing",[format_package(Package)]),
    htoad:assert(Engine, Command).

format_package(#package{ name = Name, version = undefined }) ->                                      
    Name;
format_package(#package{ name = Name, version = Version}) ->
    io_lib:format("~s-~s",[Name, Version]).
