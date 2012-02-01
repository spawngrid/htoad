-module(htoad_host).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/2]).
-rules([init]).

init(Engine, #init{}) ->
    {OsFamily, OsName} = os:type(),
    OsVersion = os:version(),
    Engine1 = seresye_engine:assert(Engine, [#host{ name = hostname() },
                                             {operating_system_type, OsFamily},
                                             {operating_system_name, OsName},
                                             {operating_system_version, OsVersion}]),
    lager:debug("Initialized htoad_host"),
    Engine1.

%% private

hostname() ->
    {Host, Domain} = {inet_db:gethostname(),inet_db:res_option(domain)},
    Host ++ "." ++ Domain.
