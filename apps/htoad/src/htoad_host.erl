-module(htoad_host).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-neg_rule({init, [{htoad_argument, {host, '__IGNORE_UNDERSCORE__'}}]}).

-export([init/2, init_with_hostname_overriden/3]).
-rules([init, init_with_hostname_overriden]).

init_with_hostname_overriden(Engine, #init{}, {htoad_argument, {host, Hostname}}) ->
    lager:debug("Overriding host name to ~s",[Hostname]),
    initialize(Engine, Hostname).
    
init(Engine, #init{}) when not {rule, [{htoad_argument, {host, _}}]} ->
    initialize(Engine, hostname()).

%% private

initialize(Engine, Hostname) ->
    {OsFamily, OsName} = os:type(),
    OsVersion = os:version(),
    Engine1 = seresye_engine:assert(Engine, [{host, Hostname},
                                             {operating_system_type, OsFamily},
                                             {operating_system_name, OsName},
                                             {operating_system_version, OsVersion}]),
    lager:debug("Initialized htoad_host"),
    Engine1.

hostname() ->
    {Host, Domain} = {inet_db:gethostname(),inet_db:res_option(domain)},
    Host ++ "." ++ Domain.
