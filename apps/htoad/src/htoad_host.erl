-module(htoad_host).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-neg_rule({init, [{htoad_argument, {host, '__IGNORE_UNDERSCORE__'}}]}).

-rules([init, init_with_hostname_overriden, linux_ubuntu, linux_redhat]).

init_with_hostname_overriden(Engine, #init{}, {htoad_argument, {host, Hostname}}) ->
    initialize(Engine, Hostname).

init(Engine, #init{}) when not {rule, [{htoad_argument, {host, _}}]} ->
    initialize(Engine, hostname()).

linux_ubuntu(Engine, #file{ path="/etc/lsb-release", producer = fs, content = Content }, {operating_system_name, linux}) ->
    {match, [Dist]} = re:run(Content,".*DISTRIB_ID=(.*).*",[{capture,[1],list}]),
    lager:debug("Detected Linux ~s", [Dist]),
    htoad:assert(Engine, {linux_distribution, Dist}).


linux_redhat(Engine, #file{ path="/etc/redhat-release", producer = fs, content = Content }, {operating_system_name, linux}) ->
    {match, [Dist]} = re:run(Content,".*(CentOS|RedHat).*",[{capture,[1],list}]),
    lager:debug("Detected Linux ~s", [Dist]),
    htoad:assert(Engine, {linux_distribution, Dist}).


%% private

initialize(Engine, Hostname) ->
    {OsFamily, OsName} = os:type(),
    OsVersion = os:version(),
    Engine1 = htoad:assert(Engine, [{host, Hostname},
                                    {operating_system_type, OsFamily},
                                    {operating_system_name, OsName},
                                    {operating_system_version, OsVersion}]),
    lager:debug("Hostname: ~s",[Hostname]),
    lager:debug("Operating system type: ~p",[OsFamily]),
    lager:debug("Operating system name: ~p",[OsName]),
    lager:debug("Operating system version: ~p",[OsVersion]),
    lager:debug("Initialized htoad_host"),
    Engine1.

hostname() ->
    {Host, Domain} = {inet_db:gethostname(),inet_db:res_option(domain)},
    Host ++ "." ++ Domain.
