-module(htoad_htoad).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/2]).
-rules([init]).

init(Engine, #init{}) ->
    Engine1 = htoad:assert(Engine, #htoad{
                             version = vsn()
                            }),
    lager:debug("Initialized htoad_htoad"),
    Engine1.


%% private

vsn() ->
    {ok, Vsn} = application:get_key(htoad, vsn),
    Vsn.
