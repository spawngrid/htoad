-module(htoad_base).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-rules([retract]).

retract(Engine, {retract, Rule}) ->
    lager:debug("Retracting ~p",[Rule]),
    htoad:retract(Engine, Rule).
