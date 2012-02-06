-module(htoad_error).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-rules([report]).

report(Engine, #error_report{ rule = R, fact = F, reason = Reason }) ->
    lager:error("Rule ~p failed to process the assertion of ~p with reason: ~p", [R, F, Reason]),
    Engine.
