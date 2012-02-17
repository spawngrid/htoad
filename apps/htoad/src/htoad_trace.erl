-module(htoad_trace).
-export([rule/3, assert/4]).

-include_lib("htoad/include/htoad.hrl").

rule(Engine, Fun, Args) ->
    gen_event:notify(htoad_trace, {rule, Engine, Fun, Args}).

assert(Engine, Fun, Args, Fact) ->
    gen_event:notify(htoad_trace, {assert, Engine, Fun, Args, Fact}).
    
