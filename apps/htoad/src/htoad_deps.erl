-module(htoad_deps).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/2,on/3]).
-rules([init,on]).

init(Engine, #init{}) ->
    Engine.

on(Engine, Assertion, {on, Assertion, Plan}) ->
    seresye_engine:assert(Engine, Plan).

