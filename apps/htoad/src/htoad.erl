-module(htoad).
-export([start/0, assert/1, assert/2, retract/1, retract/2]).
-include_lib("htoad/include/htoad.hrl").

start() ->
    start(htoad).

start(App) ->
    case application:start(App) of
        {error, {not_started, Dep}} ->
            start(Dep),
            start(App);
        Other ->
            Other
    end.

assert(Fact) ->
    seresye:assert(?ENGINE, Fact).

assert(Engine, Fact) ->
    seresye_engine:assert(Engine, Fact).

retract(Fact) ->
    seresye:assert(?ENGINE, Fact).

retract(Engine, Fact) ->
    seresye_engine:retract(Engine, Fact).
