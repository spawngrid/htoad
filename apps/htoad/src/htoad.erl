-module(htoad).
-export([start/0, add_rules/1, assert/1, assert/2, retract/1, retract/2]).
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

add_rules(Rules) ->
    seresye:add_rules(?ENGINE, Rules),
    case Rules of
        Module when is_atom(Module) ->
            ok;
        {Module, _} ->
            ok
    end,
    FReqs = proplists:get_value(htoad_file_requests, Module:module_info(attributes), []),
    assert([{file_request, F} || F <- FReqs]).

assert(Fact) ->
    seresye:assert(?ENGINE, Fact).

assert(Engine, Fact) ->
    seresye_engine:assert(Engine, Fact).

retract(Fact) ->
    seresye:assert(?ENGINE, Fact).

retract(Engine, Fact) ->
    seresye_engine:retract(Engine, Fact).
