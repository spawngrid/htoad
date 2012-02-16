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
    htoad_engine:add_rules(?ENGINE, Rules),
    case Rules of
        Module when is_atom(Module) ->
            ok;
        {Module, _} ->
            ok
    end,
    FReqs = proplists:get_value(htoad_file_requests, Module:module_info(attributes), []),
    assert([{file_request, F} || F <- FReqs]).

assert(Fact) when is_list(Fact); is_tuple(Fact) ->
    htoad_engine:assert(?ENGINE, Fact).

assert(Engine, Fact) when is_list(Fact); is_tuple(Fact) ->
    {Fun, Args} = seresye_engine:get_fired_rule(Engine),
    htoad_trace:assert(Engine, Fun, Args, Fact),
    seresye_engine:assert(Engine, Fact).

retract(Fact) when is_list(Fact); is_tuple(Fact) ->
    htoad_engine:assert(?ENGINE, Fact).

retract(Engine, Fact) when is_list(Fact); is_tuple(Fact) ->
    seresye_engine:retract(Engine, Fact).
