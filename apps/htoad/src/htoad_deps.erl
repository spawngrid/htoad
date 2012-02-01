-module(htoad_deps).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/2,on/3,on_match/3]).
-rules([init,on,on_match]).

init(Engine, #init{}) ->
    Engine.

on_match(Engine, Assertion, {on, {match, MatchSpec}, Plan}) ->
    Ms = ets:match_spec_compile(MatchSpec),
    case ets:match_spec_run([Assertion], Ms) of
        [] ->
            Engine;
        [_] ->
            lager:debug("Condition ~p matching ~p for the plan of ~p has occurred", [Assertion, MatchSpec, Plan]),
            seresye_engine:assert(Engine, Plan)
    end.

on(Engine, Assertion, {on, Assertion, Plan}) ->
    lager:debug("Condition ~p for the plan of ~p has occurred", [Assertion, Plan]),
    seresye_engine:assert(Engine, Plan).

    
