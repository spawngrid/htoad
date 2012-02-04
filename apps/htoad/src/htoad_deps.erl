-module(htoad_deps).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/2,on/3,on_match/3]).
-rules([init,on,on_match]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_deps"),
    Engine.

on_match(Engine, Assertion, {on, {match, MatchSpec}, Plan}) ->
    Ms = ets:match_spec_compile(MatchSpec),
    case ets:match_spec_run([Assertion], Ms) of
        [] ->
            Engine;
        [Match] ->
            SubstPlan = substitute(Plan, Match),
            lager:debug("Condition ~p matching ~p for the plan of ~p has occurred", [Assertion, MatchSpec, SubstPlan]),
            htoad:assert(Engine, SubstPlan)
    end.

on(Engine, Assertion, {on, Assertion, Plan}) ->
    lager:debug("Condition ~p for the plan of ~p has occurred", [Assertion, Plan]),
    htoad:assert(Engine, Plan).

    
%% private

substitute([], _Match) ->
    [];
substitute([H|T], Match) ->
    [substitute(H, Match)|substitute(T, Match)];
substitute('_', Match) ->
    Match;
substitute(Tuple, Match) when is_tuple(Tuple) ->
    list_to_tuple(substitute(tuple_to_list(Tuple), Match));
substitute(Other, _Match) ->
    Other.



    
