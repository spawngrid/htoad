-module(htoad_utils).
-export([on/2, load/1]).

on([], Plan) ->
    Plan;
on([H|T], Plan) ->
    [{on, H, on(T, Plan)}];
on(What, Plan) ->
    {on, What, Plan}.

load(File) ->
    {load, File}.
