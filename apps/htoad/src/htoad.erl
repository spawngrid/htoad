-module(htoad).
-export([start/0]).

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
