-module(htoad_io).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/toadie.hrl").
-include_lib("htoad/include/stdlib.hrl").

-export([init/2, print/2]).
-rules([init, print]).

init(Engine, #init{}) ->
    lager:debug("Initialized htoad_io"),
    Engine.

print(Engine, {print, Text}) ->
    io:format(Text),
    Engine.
