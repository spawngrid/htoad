-module(htoad_app).
-behaviour(application).

-include_lib("htoad/include/htoad.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, {Args, Files}} = getopt:parse(optspec(), init:get_plain_arguments()),
    case Files of
        [] ->
            getopt:usage(optspec(), "htoad","[file ...]",
                         [{"file", "Instructions file to process"}]),
            init:stop(),
            {ok, self()};
        _ ->
            htoad_sup:start_link(Args, Files)
    end.


stop(_State) ->
    ok.

%% private
        
optspec() ->
    [
    ].
