-module(htoad_trace).
-export([rule/3, assert/4]).

-include_lib("htoad/include/htoad.hrl").

rule(_Engine, Fun, Args) ->
    Info = erlang:fun_info(Fun),
    Key = {rule,
           proplists:get_value(module, Info),
           proplists:get_value(name, Info),
           proplists:get_value(arity, Info)},
    ets:insert(htoad_trace, [{{facts, Args}, Key}|[ {{fact, Arg}, Key} || Arg <- Args ]]).

assert(Engine, Fun, Args, Facts) when is_list(Facts) ->
    [ assert(Engine, Fun, Args, Fact) || Fact <- Facts ];
assert(_Engine, Fun, _Args, Fact) ->
    Info = erlang:fun_info(Fun),
    Key = {rule,
           proplists:get_value(module, Info),
           proplists:get_value(name, Info),
           proplists:get_value(arity, Info)},
    ets:insert(htoad_trace, {Key, {fact, Fact}}).
    
