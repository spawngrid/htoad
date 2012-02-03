-module(htoad_transform).
-export([parse_transform/2]).

-record(state,{ 
          options,
          absname
         }).

parse_transform(Forms, Options) ->
    {Forms1, _State} = parse_trans:transform(fun do_transform/4, 
                                             #state{ options = Options },
                                             Forms, Options),
    Result = parse_trans:revert(Forms1),
    Result.

do_transform(attribute,{attribute, _, htoad_absname, AbsName} = Attr, _Context, #state{} = State) ->
    {Attr, false, State#state{ absname = AbsName }};

do_transform(application,{call, Line, {atom, Line1, F}, [File]}, _Context,
                 #state{ absname = AbsName } = State) when F == load; F == file ->
    {{call, Line, {atom, Line1, F}, [
                                        {call, Line, {remote, Line, 
                                                   {atom, Line, filename},
                                                   {atom, Line, join}},
                                         [list_to_cons([
                                                        {call, Line, {remote, Line,
                                                                   {atom, Line, filename},
                                                                   {atom, Line, dirname}},
                                                         [{string, Line, AbsName}]},
                                                        File
                                                       ], Line)]}
                                       ]}, true, State};


do_transform(_Type, Form, _Context, State) ->
    {Form, true, State}.


list_to_cons([],Line) ->
    {nil,Line};
list_to_cons([H|T],Line) ->
    {cons, Line, H, list_to_cons(T, Line)}.
