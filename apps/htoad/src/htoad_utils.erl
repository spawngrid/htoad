-module(htoad_utils).
-export([ensure/2, on/2, load/1, module/1, file/1, render/2, render/3, render/4]).

ensure(What, Object) ->
    {ensure, What, Object}.

on([], Plan) ->
    Plan;
on([H|T], Plan) ->
    [{on, H, on(T, Plan)}];
on(What, Plan) ->
    {on, What, Plan}.

load(File) ->
    {load, File}.

module(Module) ->
    {module, Module}.

file(File) ->
    filename:join(["/", "$htoad/master", File]).

render(Template, Vars) ->
    render(Template, [], Vars, []).

render(Template, Vars, RenderOpts) ->
    render(Template, [], Vars, RenderOpts).

render(Template, Options, Vars, RenderOpts) ->
    Module = list_to_atom("htoad_template_" ++ integer_to_list(erlang:phash2(Template))),
    case code:which(Module) of
        non_existing ->
            erlydtl:compile(Template, Module, Options);
        _ ->
            ok
    end,
    {ok, Content} = Module:render(Vars, RenderOpts),
    Content.
