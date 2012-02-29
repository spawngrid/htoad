-module(htoad_transform).
-export([parse_transform/2]).
-include_lib("htoad/include/stdlib.hrl").
-compile({parse_transform, lager_transform}).

-record(state,{ 
          rules = [],
          current_rule = [], %% [] :: atom()
          file_requests = [],
          options,
          absname
         }).

-record(context, {module,
                  function,
                  arity,
                  file,
                  options}).


parse_transform(Forms, Options) ->
    #context{ file = File } = parse_trans:initial_context(Forms, Options),
    case erl_lint:module(Forms, File, [nowarn_unused_function,nowarn_unused_vars,nowarn_unused_record]) of
        {error, Errors, Warnings} ->
            (length(Warnings) > 0 andalso
             lager:debug("erl_lint warnings in ~s: ~p", [File, Warnings])),
            throw({erl_lint, Errors});
        {ok, Warnings} when is_list(Warnings), length(Warnings) > 0 ->
            lager:debug("erl_lint warnings in ~s: ~p", [File, Warnings]);
        _ ->
            ok
    end,
    {Forms1, State} = parse_trans:transform(fun do_transform/4, 
                                             #state{ options = Options },
                                             Forms, Options),
    Result = parse_trans:revert(Forms1),
    parse_trans:do_insert_forms(above, [{attribute, 0, htoad_file_requests, State#state.file_requests}], Result, parse_trans:initial_context(Result, Options)).

transform(Fun, State, Form, Context) when is_tuple(Form) ->
    {L,Rec,State1} = transform(Fun, State, [Form], Context),
    {hd(L),Rec,State1};

transform(Fun, State, Forms, Context) when is_list(Forms) ->
    {Form1, State1} = parse_trans:do_transform(Fun,
                                               State,
                                               Forms, 
                                               Context),
    {parse_trans:revert(Form1),false,State1}.


do_transform(attribute,{attribute, _, rule, Rule} = Attr, _Context, #state{ rules = Rs } = State) ->
    {Attr, false, State#state{ rules = [rule_name(Rule)|Rs] }};

do_transform(attribute,{attribute, _, rules, Rules0} = Attr, _Context, #state{ rules = Rs } = State) ->
    Rules = [ rule_name(R) || R <- Rules0 ],
    {Attr, false, State#state{ rules = Rules ++ Rs }};

do_transform(attribute,{attribute, _, htoad_absname, AbsName} = Attr, _Context, #state{} = State) ->
    {Attr, false, State#state{ absname = AbsName }};

do_transform(function, {function, _, Fun, Arity, _Cs} = Form, _Context, #state{ rules = Rules } = State) ->
    case lists:member(Fun, Rules) of
        false ->
            {Form, true, State#state{ current_rule = [] }};
        true ->
            {Form, true, State#state{ current_rule = Fun }}
    end;

do_transform(clause, {clause, Line, Head, G, B}, Context,
             #state{ current_rule = CurFun} = State) when CurFun /= [] ->
    {Head1, _Rec, State1} = transform(fun clause_scanner/4, State, Head, Context),
    {B1, Rec, State2} = transform(fun do_transform/4, State1#state{ current_rule = [] }, B, Context),
    {{clause, Line, Head1, G, B1}, Rec, State2};

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


clause_scanner(record_expr, {record, _L, file, Fields} = Form, _Context, 
               #state{ file_requests = FReqs } = State) ->
    case scan_file_record(Fields) of
        #file{ path = Path } = File when is_list(Path) ->
            {Form, false, State#state{ file_requests = [File|FReqs] }};
        _ ->
            {Form, false, State}
    end;
clause_scanner(_Type, Form, _Context, State) ->
    {Form, false, State}.


scan_file_record(Fields) ->
    DefaultFile = #file{},
    RFields = record_info(fields, file),
    {RFields, Vals} = lists:unzip(scan_file_record_1(Fields, lists:zip(RFields, tl(tuple_to_list(DefaultFile))))),
    list_to_tuple([file|Vals]).

scan_file_record_1([], Acc) ->
    Acc;
scan_file_record_1([{record_field, _, {atom, _, _Name}, {var, _, _}}|T], Acc) ->
    scan_file_record_1(T, Acc);
scan_file_record_1([{record_field, _, {atom, _, Name}, Value0}|T], Acc) ->
    Value = erl_syntax:concrete(Value0),
    scan_file_record_1(T, lists:keyreplace(Name, 1, Acc, {Name, Value})).
    
    

list_to_cons([],Line) ->
    {nil,Line};
list_to_cons([H|T],Line) ->
    {cons, Line, H, list_to_cons(T, Line)}.

rule_name(A) when is_atom(A) ->
    A;
rule_name({A, _}) ->
    A.
