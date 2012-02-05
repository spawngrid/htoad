-module(htoad_toadie_server).
-behaviour(gen_server).

-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          file,
          toadie,
          applied = false
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(File) ->
    gen_server:start_link(?MODULE, File, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(File0) ->
    File = htoad_utils:file(File0),
    erlang:process_flag(trap_exit, true),
    gen_server:cast(self(), init),
    {ok, #state{ file = File }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(apply, #state{ file = File, toadie = Toadie, applied = false } = State) ->
    lager:debug("Applying toadie ~s",[File]),
    case erlang:function_exported(Toadie, main, 0) of
        true ->
            htoad:assert(Toadie:main()),
            lager:debug("Finished applying toadie ~s assertions", [File]);
        false ->
            lager:debug("No main() function in toadie ~s", [File])
    end,
    {noreply, State#state { applied = true }};

handle_cast(init, #state{ file = File } = State) ->
    try load_file(File) of
        {Toadie, Bin} ->
            load_file(File),
            htoad:assert(#'htoad.toadie'{ filename = File, module = Toadie, server = self() }),
            lager:debug("Loaded toadie ~s", [File]),
            load_rules(File, Toadie, Bin),
            dry_run(File, Toadie),
            htoad:assert({htoad_toadie_server_ready, self()}),
            {noreply, State#state{ toadie = Toadie }}
    catch throw:{_, compile_forms, Errors} ->
            lager:error("Errors compiling ~s: ~p", [File, Errors]),
            {stop, shutdown, State}
    end;

handle_cast(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{ file = File, toadie = Toadie } = _State) ->
    lager:debug("Unloading toadie ~s", [File]),
    code:delete(Toadie),
    code:purge(Toadie),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Private
load_file(File) ->
    {ok, B} = file:read_file(File),
    S = binary_to_list(B),
    Module = list_to_atom(re:replace(File,"\\.","_",[{return, list}, global])),
    Utils = string:join([ atom_to_list(F) ++ "/" ++ integer_to_list(A) ||
                            {F,A} <- htoad_utils:module_info(exports) ],
                        ", "),
    Source =
        "-module('" ++ atom_to_list(Module) ++ "').\n"
        "-include(\"toadie.hrl\").\n"
        "-include(\"stdlib.hrl\").\n"
        "-htoad_absname(\"" ++ File ++ "\").\n"
        "-import(htoad_utils, [" ++ Utils ++ "]).\n" ++ S ++ "\n \n",
    {Module, Binary} = dynamic_compile:from_string(Source, [export_all,
                                                            nowarn_unused_function,nowarn_unused_vars,nowarn_unused_record,
                                                            return_errors, debug_info, 
                                                            {parse_transform, lager_transform}, 
                                                            {i, filename:dirname(File)},
                                                            {i, code:lib_dir(htoad,include)},{i, htoad_utils:file(".")}]),
    code:load_binary(Module, htoad_utils:file(File ++ ".beam"), Binary),
    {Module, Binary}.

load_rules(File, Toadie, Bin) ->
    case proplists:get_value(rules, Toadie:module_info(attributes)) of
        undefined ->
            ok;
        Rules ->
            lager:debug("[+ Adding following rules for ~s: ~p]", [File, Rules]),
            Beam = htoad_utils:file(File ++ ".beam"),
            file:write_file(Beam, Bin),
            Result = htoad:add_rules(Toadie),
            file:delete(Beam),
            Result = ok
    end.
        
dry_run(File, Toadie) ->
    {ok, DryEngine} = seresye:start(),
    lager:debug("[~w Dry run for ~s]",[DryEngine, File]),
    seresye:add_rules(DryEngine, htoad_toadies),
    seresye:assert(DryEngine, dry_run),
    case erlang:function_exported(Toadie, main, 0) of
        true ->
            seresye:assert(DryEngine, Toadie:main());
        false ->
            ok
    end,
    lager:debug("[~w Dry run is over for ~s]",[DryEngine, File]),
    %% FIXME in seresye : seresye:stop(DryEngine).
    (catch gen_server:call(DryEngine, stop)).
    
