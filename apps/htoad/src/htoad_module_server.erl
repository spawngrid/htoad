-module(htoad_module_server).
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
          module
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
    File = filename:absname(filename:join(os:getenv("HTOAD_CWD"), File0)),
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
handle_cast(init, #state{ file = File } = State) ->
    Module = load_file(File),
    seresye:assert(?ENGINE, #'htoad.module'{ filename = File, module = Module }),
    lager:debug("Loaded module ~s", [File]),
    seresye:assert(?ENGINE, Module:main()),
    lager:debug("Loaded module ~s assertions", [File]),
    {noreply, State#state{ module = Module }}.

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
terminate(_Reason, #state{ file = File, module = Module } = _State) ->
    lager:debug("Unloading module ~s", [File]),
    code:delete(Module),
    code:purge(Module),
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
    Source =
        "-module('" ++ atom_to_list(Module) ++ "').\n"
        "-include(\"stdlib.hrl\").\n"
        "-compile(export_all).\n"
        "-import(htoad_utils, [on/2]).\n" ++ S ++ "\n \n",
    dynamic_compile:load_from_string(Source, [{i, code:lib_dir(htoad,include)}]),
    Module.
