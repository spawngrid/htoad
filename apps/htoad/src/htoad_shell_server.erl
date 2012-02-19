-module(htoad_shell_server).
-include_lib("htoad/include/htoad.hrl").
-include_lib("htoad/include/stdlib.hrl").

-behaviour(gen_server).

%% API
-export([start/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
          shell :: #shell{},
          port :: port(),
          exit_status :: undefined | integer(),
          output = [] :: string(),
          return_to :: undefined | term()
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
start(Shell) ->
    {ok, Pid} = supervisor:start_child(htoad_shell, [Shell]),
    gen_server:call(Pid, start, infinity).

start_link(Shell) ->
    gen_server:start_link(?MODULE, Shell, []).

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
init(Shell) ->
    gen_server:cast(self(), start),
    {ok, #state{ shell = Shell }}.

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
handle_call(start, From, #state{ shell = #shell{ cmd = Cmd } } = State) ->
    Port = open_port({spawn_executable, "/bin/sh"},
                     [{args, ["-c", Cmd]},
                      stderr_to_stdout,
                      exit_status,
                      hide %% Windows only
                     ]),
    {noreply, State#state{ port = Port, return_to = From }}.

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
handle_cast(_Msg, State) ->
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
handle_info({Port, {data, Data}}, #state{ port = Port, output = Output } = State) ->
    {noreply, State#state{ output = Output ++ Data }};
handle_info({Port, {exit_status, Status}}, #state{ port = Port, output = Output,
                                                   return_to = From } = State) ->
    gen_server:reply(From, {Status, Output}),
    {stop, normal, State#state{ exit_status = Status }};
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
terminate(_Reason, _State) ->
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
