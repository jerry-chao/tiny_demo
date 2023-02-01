%%%-------------------------------------------------------------------
%%% @author jerry <jerry@jerry>
%%% @copyright (C) 2023, jerry
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by jerry <jerry@jerry>
%%%-------------------------------------------------------------------
-module(websocket_client).

-behaviour(gen_server).

%% API
-export([start_link/6, send/2, connect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-include_lib("kernel/include/logger.hrl").

-record(state, {org, app, user, conn_pid, transport, socket}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary(), binary(), binary(), atom(), binary(), integer()) -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link(Org, App, User, Transport, Host, Port) ->
    gen_server:start_link(?MODULE, [Org, App, User, Transport, Host, Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init([Org, App, User, Transport, Host, Port]) ->
    process_flag(trap_exit, true),
    {ok, Sock} = Transport:connect(Host, Port, [binary, {packet, 0}]),
    {ok, #state{socket = Sock,
                transport = Transport,
                org = Org,
                app = App,
                user = User}};
init([Org, App, User, ws, Host, Port]) ->
    process_flag(trap_exit, true),
    {ok, ConnPid} = gun:open(Host, Port),
    {ok, _Protocol} = gun:await_up(ConnPid),
    _MRef = monitor(process, ConnPid),
    gun:ws_upgrade(ConnPid, "/websocket"),
    {ok, #state{conn_pid = ConnPid,
                transport = tcp,
                org = Org,
                app = App,
                user = User}}.

send(Client, Command) ->
    gen_server:call(Client, Command).

connect(Client) ->
    gen_server:call(Client, {connect, <<"test">>}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call({send, Message}, _From, #state{transport = Transport, socket = Socket} = State) ->
    ?LOG_INFO("send message ~p", [Message]),
    Transport:send(Socket, Message),
    {reply, ok, State};
handle_call({connect, Token}, _From, #state{org = Org, app = App, user = User} = State) ->
    Connect = im_proto:connect(Org, App, User, Token),
    ConnectCommand = im_proto:command(Connect),
    handle_call({send, ConnectCommand}, _From, State);
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({tcp, Socket, Data}, #state{socket = Socket, transport = _Transport} = State) ->
    ?LOG_INFO("recv message ~p", [Data]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, _State) ->
    {stop, normal};
handle_info({tcp_error, _, Reason}, _State) ->
    {stop, Reason};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
