%%%-------------------------------------------------------------------
%%% @author jerry <jerry@jerry>
%%% @copyright (C) 2023, jerry
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by jerry <jerry@jerry>
%%%-------------------------------------------------------------------
-module(websocket_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_client/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_client(Org, AppKey, User) ->
    supervisor:start_child(?SERVER, [Org, AppKey, User]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.
init([]) ->

    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 5},

    WebsocketClient = #{
               id => websocket_client,
               start => {websocket_client, start_link, []},
               restart => temporary,
               shutdown => brutal_kill,
               type => worker,
               modules => [websocket_client]},

    {ok, {SupFlags, [WebsocketClient]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
