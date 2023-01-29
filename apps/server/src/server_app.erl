%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                                      {'_', [{"/websocket", websocket_handler, []}]}
                                     ]),
    %% websocket
    {ok, _} = cowboy:start_clear(websocket_listener,
                                 [{port, 6223}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    %% raw tcp
    {ok, _} = ranch:start_listener(tcp_listener,
                                   ranch_tcp, #{socket_opts => [{port, 6222}]},
                                   im_protocol, []),
    server_sup:start_link().

prep_stop(State) ->
    lists:foreach(fun(Listener) ->
                          ok = ranch:suspend_listener(Listener),
                          ok = ranch:wait_for_connections(Listener, '==', 0),
                          ok = ranch:stop_listener(Listener)
                  end, [websocket_listener, tcp_listener]),
    State.

stop(_State) ->
    ok.

%% internal functions
