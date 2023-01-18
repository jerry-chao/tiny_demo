%%%-------------------------------------------------------------------
%% @doc server public API
%% @end
%%%-------------------------------------------------------------------

-module(server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                                      {'_', [{"/websocket", websocket_handler, []}]}
                                     ]),
    {ok, _} = cowboy:start_clear(websocket_listener,
                                 [{port, 6223}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
