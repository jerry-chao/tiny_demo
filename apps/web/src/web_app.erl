%%%-------------------------------------------------------------------
%% @doc web public API
%% @end
%%%-------------------------------------------------------------------

-module(web_app).

-behaviour(application).

-export([start/2, stop/1]).
-export([update_routes/0, routes/0]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(routes()),
    {ok, _} = cowboy:start_clear(
        web_http_listener,
        [{port, 8084}],
        #{env => #{dispatch => Dispatch}}
    ),
    web_sup:start_link().

stop(_State) ->
    ok.

update_routes() ->
    Dispatch = cowboy_router:compile(routes()),
    cowboy:set_env(web_http_listener, dispatch, Dispatch).

routes() ->
    msghooks_app:routes().

%% internal functions
