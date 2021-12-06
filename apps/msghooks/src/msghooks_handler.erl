%%%-------------------------------------------------------------------
%%% @author jerry <jerry@zhangchao>
%%% @copyright (C) 2021, jerry
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2021 by jerry <jerry@zhangchao>
%%%-------------------------------------------------------------------
-module(msghooks_handler).

%% API
-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2, content_types_provided/2, db_to_json/2]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

init(Req, State) ->
    ?LOG(debug, "state:~p", [State]),
    {cowboy_rest, Req, State}.

is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {bearer, <<"test">>} ->
            {true, Req, State};
        Error ->
            ?LOG(debug, "auth error :~p ", [Error]),
            {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
    end.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"POST">>, <<"DELETE">>, <<"PUT">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    {
        [
            {<<"application/json">>, db_to_json}
        ],
        Req,
        State
    }.

db_to_json(Req, State) ->
    Body = io_lib:format("{\"test\": \"test\"}", []),
    {list_to_binary(Body), Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
