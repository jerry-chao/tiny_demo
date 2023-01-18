%%%-------------------------------------------------------------------
%%% @author jerry <jerry@jerry>
%%% @copyright (C) 2023, jerry
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by jerry <jerry@jerry>
%%%-------------------------------------------------------------------
-module(websocket_handler).

%% API
-export([init/2]).

-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init(Req, State) ->
    {cowboy_websocket, Req, State,  #{idle_timeout => 30000, max_frame_size => 8000000}}.

websocket_init(State) ->
    erlang:start_timer(1000, self(), <<"Hello!">>),
    {ok, State}.

websocket_handle(Frame = {text, _}, State) ->
    {[Frame], State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({log, Text}, State) ->
    {[{text, Text}], State};
websocket_info(_Info, State) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
