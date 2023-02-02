%%%-------------------------------------------------------------------
%%% @author jerry <jerry@jerry>
%%% @copyright (C) 2023, jerry
%%% @doc
%%%
%%% @end
%%% Created :  2 Feb 2023 by jerry <jerry@jerry>
%%%-------------------------------------------------------------------
-module(command_handler).

%% API
-export([handle/2]).

-include_lib("im_proto/include/im_pb.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec handle(Command::#'Command'{}, State::map()) -> map().
handle(#'Command'{connect = Connection}, #{status := opening, transport := Transport, socket := Socket} = State) ->
    {NewState, ConnAck} =
        case process_connection(Connection) of
            true ->
                {maps:put(status, ready, State), im_proto:connack('STATUS_OK', <<"">>)};
            false ->
                {State, im_proto:connack('STATUS_FAIL', <<"">>)}
        end,
    %% send connack to client
    Transport:send(Socket, im_proto:command(ConnAck)),
    NewState.

process_connection(Connection) ->
    #'Connect'{org = Org, appkey = Appkey, user = User, token = Token} = Connection,
    %% check user token
    ?LOG_INFO("org = ~p, appkey = ~p, user = ~p, token = ~p", [Org, Appkey, User, Token]),
    true.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_test_() ->
    Org = <<"testorg">>,
    Appkey = <<"testapp">>,
    User = <<"user1">>,
    Token = <<"token">>,
    Connection = #'Connect'{org = Org, appkey = Appkey, user = User, token = Token},

    [
     ?_assertEqual(process_connection(Connection), true),
     ?_assert(true)
    ].
-endif.
