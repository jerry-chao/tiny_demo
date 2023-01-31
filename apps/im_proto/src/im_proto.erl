%%%-------------------------------------------------------------------
%%% @author jerry <jerry@jerry>
%%% @copyright (C) 2023, jerry
%%% @doc
%%%
%%% @end
%%% Created : 29 Jan 2023 by jerry <jerry@jerry>
%%%-------------------------------------------------------------------
-module(im_proto).

%% API
-export([connect/4, command/1, decode_command/1]).
-include_lib("im_proto/include/im_pb.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec connect(Org::binary(), App::binary(), User::binary(), Token::binary()) -> #'Connect'{}.
connect(Org, App, User, Token) ->
    #'Connect'{
       org = Org,
       appkey = App,
       user = User,
       token = Token
      }.


command(Connect) when is_record(Connect, 'Connect')->
    im_pb:encode_msg(#'Command'{connect = Connect}).

decode_command(Data) ->
    im_pb:decode_msg(Data, 'Command').


%%%===================================================================
%%% Internal functions
%%%===================================================================
