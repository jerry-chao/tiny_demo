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
-export([encode_varint/1, decode_varint/1]).
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
    CommandBinary = im_pb:encode_msg(#'Command'{connect = Connect}),
    Byte = byte_size(CommandBinary),
    Len = encode_varint(Byte),
    <<Len/binary, CommandBinary/binary>>.

decode_command(Data) ->
    {Len, Rest} = decode_varint(Data),
    case byte_size(Rest) >= Len of
        true ->
            <<Command:Len/binary, Last/binary>> = Rest,
            {im_pb:decode_msg(Command, 'Command'), Last};
        false ->
            {more, Data}
    end.

-spec decode_varint(binary()) -> {non_neg_integer(), binary()}.
decode_varint(Data) when is_binary(Data) ->
    decode_varint(Data, 0, 0).

-spec encode_varint(non_neg_integer()) -> binary().
encode_varint(I) when is_integer(I), I >= 0, I =< 127 ->
    <<I>>;
encode_varint(I) when is_integer(I), I > 127 ->
    <<1:1, (I band 127):7, (encode_varint(I bsr 7))/binary>>;
encode_varint(I) when is_integer(I), I < 0 ->
    erlang:error({badarg, I}).

decode_varint(<<1:1, Number:7, Rest/binary>>, Position, Acc) ->
    decode_varint(Rest, Position + 7, (Number bsl Position) + Acc);
decode_varint(<<0:1, Number:7, Rest/binary>>, Position, Acc) ->
    {(Number bsl Position) + Acc, Rest}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
