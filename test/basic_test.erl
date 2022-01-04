-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    BufLen = 32,
    BlockSize = BufLen div 4,
    Data = crypto:strong_rand_bytes(BufLen),
    StreamId = 0,
    %% LDPC encode data
    {ok, EncData} = erldpc:encode_tc512(Data),
    {ok, Encoder} = erlang_oc:encoder(EncData, BlockSize, StreamId),
    {ok, Decoder} = erlang_oc:decoder(byte_size(EncData), BlockSize, StreamId),
    {Decoded, Iterations} = decode(Encoder, Decoder, 0),
    io:format("Iterations: ~p~n", [Iterations]),
    ?assertEqual(Decoded, EncData),
    %% LDPC decode data
    {ok, DecData} = erldpc:decode_tc512(flip(Decoded, 4)),
    ?assertEqual(binary:part(DecData, 0, BufLen), Data).

%% convenience driver function
decode(Encoder, Decoder, Iterations) ->
    case erlang_oc:next_drop(Encoder) of
        undefined ->
            decode(Encoder, Decoder, Iterations + 1);
        {ok, Drop} ->
            case erlang_oc:decode_drop(Drop, Decoder) of
                {error, incomplete} ->
                    decode(Encoder, Decoder, Iterations + 1);
                {ok, Thing} ->
                    {Thing, Iterations}
            end
    end.

flip(Data, NumBits) ->
    List = lists:seq(1, byte_size(Data) - 1),
    ToReplace = lists:sublist(shuffle(List), NumBits),

    lists:foldl(
      fun(Index, Acc) ->
              binary:replace(Acc, <<(binary:at(Acc, Index))>>, <<(binary:at(Acc, Index) bxor 1)>>)
      end, Data, ToReplace).

-spec shuffle([A]) -> [A].
shuffle(Xs) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].
