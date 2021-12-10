-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    BufLen = 1024,
    BlockSize = BufLen div 4,
    Data = crypto:strong_rand_bytes(BufLen),
    StreamId = 0,
    {ok, Encoder} = erlang_oc:encoder(Data, BlockSize, StreamId),
    {ok, Decoder} = erlang_oc:decoder(BufLen, BlockSize, StreamId),
    {Decoded, Iterations} = decode(Encoder, Decoder, 0),
    io:format("Iterations: ~p~n", [Iterations]),
    ?assertEqual(Decoded, Data).

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
