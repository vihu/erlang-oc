-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    BufLen = 128,
    BlockSize = BufLen div 4,
    Data = crypto:strong_rand_bytes(BufLen),
    StreamId = 0,
    %% LDPC encode data
    %% XXX: maybe try some other encode strategy?
    {ok, EncData} = erldpc:encode_tm1280(Data),
    {ok, Encoder} = erlang_oc:encoder(EncData, BlockSize, StreamId),
    {ok, Decoder} = erlang_oc:decoder(BufLen, BlockSize, StreamId),
    {Decoded, Iterations} = decode(Encoder, Decoder, 0),
    io:format("Iterations: ~p~n", [Iterations]),
    %% LDPC decode data
    %% XXX: nif panic!
    {ok, DecData} = erldpc:decode_tm1280(Decoded),
    ?assertEqual(DecData, Data).

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
