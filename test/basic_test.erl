-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    BufLen = 128,
    BlockSize = BufLen div 4,
    Data = crypto:strong_rand_bytes(BufLen),
    StreamId = 0,
    %% LDPC encode data
    {ok, EncData} = erldpc:encode_tm1280(Data),
    {ok, Encoder} = erlang_oc:encoder(EncData, BlockSize, StreamId),
    {ok, Decoder} = erlang_oc:decoder(byte_size(EncData), BlockSize, StreamId),
    {Decoded, Iterations} = decode(Encoder, Decoder, 0),
    io:format("Iterations: ~p~n", [Iterations]),
    ?assertEqual(Decoded, EncData),
    %% LDPC decode data
    {ok, DecData} = erldpc:decode_tm1280(flip(Decoded, 1)),
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

flip(Data, Pct) ->
    << begin
           case rand:uniform(100) < Pct of
               true -> <<(B bxor 1):1/integer>>;
               false -> <<B:1/integer>>
           end
       end || <<B:1/integer>> <= Data >>.
