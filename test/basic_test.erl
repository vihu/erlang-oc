-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    BufLen = 1024,
    BlockSize = BufLen div 4,
    NumBlocks = BufLen div BlockSize,
    Data = crypto:strong_rand_bytes(BufLen),
    StreamId = 0,

    {ok, {Coder, Iterator}} = erlang_oc:encode(BlockSize, Data, StreamId),

    {ok, Decoder} = erlang_oc:decoder(NumBlocks, BlockSize, StreamId),

    Decoded = decode(Decoder, Iterator, 0),

    ?assert(is_reference(Coder)),
    ?assert(is_reference(Iterator)),
    ?assertEqual(binary:bin_to_list(Data), Decoded).

%% convenience driver function
decode(Decoder, Iterator, Iterations) ->
    case erlang_oc:decode(Decoder, Iterator) of
        ok ->
            ok;
        {ok, {NewDecoder, NewIterator}} ->
            decode(NewDecoder, NewIterator, Iterations + 1);
        {ok, Result} ->
            io:format("Result: ~p, Iterations: ~p~n", [Result, Iterations]),
            Result
    end.
