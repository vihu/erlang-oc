-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    BufLen = 1024,
    BlockSize = BufLen div 4,
    NumBlocks = BufLen div BlockSize,
    Data = crypto:strong_rand_bytes(BufLen),
    StreamId = 0,

    {ok, {Coder, Iterator}} = erlang_oc:encode(BlockSize, Data, StreamId),
    io:format("Coder: ~p, Iterator: ~p", [Coder, Iterator]),

    {ok, Decoder} = erlang_oc:decoder(NumBlocks, BlockSize, StreamId),
    io:format("Decoder: ~p", [Decoder]),

    Decoded = decode(Decoder, Iterator, 0),
    io:format("Decoder: ~p", [Decoded]),

    ?assert(is_reference(Coder)),
    ?assert(is_reference(Iterator)),
    ?assertEqual(binary:bin_to_list(Data), Decoded).

%% convenience driver function
decode(Decoder, Iterator, Iterations) ->
    case erlang_oc:decode(Decoder, Iterator) of
        ok ->
            ok;
        {ok, {{NewDecoder, NewIterator}, nil}} ->
            decode(NewDecoder, NewIterator, Iterations + 1);
        {ok, {nil, Result}} ->
            io:format("Result: ~p, Iterations: ~p~n", [Result, Iterations]),
            Result
    end.
