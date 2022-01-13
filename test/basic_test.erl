-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    BufLen = 32,
    BlockSize = 8,
    Data = list_to_binary(lists:map(fun(_) ->
                                            case rand:uniform(2) of
                                                1 -> 85;
                                                2 -> 42
                                            end
                                    end, lists:seq(1, BufLen))),
    StreamId = 0,
    Epsilon = 0.5,
    Q = 20,
    %% LDPC encode data
    {ok, EncData} = erldpc:encode_tc512(Data),
    {ok, Encoder} = erlang_oc:encoder_with_params(EncData, BlockSize, Epsilon, Q, StreamId),
    {ok, Decoder} = erlang_oc:decoder_with_params(byte_size(EncData), BlockSize, Epsilon, Q, StreamId),
    DecodeFun = fun(ResData) ->
                        case erldpc:decode_ms_tc512(ResData) of
                            {ok, RR} ->
                                LDPCRes = binary:part(RR, 0, BufLen),
                                Result = correct(LDPCRes),
                                io:format(user, "LDPC corrected ~p bits~n", [bit_diff(binary:part(ResData, 0, BufLen), LDPCRes)]),
                                io:format(user, "hamming corrected ~p bits~n", [bit_diff(LDPCRes, Result)]),
                                {ok, Result};
                            O -> O
                        end
                end,

    {Decoded, Iterations} = decode(Encoder, Decoder, 0, DecodeFun),
    io:format("Iterations: ~p~n", [Iterations]),
    %?assertEqual(Decoded, EncData),
    %% LDPC decode data
    ?assertEqual(Decoded, Data).

correct(Bin) ->
    << <<(correct_byte(B)):8/integer>> || <<B:8/integer>> <= Bin >>.

correct_byte(Byte) ->
    case {popcount(Byte bxor 85), popcount(Byte bxor 42)} of
        {A, B} when A < B ->
            85;
        {A, B} when A > B ->
            42;
        {A, A} ->
            Byte
    end.

popcount(Byte) ->
    lists:sum([ X || <<X:1/integer>> <= <<Byte:8/integer>> ]).


bit_diff(A, B) ->
    lists:sum([ popcount(B1 bxor B2)  || {B1, B2} <-  lists:zip(binary_to_list(A), binary_to_list(B))]).
%% convenience driver function
decode(Encoder, Decoder, Iterations, Fun) ->
    case erlang_oc:next_drop(Encoder) of
        undefined ->
            io:format(user, "ran out of droplets~n", []),
            decode(Encoder, Decoder, Iterations + 1, Fun);
        {ok, {DropNum, Drop0}} ->
            %io:format(user, "drop size is ~p~n", [byte_size(Drop0)]),
            Drop = {DropNum, flip(Drop0, max(0, rand:uniform(32) + 32))},
            %io:format(user, "~w~n", [Drop]),
            case erlang_oc:decode_drop(Drop, Decoder) of
                {error, incomplete} ->
                    %io:format(user, "got incomplete~n", []),
                    decode(Encoder, Decoder, Iterations + 1, Fun);
                {ok, Thing} ->
                    case Fun(Thing) of
                        {ok, Result} ->
                            io:format(user, "recovered packet after ~p droplets~n", [Iterations]),
                            {Result, Iterations};
                        error ->
                            io:format(user, "had enough but decodefun failed ~p, keep going~n", [Iterations]),
                            decode(Encoder, Decoder, Iterations + 1, Fun)
                    end
            end
    end.

flip(Data, 0) ->
    Data;
flip(Data, NumBits) ->
    io:format(user, "flipping ~p bits of ~p~n", [NumBits, bit_size(Data)]),
    List = lists:seq(1, byte_size(Data) - 1),
    ToReplace = lists:sublist(shuffle(List), NumBits),

    lists:foldl(
      fun(Index, Acc) ->
              binary:replace(Acc, <<(binary:at(Acc, Index))>>, <<(binary:at(Acc, Index) bxor 1)>>)
      end, Data, ToReplace).

-spec shuffle([A]) -> [A].
shuffle(Xs) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].
