-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    BufLen = 15,
    BlockSize = 4,
    Data =  crypto:strong_rand_bytes(BufLen),
    Buf = <<
            begin
                {ok, Enc} = erlcode:golay_extended_encode(B),
                io:format(user, "encoded ~p to ~p~n", [B, Enc]),
                <<Enc:24/integer-unsigned-big>>
            end ||  <<B:12/integer-unsigned-big>> <= Data >>,
    io:format(user, "Raw data ~p, Golay ~p~n", [byte_size(Data), byte_size(Buf)]),

    StreamId = 0,
    Epsilon = 0.5,
    Q = 7,
    %% LDPC encode data
    {ok, EncData} = erldpc:encode_tc512(<<Buf/binary, 0, 0>>),
    {ok, Encoder} = erlang_oc:encoder_with_params(EncData, BlockSize, Epsilon, Q, StreamId),
    {ok, Decoder} = erlang_oc:decoder_with_params(byte_size(EncData), BlockSize, Epsilon, Q, StreamId),
    DecodeFun = fun(ResData) ->
                        decode_recursive(ResData)
                end,

    {Decoded, Iterations} = decode(Encoder, Decoder, 0, 0, 0, DecodeFun),
    io:format("Iterations: ~p~n", [Iterations]),
    io:format(user, "uncorrected bits ~p~n", [bit_diff(Decoded, Data)]),
    HeaderSize = 6,
    io:format(user, "uncorrected header bits ~p~n", [bit_diff(binary:part(Decoded, 0, HeaderSize), binary:part(Data, 0, HeaderSize))]),
    %?assertEqual(Decoded, EncData),
    %% LDPC decode data
    ?assertEqual(Decoded, Data).

amplifiy(0, Factor) ->
    1 * Factor;
amplifiy(1, Factor) ->
    -1 * Factor.

popcount(Byte) ->
    lists:sum([ X || <<X:1/integer>> <= <<Byte:8/integer>> ]).


bit_diff(A, B) ->
    lists:sum([ popcount(B1 bxor B2)  || {B1, B2} <-  lists:zip(binary_to_list(A), binary_to_list(B))]).
%% convenience driver function
decode(Encoder, Decoder, Iterations, Flips, Drops, Fun) ->
    case erlang_oc:next_drop(Encoder) of
        undefined ->
            io:format(user, "ran out of droplets~n", []),
            decode(Encoder, Decoder, Iterations + 1, Flips, Drops, Fun);
        {ok, {DropNum, Drop0}} ->
            case rand:uniform(10) < 3 of
                true ->
                    io:format(user, "losing droplet ~p~n", [DropNum]),
                    decode(Encoder, Decoder, Iterations + 1, Flips, Drops + 1, Fun);
                false ->
                    FlipCount = max(0, rand:uniform(5) - 2),
                    %io:format(user, "drop size is ~p~n", [byte_size(Drop0)]),
                    Drop = {DropNum, flip(Drop0, FlipCount)},
                    %io:format(user, "~w~n", [Drop]),
                    case erlang_oc:decode_drop(Drop, Decoder) of
                        {error, incomplete} ->
                            %io:format(user, "got incomplete~n", []),
                            decode(Encoder, Decoder, Iterations + 1, Flips + FlipCount, Drops, Fun);
                        {ok, Thing} ->
                            case Fun(Thing) of
                                {ok, Result} ->
                                    io:format(user, "recovered packet after ~p droplets with ~p flips and ~p drops~n", [Iterations, Flips + FlipCount, Drops]),
                                    {Result, Iterations};
                                error ->
                                    io:format(user, "had enough but decodefun failed ~p, keep going~n", [Iterations]),
                                    decode(Encoder, Decoder, Iterations + 1, Flips + Flips, Drops, Fun)
                            end
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
              binary:replace(Acc, <<(binary:at(Acc, Index))>>, <<(binary:at(Acc, Index) bxor rand:uniform(2) - 1)>>)
      end, Data, ToReplace).

-spec shuffle([A]) -> [A].
shuffle(Xs) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].


decode_recursive(ResData) ->
    case erldpc:decode_ms_tc512(ResData) of
        {ok, RR} ->
            LDPCRes = RR,
            io:format(user, "LDPC corrected ~p bits~n", [bit_diff(binary:part(ResData, 0, byte_size(LDPCRes)), LDPCRes)]),
            io:format(user, "recovered ~p bytes~n", [byte_size(LDPCRes)]),
            io:format(user, "~w~n", [LDPCRes]),
            {Decoded, Corrected} = from_golay(LDPCRes),
            io:format(user, "golay corrected ~p bits~n", [Corrected]),
            case lists:keymember(unrecoverable, 1, Decoded) of
                true ->
                    io:format(user, "some unrecovered golay code words detected, trying soft decoding~n", []),
                    decode_recursive_soft(Decoded, ResData);
                false ->
                    Result = list_to_bitstring(lists:reverse(Decoded)),
                    {ok, Result}
            end;
        O -> O
    end.

decode_recursive_soft(Decoded, ResData) ->
    %% try to infer some soft bits based on golay's reporting
    Soft = to_soft(Decoded, ResData),
    ?assertEqual(bit_size(ResData), length(Soft)),
    io:format(user, "Soft ~w~n", [Soft]),
    case erldpc:decode_ms_soft_tc512(Soft) of
        {ok, RR2} ->
            io:format(user, "RR2 is ~p~n", [RR2]),
            {Decoded2, Corrected2} = from_golay(RR2),
            io:format(user, "decoded ~p corrected ~p~n", [Decoded2, Corrected2]),

            case lists:keymember(unrecoverable, 1, Decoded2) of
                false ->
                    Result = list_to_bitstring(lists:reverse(Decoded2)),
                    {ok, Result};
                true when Corrected2 > 0 ->
                    decode_recursive_soft(Decoded2, ResData);
                true ->
                    %% hail mary pass
                    io:format(user, "LFG YOLOOOOO~n", []),
                    {ok, RR3} = erldpc:decode_ms_soft_tc512(to_soft(Decoded2, ResData), 100),

                    {Decoded3, Corrected3} = from_golay(RR3),
                    io:format(user, "decoded ~p corrected ~p~n", [Decoded3, Corrected3]),
                    Result = list_to_bitstring(lists:reverse(lists:map(fun({unrecoverable, _}) ->
                                                                               <<0:12/integer>>;
                                                                          (X) -> X
                                                                       end, Decoded3))),
                    {ok, Result}
            end;
        Error ->
            Error
    end.


to_soft(Decoded, ResData) ->
    lists:flatmap(fun({unrecoverable, Chunk}) ->
                          [ amplifiy(B, 1) || <<B:1/integer>> <= <<Chunk:24/integer-unsigned-big>> ];
                     (<<Bits:12/integer>>) ->
                          {ok, ReEncoded} = erlcode:golay_extended_encode(Bits),
                          [ amplifiy(B, 32) || <<B:1/integer>> <= <<ReEncoded:24/integer-unsigned-big>>]
                  end, lists:reverse(Decoded)) ++ lists:duplicate(16, 0) ++  [ amplifiy(B, 1) || <<B:1/integer>> <= binary:part(ResData, 32, 32) ].


from_golay(Bytes) ->
    lists:foldl(fun(Chunk, {Acc, Corrections}) ->
                        io:format(user, "decoding ~p~n", [Chunk]),
                        case erlcode:golay_extended_decode(Chunk) of
                            {ok, {Dec, Corr}} ->
                                {[<<Dec:12/integer>> | Acc], Corrections + Corr};
                            {error, {unrecoverable, _}} ->
                                {[{unrecoverable, Chunk} | Acc], Corrections}
                        end
                end, {[], 0}, [ B || <<B:24/integer-unsigned-big>> <= Bytes ]).

