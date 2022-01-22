-module(erlang_oc_util).

-export([execute/6]).

-spec execute(
    Data :: binary(),
    Epsilon :: float(),
    Q :: pos_integer(),
    BlockSize :: pos_integer(),
    StreamId :: non_neg_integer(),
    FlipCount :: non_neg_integer()
) ->
    {
        Check :: boolean(),
        Iterations :: non_neg_integer(),
        Flips :: non_neg_integer(),
        Header :: binary(),
        BestMatch :: binary()
    }.
execute(Data, Epsilon, Q, BlockSize, StreamId, FlipCount) ->
    Buf = <<
        begin
            {ok, Enc} = erlcode:bch_encode(B),
            <<Enc:64/integer-unsigned-big>>
        end
     || <<B:16/integer-unsigned-big>> <= Data
    >>,

    %% LDPC encode data
    {ok, EncData} = erldpc:encode_tc512(Buf),
    {ok, Encoder} = erlang_oc:encoder_with_params(EncData, BlockSize, Epsilon, Q, StreamId),
    {ok, Decoder} = erlang_oc:decoder_with_params(
        byte_size(EncData), BlockSize, Epsilon, Q, StreamId
    ),
    DecodeFun = fun(ResData) ->
        decode_recursive(ResData)
    end,

    {Decoded, Iterations, Flips} = decode(Encoder, Decoder, 0, 0, 0, DecodeFun, FlipCount),
    HeaderSize = byte_size(Data),
    Header = binary:part(Data, 0, HeaderSize),
    %% match the header out of a bunch of candidates
    HeaderCandidates = [Header | [crypto:strong_rand_bytes(HeaderSize) || _ <- lists:seq(1, 1000)]],
    {_, BestMatch} = hd(
        lists:keysort(1, [
            {bit_diff(binary:part(Decoded, 0, HeaderSize), C), C}
         || C <- HeaderCandidates
        ])
    ),
    %% LDPC decode data
    Check = Header == BestMatch,
    {Check, Iterations, Flips, Header, BestMatch}.

%% ==================================================================
%% Internal Functions
%% ==================================================================

amplify(0, Factor) ->
    1 * Factor;
amplify(1, Factor) ->
    -1 * Factor.

popcount(Byte) ->
    lists:sum([X || <<X:1/integer>> <= <<Byte:8/integer>>]).

bit_diff(A, B) ->
    lists:sum([popcount(B1 bxor B2) || {B1, B2} <- lists:zip(binary_to_list(A), binary_to_list(B))]).

%% convenience driver function
decode(Encoder, Decoder, Iterations, Flips, Drops, Fun, FlipCount) ->
    case erlang_oc:next_drop(Encoder) of
        undefined ->
            decode(Encoder, Decoder, Iterations + 1, Flips, Drops, Fun, FlipCount);
        {ok, {DropNum, Drop0}} ->
            case rand:uniform(10) < 3 of
                true ->
                    decode(Encoder, Decoder, Iterations + 1, Flips, Drops + 1, Fun, FlipCount);
                false ->
                    Drop = {DropNum, flip(Drop0, FlipCount)},
                    case erlang_oc:decode_drop(Drop, Decoder) of
                        {error, incomplete} ->
                            decode(
                                Encoder,
                                Decoder,
                                Iterations + 1,
                                Flips + FlipCount,
                                Drops,
                                Fun,
                                FlipCount
                            );
                        {ok, Thing} ->
                            case Fun(Thing) of
                                {ok, Result} ->
                                    {Result, Iterations, Flips};
                                error ->
                                    decode(
                                        Encoder,
                                        Decoder,
                                        Iterations + 1,
                                        Flips + Flips,
                                        Drops,
                                        Fun,
                                        FlipCount
                                    )
                            end
                    end
            end
    end.

flip(Data, 0) ->
    Data;
flip(Data, NumBits) ->
    List = lists:seq(1, byte_size(Data) - 1),
    ToReplace = lists:sublist(local_shuffle(List), NumBits),

    lists:foldl(
        fun(Index, Acc) ->
            binary:replace(Acc, <<(binary:at(Acc, Index))>>, <<
                (binary:at(Acc, Index) bxor rand:uniform(2) - 1)
            >>)
        end,
        Data,
        ToReplace
    ).

-spec local_shuffle([A]) -> [A].
local_shuffle(Xs) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].

decode_recursive(ResData) ->
    case erldpc:decode_ms_tc512(ResData) of
        {ok, RR} ->
            LDPCRes = RR,
            {Decoded, _Corrected} = from_bch(LDPCRes),
            case lists:keymember(unrecoverable, 1, Decoded) of
                true ->
                    decode_recursive_soft(Decoded, ResData);
                false ->
                    Result = list_to_bitstring(lists:reverse(Decoded)),
                    {ok, Result}
            end;
        O ->
            O
    end.

decode_recursive_soft(Decoded, ResData) ->
    %% try to infer some soft bits based on bch's reporting
    Soft = to_soft(Decoded, ResData),
    case erldpc:decode_ms_soft_tc512(Soft) of
        {ok, RR2} ->
            {Decoded2, Corrected2} = from_bch(RR2),

            case lists:keymember(unrecoverable, 1, Decoded2) of
                false ->
                    Result = list_to_bitstring(lists:reverse(Decoded2)),
                    {ok, Result};
                true when Corrected2 > 0 ->
                    decode_recursive_soft(Decoded2, ResData);
                true ->
                    %% hail mary pass
                    {ok, RR3} = erldpc:decode_ms_soft_tc512(to_soft(Decoded2, ResData), 100),

                    {Decoded3, _Corrected3} = from_bch(RR3),
                    Result = list_to_bitstring(
                        lists:reverse(
                            lists:map(
                                fun
                                    ({unrecoverable, _}) ->
                                        <<0:16/integer>>;
                                    (X) ->
                                        X
                                end,
                                Decoded3
                            )
                        )
                    ),
                    {ok, Result}
            end;
        Error ->
            Error
    end.

to_soft(Decoded, ResData) ->
    lists:flatmap(
        fun
            ({unrecoverable, Chunk}) ->
                [amplify(B, 1) || <<B:1/integer>> <= <<Chunk:64/integer-unsigned-big>>];
            (<<Bits:16/integer>>) ->
                {ok, ReEncoded} = erlcode:bch_encode(Bits),
                [amplify(B, 32) || <<B:1/integer>> <= <<ReEncoded:64/integer-unsigned-big>>]
        end,
        lists:reverse(Decoded)
    ) ++
        [amplify(B, 1) || <<B:1/integer>> <= binary:part(ResData, 32, 32)].

from_bch(Bytes) ->
    lists:foldl(
        fun(Chunk, {Acc, Corrections}) ->
            case erlcode:bch_decode(Chunk) of
                {ok, {Dec, Corr}} ->
                    {[<<Dec:16/integer>> | Acc], Corrections + Corr};
                {error, {unrecoverable, _}} ->
                    {[{unrecoverable, Chunk} | Acc], Corrections}
            end
        end,
        {[], 0},
        [B || <<B:64/integer-unsigned-big>> <= Bytes]
    ).