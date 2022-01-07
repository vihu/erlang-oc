-module(flip_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    groups/0,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2]).

-export([
        data_32_drop_4_flip_1/1,
        data_32_drop_4_flip_2/1,
        data_32_drop_4_flip_3/1,
        data_32_drop_4_flip_4/1
        ]).

groups() ->
    [{data_32,
      [],
      data_32_tests()
     }].

data_32_tests() ->
    [
        data_32_drop_4_flip_1,
        data_32_drop_4_flip_2,
        data_32_drop_4_flip_3,
        data_32_drop_4_flip_4
    ].

all() ->
    [{group, data_32}].

init_per_group(data_32, Config) ->
    EncFun = fun(Data) -> erldpc:encode_tc512(Data) end,
    DecFun = fun(Data) -> erldpc:decode_tc512(Data) end,
    [{enc_fun, EncFun}, {dec_fun, DecFun} | Config].

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_, Config) ->
    Config.

data_32_drop_4_flip_1(Config) ->
    ct:pal("DataSize: ~p, DropSize: ~p, FlipBits: ~p~n", [32, 4, 1]),
    run(32, 4, 1, Config).
data_32_drop_4_flip_2(Config) ->
    ct:pal("DataSize: ~p, DropSize: ~p, FlipBits: ~p~n", [32, 4, 2]),
    run(32, 4, 2, Config).
data_32_drop_4_flip_3(Config) ->
    ct:pal("DataSize: ~p, DropSize: ~p, FlipBits: ~p~n", [32, 4, 3]),
    run(32, 4, 3, Config).
data_32_drop_4_flip_4(Config) ->
    ct:pal("DataSize: ~p, DropSize: ~p, FlipBits: ~p~n", [32, 4, 4]),
    run(32, 4, 4, Config).

%% Helper functions

run(DataSize, DropSize, NumFlipBits, Config) ->
    EncFun = ?config(enc_fun, Config),
    DecFun = ?config(dec_fun, Config),
    Data = crypto:strong_rand_bytes(DataSize),
    StreamId = 0,
    %% LDPC encode data
    {ok, EncData} = EncFun(Data),
    {ok, Encoder} = erlang_oc:encoder(EncData, DropSize, StreamId),
    {ok, Decoder} = erlang_oc:decoder(byte_size(EncData), DropSize, StreamId),
    {Decoded, Iterations} = decode(Encoder, Decoder, 0),
    ct:pal("Iterations: ~p~n", [Iterations]),
    true = Decoded == EncData,
    %% LDPC decode data
    {ok, DecData} = DecFun(flip(Decoded, NumFlipBits)),
    true = binary:part(DecData, 0, DataSize) == Data,
    ok.

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
