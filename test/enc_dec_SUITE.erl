-module(enc_dec_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export(
    [
        identity_test/1,
        loss_ten_pct_test/1,
        loss_thirty_pct_test/1,
        loss_fifty_pct_test/1,
        loss_ninety_pct_test/1,
        long_running_test/1
    ]
).

all() ->
    [
        identity_test,
        loss_ten_pct_test,
        loss_thirty_pct_test,
        loss_fifty_pct_test,
        loss_ninety_pct_test,
        long_running_test
    ].

init_per_testcase(TestCase, Config) ->
    Loss =
        case TestCase of
            loss_ten_pct_test -> 0.1;
            loss_thirty_pct_test -> 0.3;
            loss_fifty_pct_test -> 0.5;
            loss_ninety_pct_test -> 0.9;
            _ -> 0.0
        end,
    BufLen = 1024,
    Data = crypto:strong_rand_bytes(BufLen),
    BlockSize = 4,
    StreamID = 0,
    [
        {loss, Loss},
        {data, Data},
        {buf_len, BufLen},
        {block_size, BlockSize},
        {stream_id, StreamID}
        | Config
    ].

end_per_testcase(_, Config) ->
    Config.

identity_test(Config) ->
    Data = ?config(data, Config),
    BufLen = ?config(buf_len, Config),
    BlockSize = ?config(block_size, Config),
    StreamID = ?config(stream_id, Config),
    {ok, Enc} = erlang_oc:encoder_new(Data, BlockSize, StreamID),
    {ok, Dec} = erlang_oc:decoder_new(BufLen, BlockSize, StreamID),
    {ok, Decoded} = run_no_loss(Enc, Dec),
    true = Decoded == Data,
    ok.

long_running_test(_Config) ->
    %% Byte size should be a multiple of 4?
    BlockSizes = lists:seq(4, 1600),
    Data = [{BlockSize, lists:seq(16, 1024, BlockSize)} || BlockSize <- BlockSizes],
    true = lists:all(
        fun({BlockSize, Sizes}) ->
            run_long_running_test(BlockSize, Sizes, true)
        end,
        Data
    ),
    ok.

loss_ten_pct_test(Config) ->
    run_loss_test(Config).

loss_thirty_pct_test(Config) ->
    run_loss_test(Config).

loss_fifty_pct_test(Config) ->
    run_loss_test(Config).

loss_ninety_pct_test(Config) ->
    run_loss_test(Config).

%% Helper functions

run_no_loss(Enc, Dec) ->
    case erlang_oc:next_drop(Enc) of
        {ok, Block} ->
            case erlang_oc:decode_drop(Block, Dec) of
                {ok, Data} -> {ok, Data};
                {error, incomplete} -> run_no_loss(Enc, Dec)
            end;
        _ ->
            run_no_loss(Enc, Dec)
    end.

run_with_loss(Enc, Dec, Loss) ->
    case erlang_oc:next_drop(Enc) of
        {ok, Block} ->
            case rand:uniform() > Loss of
                true ->
                    %% don't decode this block
                    run_with_loss(Enc, Dec, Loss);
                _ ->
                    case erlang_oc:decode_drop(Block, Dec) of
                        {ok, Data} -> {ok, Data};
                        {error, incomplete} -> run_no_loss(Enc, Dec)
                    end
            end;
        _ ->
            run_with_loss(Enc, Dec, Loss)
    end.

run_loss_test(Config) ->
    Data = ?config(data, Config),
    BufLen = ?config(buf_len, Config),
    BlockSize = ?config(block_size, Config),
    StreamID = ?config(stream_id, Config),
    Loss = ?config(loss, Config),
    {ok, Enc} = erlang_oc:encoder_new(Data, BlockSize, StreamID),
    {ok, Dec} = erlang_oc:decoder_new(BufLen, BlockSize, StreamID),
    {ok, Decoded} = run_with_loss(Enc, Dec, Loss),
    true = Decoded == Data,
    ok.

run_long_running_test(_BlockSize, [], true) ->
    true;
run_long_running_test(BlockSize, [Size | Tail], true) ->
    Data = crypto:strong_rand_bytes(Size),
    {ok, Enc} = erlang_oc:encoder_new(Data, BlockSize, 0),
    {ok, Dec} = erlang_oc:decoder_new(Size, BlockSize, 0),
    {ok, Decoded} = run_no_loss(Enc, Dec),
    case Decoded == Data of
        true ->
            ct:pal("BlockSize: ~p BufSize: ~p", [BlockSize, Size]),
            run_long_running_test(BlockSize, Tail, true);
        false ->
            false
    end.
