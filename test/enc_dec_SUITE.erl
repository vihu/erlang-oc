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
    [{loss, Loss}, {data, Data}, {block_size, BufLen div 4}, {buf_len, BufLen} | Config].

end_per_testcase(_, Config) ->
    Config.

identity_test(Config) ->
    Data = ?config(data, Config),
    BlockSize = ?config(block_size, Config),
    BufLen = ?config(buf_len, Config),
    {ok, Enc} = erlang_oc:encoder(Data, BlockSize, 0),
    {ok, Dec} = erlang_oc:decoder(BufLen, BlockSize, 0),
    {ok, Decoded} = run_no_loss(Enc, Dec),
    true = Decoded == Data,
    ok.

long_running_test(_Config) ->
    %% Byte size should be a multiple of 4?
    Sizes = lists:seq(4 * 1024, 4 * 100 * 1024, 256),
    true = run_long_running_test(Sizes, true),
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
    BlockSize = ?config(block_size, Config),
    BufLen = ?config(buf_len, Config),
    Data = ?config(data, Config),
    Loss = ?config(loss, Config),
    {ok, Enc} = erlang_oc:encoder(Data, BlockSize, 0),
    {ok, Dec} = erlang_oc:decoder(BufLen, BlockSize, 0),
    {ok, Decoded} = run_with_loss(Enc, Dec, Loss),
    true = Decoded == Data,
    ok.

run_long_running_test([], true) ->
    true;
run_long_running_test([Size | Tail], true) ->
    Data = crypto:strong_rand_bytes(Size),
    {ok, Enc} = erlang_oc:encoder(Data, Size, 0),
    {ok, Dec} = erlang_oc:decoder(byte_size(Data), Size, 0),
    {ok, Decoded} = run_no_loss(Enc, Dec),
    case Decoded == Data of
        true ->
            ct:pal("Size: ~p Match!", [Size]),
            run_long_running_test(Tail, true);
        false -> false
    end.
