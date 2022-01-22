-module(encode_decode_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_check_bch/0]).

prop_check_bch() ->
    ?FORALL(
        {Data, FlipCount, Epsilon, Q},
        {gen_bytes8(), gen_flips(), gen_epsilon(), gen_q()},
        ?SOMETIMES(
            500,
            begin
                BlockSize = 4,
                StreamId = 0,
                {Check, Iterations, Flips, Header, BestMatch} =
                    erlang_oc_util:execute(Data, Epsilon, Q, BlockSize, StreamId, FlipCount),

                write_to_file(Check, Q, Epsilon, Iterations, Flips),

                ?WHENFAIL(
                    begin
                        io:format("Data: ~p~n", [Data]),
                        io:format("FlipCount: ~p~n", [FlipCount]),
                        io:format("Header: ~p~n", [Header]),
                        io:format("BestMatch: ~p~n", [BestMatch])
                    end,
                    conjunction([{verify_enc_dec, Check}])
                )
            end
        )
    ).

gen_flips() ->
    elements([0, 1, 2]).

gen_bytes8() ->
    binary(8).

gen_epsilon() ->
    elements([0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]).

gen_q() ->
    elements([3, 4, 5, 6, 7, 8, 9, 10]).

%% ==================================================================
%% Internal Functions
%% ==================================================================

write_to_file(Check, Q, Epsilon, Iterations, Flips) ->
    Fname = "/tmp/eqc.json",
    {ok, FileHandle} = file:open(Fname, [write, append]),
    ToAppend = #{
        <<"check">> => Check,
        <<"q">> => Q,
        <<"epsilon">> => Epsilon,
        <<"iterations">> => Iterations,
        <<"flips">> => Flips
    },
    ok = file:write(FileHandle, jsx:encode(ToAppend)),
    ok = file:write(FileHandle, ","),
    ok = file:write(FileHandle, io_lib:nl()),
    ok = file:close(FileHandle),
    ok.
