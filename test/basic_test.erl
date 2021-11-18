-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

identity_test() ->
    Data = crypto:strong_rand_bytes(1024),
    {ok, Enc} = erlang_oc:encoder_new(Data, 4, 0),
    {ok, Dec} = erlang_oc:decoder_new(1024, 4, 0),
    {ok, Decoded} = run(Enc, Dec),
    ?assertEqual(Decoded, Data).

run(Enc, Dec) ->
    case erlang_oc:next_drop(Enc) of
        {ok, Block} ->
            case erlang_oc:decode_drop(Block, Dec) of
                {ok, Data} -> {ok, Data};
                {error, incomplete} -> run(Enc, Dec)
            end;
        _ ->
            run(Enc, Dec)
    end.
