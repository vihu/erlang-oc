-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

identity_test() ->
    Data = crypto:strong_rand_bytes(1024),
    {ok, Enc, Dec} = erlang_oc:encode_data(Data),
    {ok, Decoded} = run(Enc, Dec, ok),
    ?assertEqual(Decoded, Data).

run(Enc, Dec, ok) ->
    case erlang_oc:next_drop(Enc) of
        {ok, Block} ->
            case erlang_oc:decode_drop(Block, Dec) of
                {ok, Data} -> {ok, Data};
                {error, incomplete} -> run(Enc, Dec, ok)
            end;
        _ ->
            run(Enc, Dec, ok)
    end.
