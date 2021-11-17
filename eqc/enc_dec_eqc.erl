-module(enc_dec_eqc).

-include_lib("eqc/include/eqc.hrl").

-export([prop_identity_match/0]).

prop_identity_match() ->
    ?FORALL(
        {Data},
        {gen_data()},
        begin
            {ok, Enc, Dec} = erlang_oc:encode_data(Data),
            {ok, Decoded} = run_no_loss(Enc, Dec),
            ?WHENFAIL(
                begin
                    io:format("Secret ~p~n", [Data])
                end,
                conjunction([
                    {enc_dec_equality, Decoded == Data}
                ])
            )
        end
    ).

gen_size() ->
    choose(5, 1*1024*1024).

gen_data() ->
    binary(gen_size()).

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
