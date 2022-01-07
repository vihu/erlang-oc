-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    BufLen = 128,
    BlockSize = BufLen div 16,
    Data = crypto:strong_rand_bytes(BufLen),
    StreamId = 0,
    %% LDPC encode data
    {ok, EncData} = erldpc:encode_tm1280(Data),
    {ok, Encoder} = erlang_oc:encoder(EncData, BlockSize, StreamId),
    {ok, Decoder} = erlang_oc:decoder(byte_size(EncData), BlockSize, StreamId),
    DecodeFun = fun(ResData) ->
                        case erldpc:decode_ms_tm1280(ResData) of
                            {ok, RR} ->
                                case binary:part(RR, 0, BufLen) == Data of
                                    true ->
                                        {ok, RR};
                                    false ->
                                        error
                                end;
                            O -> O
                        end
                end,

    {Decoded, Iterations} = decode(Encoder, Decoder, 0, DecodeFun),
    io:format("Iterations: ~p~n", [Iterations]),
    %?assertEqual(Decoded, EncData),
    %% LDPC decode data
    ?assertEqual(binary:part(Decoded, 0, BufLen), Data).

%% convenience driver function
decode(Encoder, Decoder, Iterations, Fun) ->
    case erlang_oc:next_drop(Encoder) of
        undefined ->
            io:format(user, "ran out of droplets~n", []),
            decode(Encoder, Decoder, Iterations + 1, Fun);
        {ok, {DropNum, Drop0}} ->
            %io:format(user, "drop size is ~p~n", [byte_size(Drop0)]),
            Drop = {DropNum, flip(Drop0, max(0, rand:uniform(20) - 19))},
            case erlang_oc:decode_drop(Drop, Decoder) of
                {error, incomplete} ->
                    %io:format(user, "got incomplete~n", []),
                    decode(Encoder, Decoder, Iterations + 1, Fun);
                {ok, Thing} ->
                    case Fun(Thing) of
                        {ok, Result} ->
                            io:format(user, "got thing ~p~n", [Iterations]),
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
    io:format(user, "flipping ~p bits~n", [NumBits]),
    List = lists:seq(1, byte_size(Data) - 1),
    ToReplace = lists:sublist(shuffle(List), NumBits),

    lists:foldl(
      fun(Index, Acc) ->
              binary:replace(Acc, <<(binary:at(Acc, Index))>>, <<(binary:at(Acc, Index) bxor 1)>>)
      end, Data, ToReplace).

-spec shuffle([A]) -> [A].
shuffle(Xs) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), X} || X <- Xs])].
