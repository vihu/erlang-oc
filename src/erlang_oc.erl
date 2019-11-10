-module(erlang_oc).

%% API
-export([encode/3, decoder/3, decode/2]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-spec encode(BlockSize :: pos_integer(),
             Data :: binary(),
             StreamId :: non_neg_integer())-> {ok, reference()} | {error, any()}.
encode(BlockSize, Data, StreamId) ->
    encode_native(BlockSize, binary:bin_to_list(Data), StreamId).

-spec decoder(Coder :: reference(),
              NumBlocks :: pos_integer(),
              StreamId :: non_neg_integer()) -> {ok, reference()} | {error, any()}.
decoder(_Coder, _NumBlocks, _StreamId) ->
    not_loaded(?LINE).

-spec decode(Decoder :: reference(),
             Iterator :: reference()) -> error | {ok, list()} | {ok, {reference(), reference()}}.
decode(_Decoder, _Iterator) ->
    not_loaded(?LINE).

encode_native(_, _, _) ->
    not_loaded(?LINE).

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv()->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.

