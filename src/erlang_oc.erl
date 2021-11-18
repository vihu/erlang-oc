-module(erlang_oc).

%% API
-export([encoder_new/3, decoder_new/3, next_drop/1, decode_drop/2]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-type block() :: {non_neg_integer(), binary()}.

%% ==================================================================
%% Encoder
%% ==================================================================

-spec encoder_new(
    Data :: binary(),
    BlockSize :: pos_integer(),
    StreamID :: pos_integer()
) -> {ok, Encoder :: reference()}.
encoder_new(_Data, _BlockSize, _StreamID) ->
    not_loaded(?LINE).

-spec decoder_new(
    BufLen :: pos_integer(),
    BlockSize :: pos_integer(),
    StreamID :: pos_integer()
) -> {ok, Encoder :: reference()}.
decoder_new(_BufLen, _BlockSize, _StreamID) ->
    not_loaded(?LINE).

-spec next_drop(Encoder :: reference()) -> {ok, block()} | undefined.
next_drop(_Encoder) ->
    not_loaded(?LINE).

-spec decode_drop(Block :: block(), Decoder :: reference()) ->
    {ok, Data :: binary()} | {error, incomplete}.
decode_drop(_Block, _Decoder) ->
    not_loaded(?LINE).

%% ==================================================================
%% NIF
%% ==================================================================

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.
