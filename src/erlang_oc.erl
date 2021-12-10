-module(erlang_oc).

%% API
-export([encoder/3, decoder/3, next_drop/1, decode_drop/2]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-type drop() :: {non_neg_integer(), binary()}.

%% ==================================================================
%% Encoder
%% ==================================================================

-spec encoder(Data :: binary(), BlockSize :: pos_integer(), StreamID :: non_neg_integer())-> Encoder :: reference().
encoder(_Data, _BlockSize, _StreamID) ->
    not_loaded(?LINE).

-spec decoder(BufLen :: non_neg_integer(), BlockSize :: pos_integer(), StreamID :: non_neg_integer())-> Decoder :: reference().
decoder(_BufLen, _BlockSize, _StreamID) ->
    not_loaded(?LINE).

-spec next_drop(Encoder :: reference()) -> drop() | undefined.
next_drop(_Encoder) ->
    not_loaded(?LINE).

-spec decode_drop(Drop :: drop(), Decoder :: reference()) -> Data :: binary() | undefined.
decode_drop(_Drop, _Decoder) ->
    not_loaded(?LINE).

%% ==================================================================
%% NIF
%% ==================================================================

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
