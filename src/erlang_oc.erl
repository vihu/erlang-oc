-module(erlang_oc).

%% API
-export([encode_data/1, next_drop/1, decode_block/2]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-type block() :: {non_neg_integer(), binary()}.

%% ==================================================================
%% Encoder
%% ==================================================================

-spec encode_data(Data :: binary())-> {Encoder :: reference(), Decoder :: reference()}.
encode_data(_Data) ->
    not_loaded(?LINE).

-spec next_drop(Encoder :: reference()) -> block() | undefined.
next_drop(_Encoder) ->
    not_loaded(?LINE).

-spec decode_block(Block :: block(), Decoder :: reference()) -> Data :: binary() | undefined.
decode_block(_Block, _Decoder) ->
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

