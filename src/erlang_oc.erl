-module(erlang_oc).

%% API
-export([encode_data/1, next_drop/1, decode_drop/2]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

-type block() :: {non_neg_integer(), binary()}.

%% ==================================================================
%% Encoder
%% ==================================================================

-spec encode_data(Data :: binary())-> {ok, Encoder :: reference(), Decoder :: reference()}.
encode_data(_Data) ->
    not_loaded(?LINE).

-spec next_drop(Encoder :: reference()) -> {ok, block()} | undefined.
next_drop(_Encoder) ->
    not_loaded(?LINE).

-spec decode_drop(Block :: block(), Decoder :: reference()) -> {ok, Data :: binary()} | {error, incomplete}.
decode_drop(_Block, _Decoder) ->
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

