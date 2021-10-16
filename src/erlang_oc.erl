-module(erlang_oc).

%% API
-export([encode/3, decoder/3, decode/2]).

%% Native lib support
-export([load/0]).
-on_load(load/0).

%% ==================================================================
%% Encoder
%% ==================================================================

-spec encode(BlockSize :: pos_integer(),
             Data :: binary(),
             StreamId :: non_neg_integer())-> {ok, reference()} | {error, any()}.
encode(_BlockSize, _Data, _StreamId) ->
    not_loaded(?LINE).

%% ==================================================================
%% Decoder
%% ==================================================================

-spec decoder(NumBlocks :: pos_integer(),
              BlockSize :: pos_integer(),
              StreamId :: non_neg_integer()) -> {ok, reference()} | {error, any()}.
decoder(_Coder, _NumBlocks, _StreamId) ->
    not_loaded(?LINE).

-spec decode(Decoder :: reference(),
             Iterator :: reference()) -> error | {ok, list()} | {ok, {reference(), reference()}}.
decode(_Decoder, _Iterator) ->
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

