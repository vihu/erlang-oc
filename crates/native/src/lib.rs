extern crate online_codes;

#[macro_use]
extern crate rustler;

use rustler::{Env, Error, NifResult, Term};
use rustler::resource::ResourceArc;
use rustler::types::Atom;
use online_codes::encode::{OnlineCoder, BlockIter};
use online_codes::decode::Decoder;

mod atoms {
    atoms! {
        ok,
        error
    }
}

init!(
    "erlang_oc",
    [
        encode_native,
        decoder,
        decode
    ],
    load=on_load
);

struct EncoderResource {
    pub coder: OnlineCoder,
}

struct IterResource {
    pub iter: BlockIter,
}

struct DecoderResource {
    pub decoder: Decoder,
}

fn on_load(env: Env, _info: Term) -> bool {
    resource!(EncoderResource, env);
    resource!(IterResource, env);
    resource!(DecoderResource, env);
    true
}

#[rustler::nif]
fn encode_native<'a>(
    block_size: usize,
    buf: Vec<u8>,
    stream_id: u64) -> NifResult<(Atom, (ResourceArc<EncoderResource>, ResourceArc<IterResource>))> {
    let coder = OnlineCoder::new(block_size);
    let iter = coder.encode(buf, stream_id);

    let res1 = ResourceArc::new(EncoderResource {
        coder
    });

    let res2 = ResourceArc::new(IterResource {
        iter
    });

    Ok((atoms::ok(), (res1, res2)))
}

#[rustler::nif]
fn decoder<'a>(num_blocks: usize, block_size: usize, stream_id: u64) -> NifResult<(Atom, ResourceArc<DecoderResource>)> {
    let decoder = Decoder::new(num_blocks, block_size, stream_id);
    let res = ResourceArc::new(DecoderResource {
        decoder
    });
    Ok((atoms::ok(), res))
}

#[derive(NifTuple)]
pub struct DecodeResult {
    next: Option<(ResourceArc<DecoderResource>, ResourceArc<IterResource>)>,
    result: Option<Vec<u8>>
}

#[rustler::nif]
fn decode<'a>(
    decoder_resource: ResourceArc<DecoderResource>,
    iter_resource: ResourceArc<IterResource>) -> NifResult<(Atom, DecodeResult)> {
    let mut dec = decoder_resource.decoder.clone();
    let mut iter = iter_resource.iter.clone();

    match iter.next() {
        None => {
            Err(Error::Atom("none"))
        }
        Some((block_id, block)) => {
            match dec.decode_block(block_id, &block) {
                None => {
                    let res = ResourceArc::new(DecoderResource {
                        decoder: dec
                    });

                    let res2 = ResourceArc::new(IterResource {
                        iter
                    });
                    // {ok, {NewDecoder, NewIterator}}
                    let ret = DecodeResult {next: Some((res, res2)), result: None };
                    Ok((atoms::ok(), ret))
                }
                Some(result) => {
                    // {ok, Result}
                    Ok((atoms::ok(), DecodeResult { next: None, result: Some(result) }))
                }
            }
        }
    }
}
