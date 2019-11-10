extern crate online_codes;

extern crate rustler;

use rustler::{Encoder, Env, Error, Term};
use rustler::resource::ResourceArc;
use online_codes::encode::{OnlineCoder, BlockIter};
use online_codes::decode::Decoder;

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
        atom error;
    }
}

rustler::rustler_export_nifs!(
    "erlang_oc",
    [
        ("encode_native", 3, encode_native),
        ("decoder", 3, decoder),
        ("decode", 2, decode)
    ],
    Some(on_load)
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
    rustler::resource_struct_init!(EncoderResource, env);
    rustler::resource_struct_init!(IterResource, env);
    rustler::resource_struct_init!(DecoderResource, env);
    true
}

fn encode_native<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let block_size: usize = args[0].decode()?;
    let buf: Vec<u8> = args[1].decode()?;
    let stream_id: u64 = args[2].decode()?;
    let coder = OnlineCoder::new(block_size);
    let iter = coder.encode(buf, stream_id);

    let res1 = ResourceArc::new(EncoderResource {
        coder: coder,
    });

    let res2 = ResourceArc::new(IterResource {
        iter: iter,
    });

    Ok((atoms::ok(), (res1, res2)).encode(env))
}

fn decoder<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let num_blocks: usize = args[0].decode()?;
    let block_size: usize = args[1].decode()?;
    let stream_id: u64 = args[2].decode()?;
    let decoder = Decoder::new(num_blocks, block_size, stream_id);
    let res = ResourceArc::new(DecoderResource {
        decoder: decoder,
    });
    Ok((atoms::ok(), res).encode(env))
}

fn decode<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let decoder_resource: ResourceArc<DecoderResource> = args[0].decode()?;
    let iter_resource: ResourceArc<IterResource> = args[1].decode()?;

    let mut decoder = decoder_resource.decoder.clone();
    let mut iter = iter_resource.iter.clone();

    match iter.next() {
        None => {
            Ok((atoms::error()).encode(env))
        }
        Some((block_id, block)) => {
            match decoder.decode_block(block_id, &block) {
                None => {
                    let res = ResourceArc::new(DecoderResource {
                        decoder: decoder,
                    });

                    let res2 = ResourceArc::new(IterResource {
                        iter: iter,
                    });
                    // {ok, {NewDecoder, NewIterator}}
                    Ok((atoms::ok(), (res, res2)).encode(env))
                }
                Some(result) => {
                    // {ok, Result}
                    Ok((atoms::ok(), result).encode(env))
                }
            }
        }
    }
}
