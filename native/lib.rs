use crate::bin::Bin;
use crate::res::{DecoderRes, EncoderRes};
use online_codes::types::CheckBlockId;
use online_codes::{
    decode_block, new_decoder, new_decoder_with_params, new_encoder, new_encoder_with_params,
    next_block, types::StreamId,
};
use rustler::{error::Error, Atom, Env, NifResult, ResourceArc, Term};

mod bin;
mod res;

type Drop = (u64, Bin);

rustler::atoms! {
    ok,
    incomplete,
    undefined,
}

#[rustler::nif(name = "encoder")]
pub fn encoder(
    data: Bin,
    block_size: usize,
    stream_id: StreamId,
) -> NifResult<(Atom, ResourceArc<EncoderRes>)> {
    let e = new_encoder(data.0, block_size, stream_id);
    Ok((ok(), ResourceArc::new(EncoderRes::from(e))))
}

#[rustler::nif(name = "encoder_with_params")]
pub fn encoder_with_params(
    data: Bin,
    block_size: usize,
    epsilon: f64,
    q: usize,
    stream_id: StreamId,
) -> NifResult<(Atom, ResourceArc<EncoderRes>)> {
    let e = new_encoder_with_params(data.0, block_size, epsilon, q, stream_id);
    Ok((ok(), ResourceArc::new(EncoderRes::from(e))))
}

#[rustler::nif(name = "decoder")]
pub fn decoder(
    buf_len: usize,
    block_size: usize,
    stream_id: StreamId,
) -> NifResult<(Atom, ResourceArc<DecoderRes>)> {
    let d = new_decoder(buf_len, block_size, stream_id);
    Ok((ok(), ResourceArc::new(DecoderRes::from(d))))
}

#[rustler::nif(name = "decoder_with_params")]
pub fn decoder_with_params(
    buf_len: usize,
    block_size: usize,
    epsilon: f64,
    q: usize,
    stream_id: StreamId,
) -> NifResult<(Atom, ResourceArc<DecoderRes>)> {
    let d = new_decoder_with_params(buf_len, block_size, epsilon, q, stream_id);
    Ok((ok(), ResourceArc::new(DecoderRes::from(d))))
}

#[rustler::nif(name = "next_drop")]
pub fn next_drop(enc_arc: ResourceArc<EncoderRes>) -> NifResult<(Atom, (CheckBlockId, Bin))> {
    let mut enc = enc_arc.write();
    match next_block(&mut enc) {
        Some(block) => {
            let (check_block_id, check_block) = block;
            Ok((ok(), (check_block_id, Bin(check_block))))
        }

        None => Err(Error::Term(Box::new(undefined()))),
    }
}

#[rustler::nif(name = "decode_drop")]
pub fn decode_drop(drop: Drop, dec_arc: ResourceArc<DecoderRes>) -> NifResult<(Atom, Bin)> {
    let mut dec = dec_arc.write();
    match decode_block((drop.0, (drop.1).0), &mut dec) {
        Some(data) => Ok((ok(), Bin(data))),
        None => Err(Error::Term(Box::new(incomplete()))),
    }
}

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(EncoderRes, env);
    rustler::resource!(DecoderRes, env);
    true
}

fn unload(_env: Env) {}

fn upgrade(_env: Env, _: Term) -> bool {
    true
}

rustler::init!(
    "erlang_oc",
    [
        encoder,
        encoder_with_params,
        decoder,
        decoder_with_params,
        next_drop,
        decode_drop
    ],
    load = Some(load),
    upgrade = Some(upgrade),
    unload = Some(unload)
);
