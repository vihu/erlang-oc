use crate::bin::Bin;
use online_codes::{
    decode_block, new_decoder, new_decoder_with_params, new_encoder, new_encoder_with_params,
    next_block, types::StreamId,
};
use rustler::{Encoder, Env, NifResult, ResourceArc, Term};
use std::sync::{RwLock, RwLockWriteGuard};

mod bin;

rustler::atoms! {
    ok,
    error,
    incomplete,
    undefined,
}

pub struct EncoderRes(RwLock<online_codes::Encoder>);
pub struct DecoderRes(RwLock<online_codes::decode::Decoder>);

type Drop = (u64, Bin);

impl EncoderRes {
    // fn read(&self) -> RwLockReadGuard<'_, Encoder> {
    //     self.0.read().unwrap()
    // }

    fn write(&self) -> RwLockWriteGuard<'_, online_codes::Encoder> {
        self.0.write().unwrap()
    }
}

impl From<online_codes::Encoder> for EncoderRes {
    fn from(other: online_codes::Encoder) -> Self {
        EncoderRes(RwLock::new(other))
    }
}

impl From<online_codes::decode::Decoder> for DecoderRes {
    fn from(other: online_codes::decode::Decoder) -> Self {
        DecoderRes(RwLock::new(other))
    }
}

impl DecoderRes {
    // fn read(&self) -> RwLockReadGuard<'_, Decoder> {
    //     self.0.read().unwrap()
    // }

    fn write(&self) -> RwLockWriteGuard<'_, online_codes::decode::Decoder> {
        self.0.write().unwrap()
    }
}

#[rustler::nif(name = "encoder")]
pub fn encoder<'a>(
    env: Env<'a>,
    data: Bin,
    block_size: usize,
    stream_id: StreamId,
) -> NifResult<Term<'a>> {
    let e = new_encoder(data.0, block_size, stream_id);
    Ok((ok(), ResourceArc::new(EncoderRes::from(e))).encode(env))
}

#[rustler::nif(name = "encoder_with_params")]
pub fn encoder_with_params<'a>(
    env: Env<'a>,
    data: Bin,
    block_size: usize,
    epsilon: f64,
    q: usize,
    stream_id: StreamId,
) -> NifResult<Term<'a>> {
    let e = new_encoder_with_params(data.0, block_size, epsilon, q, stream_id);
    Ok((ok(), ResourceArc::new(EncoderRes::from(e))).encode(env))
}

#[rustler::nif(name = "decoder")]
pub fn decoder<'a>(
    env: Env<'a>,
    buf_len: usize,
    block_size: usize,
    stream_id: StreamId,
) -> NifResult<Term<'a>> {
    let d = new_decoder(buf_len, block_size, stream_id);
    Ok((ok(), ResourceArc::new(DecoderRes::from(d))).encode(env))
}

#[rustler::nif(name = "decoder_with_params")]
pub fn decoder_with_params<'a>(
    env: Env<'a>,
    buf_len: usize,
    block_size: usize,
    epsilon: f64,
    q: usize,
    stream_id: StreamId,
) -> NifResult<Term<'a>> {
    let d = new_decoder_with_params(buf_len, block_size, epsilon, q, stream_id);
    Ok((ok(), ResourceArc::new(DecoderRes::from(d))).encode(env))
}

#[rustler::nif(name = "next_drop")]
pub fn next_drop<'a>(env: Env<'a>, enc_arc: ResourceArc<EncoderRes>) -> NifResult<Term<'a>> {
    let mut enc = enc_arc.write();
    match next_block(&mut enc) {
        Some(block) => {
            let (check_block_id, check_block) = block;
            Ok((ok(), (check_block_id, Bin(check_block))).encode(env))
        }

        None => Ok(undefined().encode(env)),
    }
}

#[rustler::nif(name = "decode_drop")]
pub fn decode_drop<'a>(
    env: Env<'a>,
    drop: Drop,
    dec_arc: ResourceArc<DecoderRes>,
) -> NifResult<Term<'a>> {
    let mut dec = dec_arc.write();
    match decode_block((drop.0, (drop.1).0), &mut dec) {
        Some(data) => Ok((ok(), Bin(data)).encode(env)),
        None => Ok((error(), incomplete()).encode(env)),
    }
}

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(EncoderRes, env);
    rustler::resource!(DecoderRes, env);
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
    load = load
);
