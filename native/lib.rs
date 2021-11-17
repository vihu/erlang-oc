use crate::bin::Bin;
use online_codes::decode::Decoder;
use online_codes::{decode_block, encode, next_block, Encoder};
use rustler::{Encoder as Enc, Env, NifResult, ResourceArc, Term};
// use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};
use std::sync::{RwLock, RwLockWriteGuard};

mod bin;

rustler::atoms! {
    ok,
    error,
    incomplete,
    undefined,
}

pub struct EncoderResource(RwLock<Encoder>);

// Prefer the erlang style Bin for drops
type Drop = (u64, Bin);

impl EncoderResource {
    // fn read(&self) -> RwLockReadGuard<'_, Encoder> {
    //     self.0.read().unwrap()
    // }

    fn write(&self) -> RwLockWriteGuard<'_, Encoder> {
        self.0.write().unwrap()
    }
}

impl From<Encoder> for EncoderResource {
    fn from(other: Encoder) -> Self {
        EncoderResource(RwLock::new(other))
    }
}

pub struct DecoderResource(RwLock<Decoder>);

impl From<Decoder> for DecoderResource {
    fn from(other: Decoder) -> Self {
        DecoderResource(RwLock::new(other))
    }
}

impl DecoderResource {
    // fn read(&self) -> RwLockReadGuard<'_, Decoder> {
    //     self.0.read().unwrap()
    // }

    fn write(&self) -> RwLockWriteGuard<'_, Decoder> {
        self.0.write().unwrap()
    }
}

#[rustler::nif(name = "encode_data")]
pub fn encode_data<'a>(env: Env<'a>, data: Bin) -> NifResult<Term<'a>> {
    let (e, d) = encode(data.0);
    Ok((
        ok(),
        ResourceArc::new(EncoderResource::from(e)),
        ResourceArc::new(DecoderResource::from(d)),
    )
        .encode(env))
}

#[rustler::nif(name = "next_drop")]
pub fn next_drop<'a>(env: Env<'a>, enc_arc: ResourceArc<EncoderResource>) -> NifResult<Term<'a>> {
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
    dec_arc: ResourceArc<DecoderResource>,
) -> NifResult<Term<'a>> {
    let mut dec = dec_arc.write();
    match decode_block((drop.0, (drop.1).0), &mut dec) {
        Some(data) => Ok((ok(), Bin(data)).encode(env)),
        None => Ok((error(), incomplete()).encode(env)),
    }
}

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(EncoderResource, env);
    rustler::resource!(DecoderResource, env);
    true
}

rustler::init!(
    "erlang_oc",
    [encode_data, next_drop, decode_drop],
    load = load
);
