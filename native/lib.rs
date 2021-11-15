use crate::bin::Bin;
use online_codes::{decode_block, encode, next_block, Block};
use rustler::{Env, ResourceArc, Term};

mod bin;

pub struct Encoder {
    pub enc: online_codes::Encoder,
}

pub struct Decoder {
    pub dec: online_codes::decode::Decoder,
}

pub type EncoderArc = ResourceArc<Encoder>;
pub type DecoderArc = ResourceArc<Decoder>;

#[rustler::nif(name = "encode_data")]
pub fn encode_data<'a>(data: Bin) -> (EncoderArc, DecoderArc) {
    let (e, d) = encode(data.0);
    let enc = Encoder { enc: e };
    let dec = Decoder { dec: d };
    (EncoderArc::new(enc), DecoderArc::new(dec))
}

#[rustler::nif(name = "next_drop")]
pub fn next_drop<'a>(enc_arc: EncoderArc) -> Option<Block> {
    next_block(&mut enc_arc.enc)
}

#[rustler::nif(name = "decode_drop")]
pub fn decode_drop<'a>(block: Block, dec_arc: DecoderArc) -> Option<Vec<u8>> {
    decode_block(block, &mut dec_arc.dec)
}

fn load(env: Env, _: Term) -> bool {
    rustler::resource!(Encoder, env);
    rustler::resource!(Decoder, env);
    true
}

rustler::init!(
    "erlang_oc",
    [encode_data, next_drop, decode_drop],
    load = load
);
