use crate::atom;
use crate::iter::{IterArc, IterRes};
use online_codes::decode::Decoder;
use rustler::ResourceArc;
use rustler::{Encoder, Env, Term};

pub struct DecoderRes {
    pub decoder: Decoder,
}

pub type DecoderArc = ResourceArc<DecoderRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(DecoderRes, env);
    true
}

#[rustler::nif(name = "decoder")]
pub fn decoder(num_blocks: usize, block_size: usize, stream_id: u64) -> DecoderArc {
    let decoder = Decoder::new(num_blocks, block_size, stream_id);
    DecoderArc::new(DecoderRes { decoder })
}

#[rustler::nif(name = "decode")]
pub fn decode<'a>(env: Env<'a>, decoder_arc: DecoderArc, iter_arc: IterArc) -> Term<'a> {
    let mut mdecoder = decoder_arc.decoder.clone();
    let mut miter = iter_arc.iter.clone();

    match miter.next() {
        None => (atom::error().encode(env)),
        Some((block_id, block)) => {
            match mdecoder.decode_block(block_id, &block) {
                None => {
                    let res1 = DecoderArc::new(DecoderRes { decoder: mdecoder });
                    let res2 = IterArc::new(IterRes { iter: miter });
                    // {ok, {NewDecoder, NewIterator}}
                    (res1, res2).encode(env)
                }
                Some(result) => result.encode(env),
            }
        }
    }
}
