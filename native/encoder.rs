use crate::bin::Bin;
use crate::iter::{IterArc, IterRes};
use online_codes::encode::OnlineCoder;
use rustler::{Env, ResourceArc};

pub struct EncoderRes {
    pub encoder: OnlineCoder,
}

pub type EncoderArc = ResourceArc<EncoderRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(EncoderRes, env);
    true
}

#[rustler::nif(name = "encode")]
pub fn encode(block_size: usize, buf: Bin, stream_id: u64) -> (EncoderArc, IterArc) {
    let encoder = OnlineCoder::new(block_size);
    let iter = encoder.encode(buf.0, stream_id);
    let res1 = EncoderArc::new(EncoderRes { encoder });
    let res2 = IterArc::new(IterRes { iter });
    (res1, res2)
}
