use online_codes::encode::BlockIter;
use rustler::{Env, ResourceArc};

pub struct IterRes {
    pub iter: BlockIter,
}

pub type IterArc = ResourceArc<IterRes>;

pub fn load(env: Env) -> bool {
    rustler::resource!(IterRes, env);
    true
}
