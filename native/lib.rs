extern crate online_codes;
extern crate rustler;

use rustler::{Env, Term};

mod atom;
mod bin;
mod decoder;
mod encoder;
mod iter;

fn load(env: Env, _: Term) -> bool {
    encoder::load(env);
    decoder::load(env);
    iter::load(env);
    true
}

rustler::init!(
    "erlang_oc",
    [encoder::encode, decoder::decode, decoder::decoder],
    load = load
);
