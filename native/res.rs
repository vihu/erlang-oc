use std::sync::{RwLock, RwLockWriteGuard};

pub struct EncoderRes(RwLock<online_codes::Encoder>);
pub struct DecoderRes(RwLock<online_codes::decode::Decoder>);

impl EncoderRes {
    // fn read(&self) -> RwLockReadGuard<'_, Encoder> {
    //     self.0.read().unwrap()
    // }

    pub fn write(&self) -> RwLockWriteGuard<'_, online_codes::Encoder> {
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

    pub fn write(&self) -> RwLockWriteGuard<'_, online_codes::decode::Decoder> {
        self.0.write().unwrap()
    }
}
