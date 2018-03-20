use std::io;
use rust_lzo;

use super::Decompress;

struct LZODec;

pub fn decompress() -> io::Result<Box<Decompress>> {
    Ok(Box::new(LZODec))
}

impl Decompress for LZODec {
    fn decompress(&mut self, ins: &mut [u8], outs: &mut [u8]) -> io::Result<usize> {
        let (ret, err) = rust_lzo::LZOContext::decompress_to_slice(ins, outs);
        match err {
            rust_lzo::LZOError::OK =>
                Ok(ret.len()),
            rust_lzo::LZOError::ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzo: error")),
            rust_lzo::LZOError::OUT_OF_MEMORY =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzo: out of memory")),
            rust_lzo::LZOError::OUTPUT_OVERRUN =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzo: output too long")),
            _ =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzo: unknown error")),
        }
    }
}
