use std::{io, fmt};
use byteorder::ByteOrder;
use byteorder::LittleEndian as LE;
use rust_lzo;

use super::Decompress;
use super::Compress;

/* Define the compression flags recognised. */
//const SQUASHFS_LZO1X_1:    i32 = 0;
//const SQUASHFS_LZO1X_1_11: i32 = 1;
//const SQUASHFS_LZO1X_1_12: i32 = 2;
//const SQUASHFS_LZO1X_1_15: i32 = 3;
const SQUASHFS_LZO1X_999:  i32 = 4;

#[derive(PartialEq)]
pub struct Options {
    algorithm: i32,
    compression_level: i32,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            algorithm: SQUASHFS_LZO1X_999,
            compression_level: 8,
        }
    }
}

impl fmt::Display for Options {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "lzo algorithm={} level={}",
               self.algorithm,
               self.compression_level)
    }
}

impl Options {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn read(data: &[u8]) -> io::Result<Options> {
        match data.len() {
            0 => Ok(Options::new()),
            8 => {
                let algorithm =         LE::read_i32(&data[0..]);
                let compression_level = LE::read_i32(&data[4..]);
                Ok(Options { algorithm, compression_level })
            }
            _ => super::comp_err(),
        }
    }

    pub fn decoder(&self) -> io::Result<Box<Decompress>> {
        Ok(Box::new(LZODec))
    }

    pub fn encoder(&self, blocksize: usize) -> io::Result<Box<Compress>> {
        let mut buf = Vec::with_capacity(blocksize);
        unsafe { buf.set_len(blocksize) };
        Ok(Box::new(LZOEnc {
            ctx: rust_lzo::LZOContext::new(),
            buf: buf.into(),
        }))
    }
}

struct LZODec;

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

struct LZOEnc {
    ctx: rust_lzo::LZOContext,
    buf: Box<[u8]>,
}

impl Compress for LZOEnc {
    fn compress(&mut self, ins: &mut [u8], blocksize: usize) -> io::Result<&[u8]> {
        debug_assert!(blocksize <= self.buf.len());
        let (ret, err) = self.ctx.compress_to_slice(ins, &mut self.buf);
        match err {
            rust_lzo::LZOError::OK =>
                Ok(ret),
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
