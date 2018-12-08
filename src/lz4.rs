use std::{io, fmt};
use byteorder::ByteOrder;
use byteorder::LittleEndian as LE;
use lz4_sys;

use super::Decompress;
use super::Compress;

const LZ4_VERSION_LEGACY: i32 = 1;
//const LZ4_FLAGS_HC: i32 = 0x1;

#[derive(PartialEq)]
pub struct Options {
    version: i32,
    flags: i32,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            version: LZ4_VERSION_LEGACY,
            flags: 0,
        }
    }
}

impl fmt::Display for Options {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "lz4 version={} flags=0x{:x}",
               self.version,
               self.flags)
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
                let version = LE::read_i32(&data[0..]);
                let flags =   LE::read_i32(&data[4..]);
                Ok(Options { version, flags })
            }
            _ => super::comp_err(),
        }
    }

    pub fn decoder(&self) -> io::Result<Box<dyn Decompress>> {
        Ok(Box::new(LZ4Dec(unsafe { lz4_sys::LZ4_createStreamDecode() })))
    }

    pub fn encoder(&self, blocksize: usize) -> io::Result<Box<Compress>> {
        let bound = unsafe { lz4_sys::LZ4_compressBound(blocksize as i32) } as usize;
        let mut buf = Vec::with_capacity(bound);
        unsafe { buf.set_len(bound) };
        Ok(Box::new(LZ4Enc(buf.into())))
    }
}

struct LZ4Dec(*mut lz4_sys::LZ4StreamDecode);

impl Drop for LZ4Dec {
    fn drop(&mut self) {
        unsafe { lz4_sys::LZ4_freeStreamDecode(self.0) };
    }
}

impl Decompress for LZ4Dec {
    fn decompress(&mut self, ins: &mut [u8], outs: &mut [u8]) -> io::Result<usize> {
        let ret = unsafe {
            lz4_sys::LZ4_decompress_safe_continue(self.0, &ins[0], &mut outs[0],
                                                  ins.len() as i32, outs.len() as i32)
        };
        if ret < 0 {
            unsafe { lz4_sys::LZ4_freeStreamDecode(self.0) };
            self.0 = unsafe { lz4_sys::LZ4_createStreamDecode() };
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "lz4: decoding error"));
        }
        Ok(ret as usize)
    }
}

struct LZ4Enc(Box<[u8]>);

impl Compress for LZ4Enc {
    fn compress(&mut self, ins: &mut [u8], blocksize: usize) -> io::Result<&[u8]> {
        debug_assert!(unsafe { lz4_sys::LZ4_compressBound(blocksize as i32) } as usize <= self.0.len());
        let ret;
        unsafe {
            let strm = lz4_sys::LZ4_createStream();
            ret = lz4_sys::LZ4_compress_continue(strm, &ins[0], &mut self.0[0],
                                           ins.len() as i32);
            lz4_sys::LZ4_freeStream(strm);
        };
        if ret <= 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "lz4: encoding error"
            ));
        }
        Ok(&self.0[..(ret as usize)])
    }
}
