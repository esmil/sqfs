use std::{io, mem};
use libz_sys;

use super::Decompress;

struct ZLibDec(libz_sys::z_stream);

pub fn decompress() -> io::Result<Box<Decompress>> {
    let mut dec = Box::new(ZLibDec(unsafe { mem::zeroed() }));

    let ret = unsafe {
        libz_sys::inflateInit_(&mut dec.0, libz_sys::zlibVersion(), mem::size_of::<libz_sys::z_stream>() as i32)
    };
    match ret {
        libz_sys::Z_OK =>
            Ok(dec),
        libz_sys::Z_MEM_ERROR =>
            Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: not enough memory")),
        libz_sys::Z_VERSION_ERROR =>
            Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: version mismatch")),
        libz_sys::Z_STREAM_ERROR =>
            Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: invalid parameters")),
        _ =>
            Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: unknown error")),
    }
}

impl Drop for ZLibDec {
    fn drop(&mut self) {
        unsafe { libz_sys::inflateEnd(&mut self.0) };
    }
}

impl Decompress for ZLibDec {
    fn decompress(&mut self, ins: &mut [u8], outs: &mut [u8]) -> io::Result<usize> {
        self.0.next_in = &mut ins[0];
        self.0.avail_in = ins.len() as u32;
        self.0.next_out = &mut outs[0];
        self.0.avail_out = outs.len() as u32;

        let mut ret = unsafe {
            libz_sys::inflateReset(&mut self.0)
        };
        if ret == libz_sys::Z_OK {
            ret = unsafe {
                libz_sys::inflate(&mut self.0, libz_sys::Z_FINISH)
            };
        }
        match ret {
            libz_sys::Z_STREAM_END =>
                Ok(self.0.total_out as usize),
            libz_sys::Z_NEED_DICT =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: need preset dictionary")),
            libz_sys::Z_DATA_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: corrupted input data")),
            libz_sys::Z_STREAM_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: invalid stream")),
            libz_sys::Z_MEM_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: not enough memory")),
            libz_sys::Z_BUF_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: no progress possible/output too long")),
            _ =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "zlib: unknown error")),
        }
    }
}
