use std::{io, mem};
use lzma_sys;

use super::Decompress;

const MEMLIMIT: u64 = 32 * 1024 * 1024;

struct XZDec(lzma_sys::lzma_stream);

pub fn decompress() -> io::Result<Box<Decompress>> {
    Ok(Box::new(XZDec(unsafe { mem::zeroed() })))
}

impl Drop for XZDec {
    fn drop(&mut self) {
        unsafe { lzma_sys::lzma_end(&mut self.0) };
    }
}

impl Decompress for XZDec {
    fn decompress(&mut self, ins: &mut [u8], outs: &mut [u8]) -> io::Result<usize> {
        self.0.next_in = &ins[0];
        self.0.avail_in = ins.len();
        self.0.next_out = &mut outs[0];
        self.0.avail_out = outs.len();

        let mut ret = unsafe {
            lzma_sys::lzma_stream_decoder(&mut self.0, MEMLIMIT, 0)
        };
        if ret == lzma_sys::LZMA_OK {
            ret = unsafe {
                lzma_sys::lzma_code(&mut self.0, lzma_sys::LZMA_FINISH)
            };
        }
        match ret {
            lzma_sys::LZMA_STREAM_END =>
                Ok(self.0.total_out as usize),
            lzma_sys::LZMA_FORMAT_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "xz: format error")),
            lzma_sys::LZMA_OPTIONS_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "xz: options error")),
            lzma_sys::LZMA_DATA_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "xz: data error")),
            lzma_sys::LZMA_MEM_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "xz: memory error")),
            lzma_sys::LZMA_MEMLIMIT_ERROR =>
                // memory usage limit was reached
                // the minimum required memlimit value was stored to memlimit
                Err(io::Error::new(io::ErrorKind::InvalidInput, "xz: memory limit error")),
            lzma_sys::LZMA_BUF_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "xz: out buffer too small")),
            lzma_sys::LZMA_PROG_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "xz: program error")),
            _ =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "xz: unknown error")),
        }
    }
}
