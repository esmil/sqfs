use std::{io, mem, fmt};
use lzma_sys;

use super::Decompress;

const LZMA_PROPS_SIZE:  usize = 5;
const LZMA_UNCOMP_SIZE: usize = 8;
const LZMA_HEADER_SIZE: usize = LZMA_PROPS_SIZE + LZMA_UNCOMP_SIZE;

const MEMLIMIT: u64 = 32 * 1024 * 1024;

#[derive(PartialEq)]
pub struct Options;

impl fmt::Display for Options {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "lzma")
    }
}

impl Options {
    pub fn read(data: &[u8]) -> io::Result<Options> {
        match data.len() {
            0 => Ok(Options),
            _ => super::comp_err(),
        }
    }

    pub fn decoder(&self) -> io::Result<Box<Decompress>> {
        Ok(Box::new(LZMADec(unsafe { mem::zeroed() })))
    }
}

struct LZMADec(lzma_sys::lzma_stream);

impl Drop for LZMADec {
    fn drop(&mut self) {
        unsafe { lzma_sys::lzma_end(&mut self.0) };
    }
}

impl Decompress for LZMADec {
    fn decompress(&mut self, ins: &mut [u8], outs: &mut [u8]) -> io::Result<usize> {
        if ins.len() < LZMA_HEADER_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "lzma: input too short"
            ));
        }

        let uncompressed_size = (ins[LZMA_PROPS_SIZE] as usize)
            | (ins[LZMA_PROPS_SIZE + 1] as usize) <<  8
            | (ins[LZMA_PROPS_SIZE + 2] as usize) << 16
            | (ins[LZMA_PROPS_SIZE + 3] as usize) << 24;

        for v in &mut ins[LZMA_PROPS_SIZE..LZMA_HEADER_SIZE] {
            *v = 0xff;
        }

        self.0.next_in = &ins[0];
        self.0.avail_in = ins.len();
        self.0.next_out = &mut outs[0];
        self.0.avail_out = outs.len();

        let mut ret = unsafe {
            lzma_sys::lzma_alone_decoder(&mut self.0, MEMLIMIT)
        };
        if ret == lzma_sys::LZMA_OK {
            ret = unsafe {
                lzma_sys::lzma_code(&mut self.0, lzma_sys::LZMA_FINISH)
            };
        }
        match ret {
            lzma_sys::LZMA_STREAM_END =>
                if self.0.total_out == uncompressed_size as u64 {
                    Ok(uncompressed_size)
                } else {
                    Err(io::Error::new(io::ErrorKind::InvalidInput, "lzma: format error"))
                },
            lzma_sys::LZMA_FORMAT_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzma: format error")),
            lzma_sys::LZMA_OPTIONS_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzma: options error")),
            lzma_sys::LZMA_DATA_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzma: data error")),
            lzma_sys::LZMA_MEM_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzma: memory error")),
            lzma_sys::LZMA_MEMLIMIT_ERROR =>
                // memory usage limit was reached
                // the minimum required memlimit value was stored to memlimit
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzma: memory limit error")),
            lzma_sys::LZMA_BUF_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzma: out buffer too small")),
            lzma_sys::LZMA_PROG_ERROR =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzma: program error")),
            _ =>
                Err(io::Error::new(io::ErrorKind::InvalidInput, "lzma: unknown error")),
        }
    }
}
