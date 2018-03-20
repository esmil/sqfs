use std::io;
use lz4_sys;

use super::Decompress;

struct LZ4Dec(*mut lz4_sys::LZ4StreamDecode);

pub fn decompress() -> io::Result<Box<Decompress>> {
    Ok(Box::new(LZ4Dec(unsafe { lz4_sys::LZ4_createStreamDecode() })))
}

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
