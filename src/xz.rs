use std;
use libc;
use lzma_sys;

const MEMLIMIT: u64 = 32 * 1024 * 1024;

pub fn decompress(ins: &[u8], outs: &mut [u8]) -> std::io::Result<usize> {
    let mut memlimit = MEMLIMIT;
    let mut src_pos: libc::size_t = 0;
    let mut out_pos: libc::size_t = 0;
    let src_len = ins.len()  as libc::size_t;
    let out_len = outs.len() as libc::size_t;

    let ret = unsafe {
        lzma_sys::lzma_stream_buffer_decode(&mut memlimit, 0, std::ptr::null(),
            ins.as_ptr(),      &mut src_pos, src_len,
            outs.as_mut_ptr(), &mut out_pos, out_len)
    };
    match ret {
        lzma_sys::LZMA_OK
            => Ok(out_pos as usize),
        lzma_sys::LZMA_FORMAT_ERROR
            => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "xz format error")),
        lzma_sys::LZMA_OPTIONS_ERROR
            => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "xz options error")),
        lzma_sys::LZMA_DATA_ERROR
            => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "xz data error")),
        lzma_sys::LZMA_MEM_ERROR
            => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "xz memory error")),
        lzma_sys::LZMA_MEMLIMIT_ERROR
            // memory usage limit was reached
            // the minimum required memlimit value was stored to memlimit
            => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "xz memory limit error")),
        lzma_sys::LZMA_BUF_ERROR
            => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "xz out buffer too small")),
        lzma_sys::LZMA_PROG_ERROR
            => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "xz program error")),
        _ => Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "xz unknown error")),
    }
}
