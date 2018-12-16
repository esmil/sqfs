use std::{io, mem, ptr, fmt};
use byteorder::ByteOrder;
use byteorder::LittleEndian as LE;
use lzma_sys;
use libc::c_void;

use super::Decompress;
use super::Compress;

const MEMLIMIT: u64 = 32 * 1024 * 1024;

struct Filter {
    value: u64,
    mask: i32,
}

static FILTERS: [Filter; 6] = [
    Filter { value: lzma_sys::LZMA_FILTER_X86,       mask: 0x01 },
    Filter { value: lzma_sys::LZMA_FILTER_POWERPC,   mask: 0x02 },
    Filter { value: lzma_sys::LZMA_FILTER_IA64,      mask: 0x04 },
    Filter { value: lzma_sys::LZMA_FILTER_ARM,       mask: 0x08 },
    Filter { value: lzma_sys::LZMA_FILTER_ARMTHUMB,  mask: 0x10 },
    Filter { value: lzma_sys::LZMA_FILTER_SPARC,     mask: 0x20 },
];

#[derive(PartialEq)]
pub struct Options {
    dictionary_size: i32,
    flags: i32,
}

impl fmt::Display for Options {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "xz dictionary={} flags=0x{:x}",
               self.dictionary_size,
               self.flags)
    }
}

impl Options {
    pub fn new(blocksize: usize) -> Self {
        Self {
            dictionary_size: blocksize as i32,
            flags: 0x0,
        }
    }

    pub fn read(data: &[u8], blocksize: usize) -> io::Result<Options> {
        match data.len() {
            0 => Ok(Options::new(blocksize)),
            8 => {
                let dictionary_size = LE::read_i32(&data[0..]);
                let flags =           LE::read_i32(&data[4..]);
                Ok(Options { dictionary_size, flags })
            }
            _ => super::comp_err(),
        }
    }

    pub fn decoder(&self) -> io::Result<Box<Decompress>> {
        Ok(Box::new(XZDec(unsafe { mem::zeroed() })))
    }

    pub fn encoder(&self, blocksize: usize) -> io::Result<Box<Compress>> {
        let mut a = Vec::with_capacity(blocksize);
        unsafe { a.set_len(blocksize) };
        let mut b = Vec::with_capacity(blocksize);
        unsafe { b.set_len(blocksize) };

        let mut n = 1;
        for filter in &FILTERS {
            if (self.flags & filter.mask) != 0 {
                n += 1;
            }
        }

        let null = ptr::null::<c_void>() as *mut c_void;
        let mut filters: Vec<[lzma_sys::lzma_filter; 3]> = Vec::with_capacity(n);
        filters.push([
            lzma_sys::lzma_filter { id: lzma_sys::LZMA_FILTER_LZMA2, options: null },
            lzma_sys::lzma_filter { id: lzma_sys::LZMA_VLI_UNKNOWN,  options: null },
            unsafe { mem::zeroed() },
        ]);
        for filter in &FILTERS {
            if (self.flags & filter.mask) != 0 {
                filters.push([
                    lzma_sys::lzma_filter { id: filter.value,                options: null },
                    lzma_sys::lzma_filter { id: lzma_sys::LZMA_FILTER_LZMA2, options: null },
                    lzma_sys::lzma_filter { id: lzma_sys::LZMA_VLI_UNKNOWN,  options: null },
                ]);
            }
        }

        let mut enc = Box::new(XZEnc {
            strm: unsafe { mem::zeroed() },
            opts: unsafe { mem::zeroed() },
            filters: filters.into(),
            buf: [a.into(), b.into()],
            dictionary_size: self.dictionary_size,
        });

        let opts = &mut enc.opts as *mut lzma_sys::lzma_options_lzma as *mut c_void;
        enc.filters[0][0].options = opts;
        for filter in &mut enc.filters[1..] {
            filter[1].options = opts;
        }

        Ok(enc)
    }
}

struct XZDec(lzma_sys::lzma_stream);

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

struct XZEnc {
    strm: lzma_sys::lzma_stream,
    opts: lzma_sys::lzma_options_lzma,
    filters: Box<[[lzma_sys::lzma_filter; 3]]>,
    buf: [Box<[u8]>; 2],
    dictionary_size: i32,
}

impl Drop for XZEnc {
    fn drop(&mut self) {
        unsafe { lzma_sys::lzma_end(&mut self.strm) };
    }
}

impl Compress for XZEnc {
    fn compress(&mut self, ins: &mut [u8]) -> io::Result<&[u8]> {
        debug_assert!(ins.len() <= self.buf[0].len());
        debug_assert!(ins.len() <= self.buf[1].len());
        let mut best = ins.len();
        let mut tick = 0;
        let mut ret = lzma_sys::LZMA_BUF_ERROR;

        for filter in self.filters.iter() {
            let pret = unsafe {
                lzma_sys::lzma_lzma_preset(&mut self.opts, lzma_sys::LZMA_PRESET_DEFAULT)
            };
            if pret != 0 {
                ret = lzma_sys::LZMA_OPTIONS_ERROR;
                break;
            }
            self.opts.dict_size = self.dictionary_size as u32;

            self.strm.next_in = &ins[0];
            self.strm.avail_in = ins.len();
            self.strm.next_out = &mut self.buf[tick][0];
            self.strm.avail_out = self.buf[tick].len();
            ret = unsafe {
                lzma_sys::lzma_stream_encoder(&mut self.strm,
                                              &filter[0],
                                              lzma_sys::LZMA_CHECK_CRC32)
            };
            if ret != lzma_sys::LZMA_OK {
                break;
            }
            ret = unsafe {
                lzma_sys::lzma_code(&mut self.strm, lzma_sys::LZMA_FINISH)
            };
            if ret != lzma_sys::LZMA_STREAM_END {
                if ret == lzma_sys::LZMA_BUF_ERROR {
                    continue;
                }
                break;
            }

            let len = self.strm.total_out as usize;
            if len < best {
                best = len;
                tick ^= 1;
            }

            ret = lzma_sys::LZMA_BUF_ERROR;
        }

        match ret {
            lzma_sys::LZMA_BUF_ERROR if best < ins.len() =>
                Ok(&self.buf[tick^1][..best]),
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
