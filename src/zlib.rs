use std::{io, mem, fmt};
use byteorder::ByteOrder;
use byteorder::LittleEndian as LE;
use libz_sys;

use super::Decompress;
use super::Compress;

struct Strategy {
    value: i32,
    mask: i16,
}

/* zlib strategies ordered by ease of decoding */
static STRATEGIES: [Strategy; 5] = [
    Strategy { value: libz_sys::Z_HUFFMAN_ONLY,     mask: 0x04 },
    Strategy { value: libz_sys::Z_RLE,              mask: 0x08 },
    Strategy { value: libz_sys::Z_FILTERED,         mask: 0x02 },
    Strategy { value: libz_sys::Z_FIXED,            mask: 0x10 },
    Strategy { value: libz_sys::Z_DEFAULT_STRATEGY, mask: 0x01 },
];

#[derive(PartialEq)]
pub struct Options {
    compression_level: i32,
    window_size: i16,
    strategy: i16,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            compression_level: 9,
            window_size: 15,
            strategy: 0x1,
        }
    }
}

impl fmt::Display for Options {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "zlib level={} window={} strategy=0x{:x}",
               self.compression_level,
               self.window_size,
               self.strategy)
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
                let compression_level = LE::read_i32(&data[0..]);
                let window_size       = LE::read_i16(&data[4..]);
                let strategy          = LE::read_i16(&data[6..]);
                Ok(Options {
                    compression_level,
                    window_size,
                    strategy,
                })
            }
            _ => super::comp_err(),
        }
    }

    pub fn decoder(&self) -> io::Result<Box<Decompress>> {
        let mut dec = Box::new(ZLibDec(unsafe { mem::zeroed() }));

        let ret = unsafe {
            libz_sys::inflateInit2_(&mut dec.0,
                                    i32::from(self.window_size),
                                    libz_sys::zlibVersion(),
                                    mem::size_of::<libz_sys::z_stream>() as i32)
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

    pub fn encoder(&self, blocksize: usize) -> io::Result<Box<Compress>> {
        let mut a = Vec::with_capacity(blocksize);
        unsafe { a.set_len(blocksize) };
        let mut b = Vec::with_capacity(blocksize);
        unsafe { b.set_len(blocksize) };

        let mut enc = Box::new(ZLibEnc {
            strm: unsafe { mem::zeroed() },
            compression_level: self.compression_level,
            strategy: self.strategy,
            buf: [a.into(), b.into()],
        });

        let ret = unsafe {
            libz_sys::deflateInit2_(&mut enc.strm,
                                    self.compression_level,
                                    libz_sys::Z_DEFLATED,
                                    i32::from(self.window_size),
                                    9, /* memlevel, 8 is default */
                                    libz_sys::Z_DEFAULT_STRATEGY,
                                    libz_sys::zlibVersion(),
                                    mem::size_of::<libz_sys::z_stream>() as i32)
        };
        match ret {
            libz_sys::Z_OK =>
                Ok(enc),
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
}

struct ZLibDec(libz_sys::z_stream);

impl Drop for ZLibDec {
    fn drop(&mut self) {
        unsafe { libz_sys::inflateEnd(&mut self.0) };
    }
}

impl Decompress for ZLibDec {
    fn decompress(&mut self, ins: &mut [u8], outs: &mut [u8]) -> io::Result<usize> {
        let mut ret = unsafe {
            libz_sys::inflateReset(&mut self.0)
        };

        self.0.next_in = &mut ins[0];
        self.0.avail_in = ins.len() as u32;
        self.0.next_out = &mut outs[0];
        self.0.avail_out = outs.len() as u32;

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

struct ZLibEnc {
    strm: libz_sys::z_stream,
    compression_level: i32,
    strategy: i16,
    buf: [Box<[u8]>; 2],
}

impl Drop for ZLibEnc {
    fn drop(&mut self) {
        unsafe { libz_sys::deflateEnd(&mut self.strm) };
    }
}

impl Compress for ZLibEnc {
    fn compress(&mut self, ins: &mut [u8]) -> io::Result<&[u8]> {
        debug_assert!(ins.len() <= self.buf[0].len());
        debug_assert!(ins.len() <= self.buf[1].len());
        let mut ret = libz_sys::Z_BUF_ERROR;
        let mut best = ins.len();
        let mut tick = 0;

        for strat in &STRATEGIES {
            if (self.strategy & strat.mask) == 0 {
                continue;
            }

            ret = unsafe {
                libz_sys::deflateReset(&mut self.strm)
            };
            if ret != libz_sys::Z_OK {
                break;
            }

            /* deflateParams calls deflate to consume
             * any input available with the old paramaters,
             * so make sure it doesn't consume anything */
            self.strm.avail_in = 0;
            self.strm.next_out = &mut self.buf[tick][0];
            self.strm.avail_out = self.buf[tick].len() as u32;

            ret = unsafe {
                libz_sys::deflateParams(&mut self.strm,
                                        self.compression_level,
                                        strat.value)
            };
            if ret != libz_sys::Z_OK {
                break;
            }

            /* now deflate input with the new parameters */
            self.strm.next_in = &mut ins[0];
            self.strm.avail_in = ins.len() as u32;

            ret = unsafe {
                libz_sys::deflate(&mut self.strm, libz_sys::Z_FINISH)
            };
            if ret != libz_sys::Z_STREAM_END {
                if ret == libz_sys::Z_BUF_ERROR {
                    continue;
                }
                break;
            }

            let len = self.strm.total_out as usize;
            if len < best {
                best = len;
                tick ^= 1;
            }
            ret = libz_sys::Z_BUF_ERROR;
        }

        match ret {
            libz_sys::Z_BUF_ERROR if best < ins.len() =>
                Ok(&self.buf[tick^1][..best]),
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
