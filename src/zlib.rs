use std::{io, mem, fmt};
use byteorder::ByteOrder;
use byteorder::LittleEndian as LE;
use libz_sys;

use super::Decompress;

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
            strategy: 0,
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
                let window_size =       LE::read_i16(&data[4..]);
                let strategy =          LE::read_i16(&data[6..]);
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
}

struct ZLibDec(libz_sys::z_stream);

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
