use std::{fmt, io};

extern crate libc;
extern crate byteorder;

pub trait Decompress {
    fn decompress(&mut self, ins: &mut [u8], outs: &mut [u8]) -> io::Result<usize>;
}

pub trait Compress {
    fn compress<'a>(&'a mut self, ins: &mut [u8]) -> io::Result<&'a [u8]>;
}

pub trait ReadBlock {
    fn readblock(&mut self, buf: &mut [u8]) -> io::Result<usize>;
}

pub trait FileData {
    fn open(&self) -> io::Result<Box<dyn ReadBlock>>;
}

extern crate libz_sys;
mod zlib;

extern crate lzma_sys;
mod lzma;
mod xz;

extern crate rust_lzo;
mod lzo;

extern crate lz4_sys;
mod lz4;

pub mod unsquash;
pub mod squash;

const SQUASHFS_SUPERBLOCK_SIZE: usize = 96;
const SQUASHFS_METADATA_SIZE:   usize = 8192;
const SQUASHFS_MAGIC:             u32 = 0x7371_7368;
const SQUASHFS_INVALID_FRAG:      u32 = 0xffff_ffff;
const SQUASHFS_INVALID_XATTR:     u32 = 0xffff_ffff;

// max length of direntry name
const SQUASHFS_NAME_LEN:        usize = 256;

// max block size
//const SQUASHFS_FILE_MAX_SIZE:     u32 = 1_048_576;
const SQUASHFS_FILE_MAX_LOG:      u16 = 20;

// inode types
const SQUASHFS_DIR_TYPE:      u16 =  1;
const SQUASHFS_FILE_TYPE:     u16 =  2;
const SQUASHFS_SYMLINK_TYPE:  u16 =  3;
const SQUASHFS_BLKDEV_TYPE:   u16 =  4;
const SQUASHFS_CHRDEV_TYPE:   u16 =  5;
const SQUASHFS_FIFO_TYPE:     u16 =  6;
const SQUASHFS_SOCKET_TYPE:   u16 =  7;
const SQUASHFS_LDIR_TYPE:     u16 =  8;
const SQUASHFS_LREG_TYPE:     u16 =  9;
const SQUASHFS_LSYMLINK_TYPE: u16 = 10;
const SQUASHFS_LBLKDEV_TYPE:  u16 = 11;
const SQUASHFS_LCHRDEV_TYPE:  u16 = 12;
const SQUASHFS_LFIFO_TYPE:    u16 = 13;
const SQUASHFS_LSOCKET_TYPE:  u16 = 14;

// flags
const SQUASHFS_NOI:         u16 = 0x001; // bit  0, no inode compression
const SQUASHFS_NOD:         u16 = 0x002; // bit  1, no data compression
const SQUASHFS_CHECK:       u16 = 0x004; // bit  2
const SQUASHFS_NOF:         u16 = 0x008; // bit  3, no fragment compression
const SQUASHFS_NO_FRAG:     u16 = 0x010; // bit  4, no fragments
const SQUASHFS_ALWAYS_FRAG: u16 = 0x020; // bit  5
const SQUASHFS_DUPLICATE:   u16 = 0x040; // bit  6
const SQUASHFS_EXPORT:      u16 = 0x080; // bit  7
const SQUASHFS_NOX:         u16 = 0x100; // bit  8, no xattr compression
const SQUASHFS_NO_XATTR:    u16 = 0x200; // bit  9, no xattrs
const SQUASHFS_COMP_OPT:    u16 = 0x400; // bit 10, compression options after superblock

// compression
const ZLIB_COMPRESSION: u16 = 1;
const LZMA_COMPRESSION: u16 = 2;
const LZO_COMPRESSION:  u16 = 3;
const XZ_COMPRESSION:   u16 = 4;
const LZ4_COMPRESSION:  u16 = 5;

fn not_found<T>() -> io::Result<T> {
    Err(io::Error::new(
        io::ErrorKind::NotFound,
        "path not found"
    ))
}

fn comp_err<T>() -> io::Result<T> {
    Err(io::Error::new(
        io::ErrorKind::InvalidInput,
        "error reading compression options"
    ))
}

#[derive(PartialEq)]
pub enum Compression {
    ZLIB(zlib::Options),
    LZMA(lzma::Options),
    LZO(lzo::Options),
    XZ(xz::Options),
    LZ4(lz4::Options),
}

impl Compression {
    fn from_data(v: u16, data: &[u8], blocksize: usize) -> io::Result<Compression> {
        match v {
            ZLIB_COMPRESSION => Ok(Compression::ZLIB(zlib::Options::read(data)?)),
            LZMA_COMPRESSION => Ok(Compression::LZMA(lzma::Options::read(data)?)),
            LZO_COMPRESSION  => Ok(Compression::LZO(lzo::Options::read(data)?)),
            XZ_COMPRESSION   => Ok(Compression::XZ(xz::Options::read(data, blocksize)?)),
            LZ4_COMPRESSION  => Ok(Compression::LZ4(lz4::Options::read(data)?)),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "unknown compression type",
            )),
        }
    }

    fn tag(&self) -> u16 {
        match *self {
            Compression::ZLIB(_) => ZLIB_COMPRESSION,
            Compression::LZMA(_) => LZMA_COMPRESSION,
            Compression::LZO(_)  => LZO_COMPRESSION,
            Compression::XZ(_)   => XZ_COMPRESSION,
            Compression::LZ4(_)  => LZ4_COMPRESSION,
        }
    }

    #[allow(unused)]
    fn zlib() -> Compression {
        Compression::ZLIB(Default::default())
    }

    #[allow(unused)]
    fn lzo() -> Compression {
        Compression::LZO(Default::default())
    }

    #[allow(unused)]
    fn lzma() -> Compression {
        Compression::LZMA(Default::default())
    }

    #[allow(unused)]
    fn xz(blocksize: usize) -> Compression {
        Compression::XZ(xz::Options::new(blocksize))
    }

    #[allow(unused)]
    fn lz4() -> Compression {
        Compression::LZ4(Default::default())
    }

    fn decoder(&self) -> io::Result<Box<Decompress>> {
        match *self {
            Compression::ZLIB(ref opts) => opts.decoder(),
            Compression::LZMA(ref opts) => opts.decoder(),
            Compression::LZO(ref opts)  => opts.decoder(),
            Compression::XZ(ref opts)   => opts.decoder(),
            Compression::LZ4(ref opts)  => opts.decoder(),
        }
    }

    fn encoder(&self, blocksize: usize) -> io::Result<Box<Compress>> {
        match *self {
            Compression::ZLIB(ref opts) => opts.encoder(blocksize),
            Compression::LZMA(ref opts) => opts.encoder(blocksize),
            Compression::LZO(ref opts)  => opts.encoder(blocksize),
            Compression::XZ(ref opts)   => opts.encoder(blocksize),
            Compression::LZ4(ref opts)  => opts.encoder(blocksize),
        }
    }
}

impl fmt::Display for Compression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Compression::ZLIB(ref opts) => opts.fmt(f),
            Compression::LZMA(ref opts) => opts.fmt(f),
            Compression::LZO(ref opts)  => opts.fmt(f),
            Compression::XZ(ref opts)   => opts.fmt(f),
            Compression::LZ4(ref opts)  => opts.fmt(f),
        }
    }
}
