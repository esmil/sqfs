use std::{fmt, fs, io, mem};
use std::io::Read;
use std::rc::Rc;
use std::collections::HashMap;
use std::sync::RwLock;
use std::ops::Deref;
use std::ops::DerefMut;

extern crate byteorder;
use byteorder::ByteOrder;
use byteorder::LittleEndian as LE;
use byteorder::ReadBytesExt;

extern crate libc;
extern crate lzma_sys;
mod xz;

pub trait ReadAt {
    fn read_at(&self, buf: &mut [u8], offset: u64) -> io::Result<usize>;
}

impl ReadAt for fs::File {
    fn read_at(&self, buf: &mut [u8], offset: u64) -> io::Result<usize> {
        std::os::unix::fs::FileExt::read_at(self, buf, offset)
    }
}

fn tostr(buf: &[u8]) -> &str {
    std::str::from_utf8(buf).unwrap_or("<non-utf8>")
}

const SQUASHFS_SUPERBLOCK_SIZE: usize = 96;
#[allow(unknown_lints,decimal_literal_representation)]
const SQUASHFS_METADATA_SIZE:   usize = 8192;
const SQUASHFS_MAGIC:             u32 = 0x7371_7368;
const SQUASHFS_INVALID_FRAG:      u32 = 0xffff_ffff;

// max length of direntry name
const SQUASHFS_NAME_LEN:        usize = 256;

// max block size
//const SQUASHFS_FILE_MAX_SIZE:     u32 = 1_048_576;
const SQUASHFS_FILE_MAX_LOG:      u16 = 20;

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
const SQUASHFS_NOI:         u16 = 0x001; //  0
const SQUASHFS_NOD:         u16 = 0x002; //  1
const SQUASHFS_CHECK:       u16 = 0x004; //  2
const SQUASHFS_NOF:         u16 = 0x008; //  3
const SQUASHFS_NO_FRAG:     u16 = 0x010; //  4
const SQUASHFS_ALWAYS_FRAG: u16 = 0x020; //  5
const SQUASHFS_DUPLICATE:   u16 = 0x040; //  6
const SQUASHFS_EXPORT:      u16 = 0x080; //  7
const SQUASHFS_NOX:         u16 = 0x100; //  8
const SQUASHFS_NO_XATTR:    u16 = 0x200; //  9
const SQUASHFS_COMP_OPT:    u16 = 0x400; // 10

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

pub enum Compression {
    ZLIB,
    LZMA,
    LZO,
    XZ,
    LZ4,
}

impl Compression {
    fn new(v: u16) -> io::Result<Compression> {
        match v {
            ZLIB_COMPRESSION => Ok(Compression::ZLIB),
            LZMA_COMPRESSION => Ok(Compression::LZMA),
            LZO_COMPRESSION  => Ok(Compression::LZO),
            XZ_COMPRESSION   => Ok(Compression::XZ),
            LZ4_COMPRESSION  => Ok(Compression::LZ4),
            _ => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "unknown compression type",
            )),
        }
    }
}

impl fmt::Display for Compression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Compression::ZLIB => write!(f, "ZLIB"),
            Compression::LZMA => write!(f, "LZMA"),
            Compression::LZO  => write!(f, "LZO"),
            Compression::XZ   => write!(f, "XZ"),
            Compression::LZ4  => write!(f, "LZ4"),
        }
    }
}

pub struct SuperBlock {
    pub inodes:                u32,
    pub mkfs_time:             i32,
    pub block_size:            u32,
    pub fragments:             u32,
    pub compression:           Compression,
    pub block_log:             u16,
    pub flags:                 u16,
    pub no_ids:                u16,
    pub vers_major:            u16,
    pub vers_minor:            u16,
    pub root_inode:            i64,
    pub bytes_used:            i64,
    pub id_table_start:        i64,
    pub xattr_id_table_start:  i64,
    pub inode_table_start:     i64,
    pub directory_table_start: i64,
    pub fragment_table_start:  i64,
    pub lookup_table_start:    i64,
}

impl SuperBlock {
    fn read(h: &ReadAt, offset: u64) -> io::Result<SuperBlock> {
        let mut buf: [u8; SQUASHFS_SUPERBLOCK_SIZE] = unsafe { mem::uninitialized() };
        if h.read_at(&mut buf, offset)? != SQUASHFS_SUPERBLOCK_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "input shorter than superblock",
            ));
        }

        let magic = LE::read_u32(&buf);
        if magic != SQUASHFS_MAGIC {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "not a squashfs",
            ));
        }
        let inodes                = LE::read_u32(&buf[ 4..]);
        let mkfs_time             = LE::read_i32(&buf[ 8..]);
        let block_size            = LE::read_u32(&buf[12..]);
        let fragments             = LE::read_u32(&buf[16..]);
        let compression           = Compression::new(LE::read_u16(&buf[20..]))?;
        let block_log             = LE::read_u16(&buf[22..]);
        if block_log > SQUASHFS_FILE_MAX_LOG || (1u32 << block_log) != block_size {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid block size",
            ));
        }
        let flags                 = LE::read_u16(&buf[24..]);
        let no_ids                = LE::read_u16(&buf[26..]);
        let vers_major            = LE::read_u16(&buf[28..]);
        let vers_minor            = LE::read_u16(&buf[30..]);
        let root_inode            = LE::read_i64(&buf[32..]);
        let bytes_used            = LE::read_i64(&buf[40..]);
        let id_table_start        = LE::read_i64(&buf[48..]);
        let xattr_id_table_start  = LE::read_i64(&buf[56..]);
        let inode_table_start     = LE::read_i64(&buf[64..]);
        let directory_table_start = LE::read_i64(&buf[72..]);
        let fragment_table_start  = LE::read_i64(&buf[80..]);
        let lookup_table_start    = LE::read_i64(&buf[88..]);

        Ok(SuperBlock {
            inodes,
            mkfs_time,
            block_size,
            fragments,
            compression,
            block_log,
            flags,
            no_ids,
            vers_major,
            vers_minor,
            root_inode,
            bytes_used,
            id_table_start,
            xattr_id_table_start,
            inode_table_start,
            directory_table_start,
            fragment_table_start,
            lookup_table_start,
        })
    }

    pub fn noi(&self)         -> bool { (self.flags & SQUASHFS_NOI) != 0 }
    pub fn nod(&self)         -> bool { (self.flags & SQUASHFS_NOD) != 0 }
    pub fn check(&self)       -> bool { (self.flags & SQUASHFS_CHECK) != 0 }
    pub fn nof(&self)         -> bool { (self.flags & SQUASHFS_NOF) != 0 }
    pub fn no_frag(&self)     -> bool { (self.flags & SQUASHFS_NO_FRAG) != 0 }
    pub fn always_frag(&self) -> bool { (self.flags & SQUASHFS_ALWAYS_FRAG) != 0 }
    pub fn duplicate(&self)   -> bool { (self.flags & SQUASHFS_DUPLICATE) != 0 }
    pub fn export(&self)      -> bool { (self.flags & SQUASHFS_EXPORT) != 0 }
    pub fn nox(&self)         -> bool { (self.flags & SQUASHFS_NOX) != 0 }
    pub fn no_xattr(&self)    -> bool { (self.flags & SQUASHFS_NO_XATTR) != 0 }
    pub fn comp_opt(&self)    -> bool { (self.flags & SQUASHFS_COMP_OPT) != 0 }
}

#[derive(Debug)]
pub enum INodeType {
    Dir {
        start_block : u32,
        nlink       : u32,
        file_size   : u16,
        offset      : u16,
        parent      : u32,
    },
    File {
        start_block: u64,
        fragment: u32,
        offset: u32,
        file_size: u64,
        block_list: Box<[u32]>,
    },
    Symlink  { nlink: u32, tgt: Box<[u8]>, },
    BlockDev { nlink: u32, rdev: u32, },
    CharDev  { nlink: u32, rdev: u32, },
    Fifo     { nlink: u32, },
    Socket   { nlink: u32, },
}

#[derive(Debug)]
pub struct INode {
    tmark: u16,
    mode:  u16,
    uid:   u16,
    gid:   u16,
    mtime: i32,
    ino:   u32,
    pub typ: INodeType,
}

impl fmt::Display for INode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.typ {
            INodeType::Dir { .. } => {
                write!(f, "Dir({}, {:o})", self.ino, self.mode)
            }
            INodeType::File { .. } => {
                write!(f, "File({}, {:o})", self.ino, self.mode)
            }
            INodeType::Symlink { ref tgt, .. } => {
                write!(f, "Symlink({}, {:o}, '{}')", self.ino, self.mode, tostr(tgt))
            }
            INodeType::BlockDev { .. } => write!(f, "BlockDev({})", self.ino),
            INodeType::CharDev { .. }  => write!(f, "CharDev({})", self.ino),
            INodeType::Fifo { .. }     => write!(f, "Fifo({})", self.ino),
            INodeType::Socket { .. }   => write!(f, "Socket({})", self.ino),
        }
    }
}

impl INode {
    pub fn typechar(&self) -> char {
        match self.typ {
            INodeType::Dir { .. }      => 'd',
            INodeType::File { .. }     => 'f',
            INodeType::Symlink { .. }  => 'l',
            INodeType::BlockDev { .. } => 'b',
            INodeType::CharDev { .. }  => 'c',
            INodeType::Fifo { .. }     => 'p',
            INodeType::Socket { .. }   => 's',
        }
    }

    pub fn mode(&self) -> u16 {
        self.mode
    }
    pub fn uid_idx(&self) -> usize {
        self.uid as usize
    }
    pub fn gid_idx(&self) -> usize {
        self.gid as usize
    }
}


pub struct DirEntry {
    name: Box<[u8]>,
    ino: u32,
    block: u32,
    tmark: u16,
    offset: u16,
}

impl DirEntry {
    pub fn name(&self) -> &[u8] {
        &self.name
    }
}

pub struct DirEntryStream<'a> {
    ms: MetaStream<'a>,
    bytes: i32,
    count: u32,
    start_block: u32,
    ino_base: u32,
}

impl<'a> DirEntryStream<'a> {
    pub fn snext(&mut self) -> io::Result<Option<DirEntry>> {
        fn broken<T>() -> io::Result<T> {
            Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "broken directory entry",
            ))
        }
        if self.bytes <= 0 {
            if self.bytes < 0 {
                return broken();
            }
            return Ok(None);
        }
        if self.count == 0 {
            self.count       = self.ms.u32()? + 1;
            self.start_block = self.ms.u32()?;
            self.ino_base    = self.ms.u32()?;
            self.bytes -= 12;
        }
        let offset   = self.ms.u16()?;
        let ino_add  = self.ms.i16()?;
        let tmark    = self.ms.u16()?;
        let size     = self.ms.u16()?;

        let len = size as usize + 1;
        if len > SQUASHFS_NAME_LEN {
            return broken();
        }
        let ino = i64::from(self.ino_base) + i64::from(ino_add);
        if ino < 0 || ino > i64::from(std::u32::MAX) {
            return broken();
        }

        let name = self.ms.boxed_bytes(len)?;
        self.bytes -= 8 + i32::from(size) + 1;
        self.count -= 1;
        Ok(Some(DirEntry {
            name,
            ino: ino as u32,
            block: self.start_block,
            tmark,
            offset,
        }))
    }
}

#[derive(Clone)]
struct MetaEntry {
    next: u64,
    data: Rc<[u8]>,
}

struct FragCache {
    table: Box<[u64]>,
    map: HashMap<u32, Rc<[u8]>>,
}

struct MetaStream<'a> {
    sqfs: &'a SquashFS,
    next: u64,
    pos: usize,
    data: Rc<[u8]>,
}

impl<'a> io::Read for MetaStream<'a> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut idx = 0;
        loop {
            let mut pos = self.pos;
            while pos < self.data.len() && idx < buf.len() {
                buf[idx] = self.data[pos];
                idx += 1;
                pos += 1;
            }
            if idx == buf.len() || self.next == 0 {
                self.pos = pos;
                return Ok(idx);
            }

            let mb = self.sqfs.metablock(self.next)?;
            self.next = mb.next;
            self.data = mb.data;
            self.pos = 0;
        }
    }
}

impl<'a> MetaStream<'a> {
    fn u16(&mut self) -> io::Result<u16> {
        let idx = self.pos;
        let end = idx + 2;
        if end > self.data.len() {
            return self.read_u16::<LE>();
        }
        self.pos = end;
        Ok(LE::read_u16(&self.data[idx..]))
    }

    fn i16(&mut self) -> io::Result<i16> {
        let idx = self.pos;
        let end = idx + 2;
        if end > self.data.len() {
            return self.read_i16::<LE>();
        }
        self.pos = end;
        Ok(LE::read_i16(&self.data[idx..]))
    }

    fn u32(&mut self) -> io::Result<u32> {
        let idx = self.pos;
        let end = idx + 4;
        if end >= self.data.len() {
            return self.read_u32::<LE>();
        }
        self.pos = end;
        Ok(LE::read_u32(&self.data[idx..]))
    }

    fn i32(&mut self) -> io::Result<i32> {
        let idx = self.pos;
        let end = idx + 4;
        if end > self.data.len() {
            return self.read_i32::<LE>();
        }
        self.pos = end;
        Ok(LE::read_i32(&self.data[idx..]))
    }

    fn boxed_bytes(&mut self, len: usize) -> io::Result<Box<[u8]>> {
        let mut v = Vec::with_capacity(len);
        unsafe { v.set_len(len) };
        self.read_exact(&mut v)?;
        Ok(v.into_boxed_slice())
    }
}

pub struct SquashFS {
    handle: Box<ReadAt>,
    offset: u64,
    pub sb: SuperBlock,
    metacache: RwLock<HashMap<u64, MetaEntry>>,
    fragcache: RwLock<Option<FragCache>>,
}

impl SquashFS {
    fn metablock_read(&self, addr: u64) -> io::Result<MetaEntry> {
        fn short_read<T>() -> io::Result<T> {
            Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "short read"
            ))
        }

        let mut buf: [u8; SQUASHFS_METADATA_SIZE] = unsafe { mem::uninitialized() };
        let mut offset = self.offset + addr;
        if self.handle.read_at(&mut buf[..2], offset)? != 2 {
            return short_read();
        }
        offset += 2;

        let (blocklen, compressed) = {
            let v = LE::read_u16(&buf);

            // block is uncompressed if highest bit is set
            if v & 0x8000 == 0 {
                (v as usize, true)
            } else {
                ((v & 0x7fff) as usize, false)
            }
        };
        if blocklen > SQUASHFS_METADATA_SIZE {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "metablock too big"
            ));
        }
        let data: Rc<[u8]> = if !compressed {
            let mut v = Vec::with_capacity(blocklen);
            unsafe { v.set_len(blocklen) };
            if self.handle.read_at(&mut v, offset)? != blocklen {
                return short_read();
            }
            v
        } else {
            if self.handle.read_at(&mut buf[..blocklen], offset)? != blocklen {
                return short_read();
            }
            let mut v = Vec::with_capacity(SQUASHFS_METADATA_SIZE);
            unsafe { v.set_len(SQUASHFS_METADATA_SIZE) };
            let len = xz::decompress(&buf[..blocklen], &mut v)?;
            unsafe { v.set_len(len) };
            v.shrink_to_fit();
            v
        }.into();

        eprintln!("reading metablock {}, {} -> {} bytes, compressed = {}", addr, blocklen, data.len(), compressed);
        let next = if data.len() < SQUASHFS_METADATA_SIZE {
            0
        } else {
            addr + 2 + blocklen as u64
        };

        Ok(MetaEntry { next, data })
    }

    fn metablock(&self, addr: u64) -> io::Result<MetaEntry> {
        {
            let cache = self.metacache.read().unwrap();
            if let Some(mb) = cache.get(&addr) {
                return Ok(mb.clone());
            }
        }
        let mb = self.metablock_read(addr)?;
        self.metacache.write().unwrap().insert(addr, mb.clone());
        Ok(mb)
    }

    fn metastream(&self, addr: u64, offset: u16) -> io::Result<MetaStream> {
        let mb = self.metablock(addr)?;
        Ok(MetaStream {
            sqfs: self,
            next: mb.next,
            pos: offset as usize,
            data: mb.data,
        })
    }

    pub fn ids(&self) -> io::Result<Box<[u32]>> {
        fn corrupt<T>() -> io::Result<T> {
            Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "corrupt id table",
                    ))
        }
        eprintln!("reading id index");
        let no_ids = self.sb.no_ids as usize;
        let indexes = (4 * no_ids + SQUASHFS_METADATA_SIZE - 1) / SQUASHFS_METADATA_SIZE;
        let ilen = 8 * indexes;

        let mut buf: [u8; SQUASHFS_METADATA_SIZE] = unsafe { mem::uninitialized() };
        let mut ids = Vec::with_capacity(no_ids);
        let mut iread = 0;
        while iread < ilen {
            let chunk = std::cmp::min(ilen - iread, buf.len());
            if self.handle.read_at(&mut buf[..chunk], self.offset + self.sb.id_table_start as u64 + iread as u64)? != chunk {
                return corrupt();
            }

            let mut pos = 0;
            while pos < chunk {
                let addr = LE::read_i64(&buf[pos..]);
                if addr < 0 {
                    return corrupt();
                }

                let mb = self.metablock_read(addr as u64)?;
                let mut i = 0;
                while i < mb.data.len() {
                    ids.push(LE::read_u32(&mb.data[i..]));
                    i += 4;
                }
                pos += 8;
            }
            iread += chunk;
        }

        Ok(ids.into_boxed_slice())
    }

    fn fragment(&self, fragment: u32) -> io::Result<Rc<[u8]>> {
        let idx = 16 * fragment as usize;
        let mut addr = {
            if let Some(ref cache) = *self.fragcache.read().unwrap().deref() {
                if let Some(data) = cache.map.get(&fragment) {
                    return Ok(data.clone());
                }
                cache.table[idx / SQUASHFS_METADATA_SIZE]
            } else {
                0
            }
        };
        if addr == 0 {
            let table = self.fragment_table()?;
            eprintln!("fragment: table = {:?}", &table);
            addr = table[idx / SQUASHFS_METADATA_SIZE];
            let mut lock = self.fragcache.write().unwrap();
            let p = lock.deref_mut();
            if p.is_none() {
                *p = Some(FragCache { table, map: HashMap::new() })
            }
        }

        let block_size = 1usize << self.sb.block_log;
        let mut raw = Vec::with_capacity(block_size);
        unsafe { raw.set_len(block_size) };
        let mut buf = Vec::with_capacity(block_size);
        unsafe { buf.set_len(block_size) };

        let mb = self.metablock(addr)?;
        let pos = idx % SQUASHFS_METADATA_SIZE;
        let start_block = LE::read_i64(&mb.data[pos..]);
        if start_block < 0 {
            return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "broken fragment table"
            ));
        }
        let (_rlen, len) = self.readblock(&mut raw, &mut buf, start_block as u64, LE::read_u32(&mb.data[(pos+8)..]))?;
        unsafe { buf.set_len(len) };
        buf.shrink_to_fit();

        let ret: Rc<[u8]> = buf.into();
        {
            if let Some(ref mut cache) = *self.fragcache.write().unwrap().deref_mut() {
                cache.map.insert(fragment, ret.clone());
            }
        }
        Ok(ret)
    }

    pub fn new(handle: Box<ReadAt>, offset: u64) -> io::Result<SquashFS> {
        let sb = SuperBlock::read(&*handle, offset)?;
        Ok(SquashFS {
            handle,
            offset,
            sb,
            metacache: RwLock::new(HashMap::new()),
            fragcache: RwLock::new(None),
        })
    }

    fn inode_at(&self, addr: u64, offset: u16) -> io::Result<INode> {
        let mut ms = self.metastream(addr, offset)?;
        let tmark = ms.u16()?;
        let mode  = ms.u16()?;
        let uid   = ms.u16()?;
        let gid   = ms.u16()?;
        if uid >= self.sb.no_ids || gid >= self.sb.no_ids {
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "invalid id"));
        }
        let mtime = ms.i32()?;
        let ino   = ms.u32()?;
        let typ   = match tmark {
            SQUASHFS_DIR_TYPE => {
                let start_block = ms.u32()?;
                let nlink       = ms.u32()?;
                let file_size   = ms.u16()?;
                let offset      = ms.u16()?;
                let parent      = ms.u32()?;
                INodeType::Dir {
                    start_block,
                    nlink,
                    file_size,
                    offset,
                    parent,
                }
            }
            SQUASHFS_FILE_TYPE => {
                let start_block = ms.u32()?;
                let fragment    = ms.u32()?;
                let offset      = ms.u32()?;
                let file_size   = ms.u32()?;

                let blocks = if fragment == SQUASHFS_INVALID_FRAG {
                    (file_size + self.sb.block_size - 1) >> self.sb.block_log
                } else {
                    file_size >> self.sb.block_log
                } as usize;

                let mut block_list = Vec::with_capacity(blocks);
                for _ in 0..blocks {
                    block_list.push(ms.u32()?);
                }

                INodeType::File {
                    start_block: u64::from(start_block),
                    fragment,
                    offset,
                    file_size: u64::from(file_size),
                    block_list: block_list.into_boxed_slice(),
                }
            }
            SQUASHFS_SYMLINK_TYPE => {
                let nlink = ms.u32()?;
                let size  = ms.u32()? as usize;
                let tgt   = ms.boxed_bytes(size)?;
                INodeType::Symlink {
                    nlink,
                    tgt,
                }
            }
            SQUASHFS_BLKDEV_TYPE => {
                let nlink = ms.u32()?;
                let rdev  = ms.u32()?;
                INodeType::BlockDev { nlink, rdev }
            }
            SQUASHFS_CHRDEV_TYPE => {
                let nlink = ms.u32()?;
                let rdev  = ms.u32()?;
                INodeType::CharDev { nlink, rdev }
            }
            SQUASHFS_FIFO_TYPE => {
                let nlink = ms.u32()?;
                INodeType::Fifo { nlink }
            }
            SQUASHFS_SOCKET_TYPE => {
                let nlink = ms.u32()?;
                INodeType::Socket { nlink }
            }
            SQUASHFS_LDIR_TYPE     => panic!("unimplemented inode type large dir"),
            SQUASHFS_LREG_TYPE     => panic!("unimplemented inode type large file"),
            SQUASHFS_LSYMLINK_TYPE => panic!("unimplemented inode type large symlink"),
            SQUASHFS_LBLKDEV_TYPE  => panic!("unimplemented inode type large blkdev"),
            SQUASHFS_LCHRDEV_TYPE  => panic!("unimplemented inode type large chrdev"),
            SQUASHFS_LFIFO_TYPE    => panic!("unimplemented inode type large fifo"),
            SQUASHFS_LSOCKET_TYPE  => panic!("unimplemented inode type large socket"),
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "invalid inode type",
                ));
            }
        };
        Ok(INode {
            tmark,
            mode,
            uid,
            gid,
            mtime,
            ino,
            typ,
        })
    }

    pub fn direntries(&self, node: &INode) -> io::Result<DirEntryStream> {
        if let INodeType::Dir { start_block, file_size, offset, ..} = node.typ {
            let addr = self.sb.directory_table_start as u64 + u64::from(start_block);
            return Ok(DirEntryStream {
                ms: self.metastream(addr, offset)?,
                bytes: i32::from(file_size) - 3,
                count: 0,
                start_block: 0,
                ino_base: 0,
            });
        }
        Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "not a directory",
        ))
    }

    pub fn child(&self, node: &INode, name: &[u8]) -> io::Result<INode> {
        fn broken<T>() -> io::Result<T> {
            Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "broken directory entry",
            ))
        }

        /*
        if name.len() > SQUASHFS_NAME_LEN {
            // TODO: error
        }
        */

        let (start_block, file_size, offset) = if let INodeType::Dir { start_block, file_size, offset, ..} = node.typ {
            (start_block, file_size, offset)
        } else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "not a directory",
            ));
        };

        let addr = self.sb.directory_table_start as u64 + u64::from(start_block);
        let mut ms = self.metastream(addr, offset)?;
        let mut buf: [u8; SQUASHFS_NAME_LEN] = unsafe { mem::uninitialized() };
        let mut bytes = i32::from(file_size) - 3;
        while bytes > 0 {
            let mut count   = ms.u32()?;
            let start_block = ms.u32()?;
            let ino_base    = ms.u32()?;
            bytes -= 12;

            loop {
                let offset   = ms.u16()?;
                let ino_add  = ms.i16()?;
                let tmark    = ms.u16()?;
                let size     = ms.u16()?;

                let len = size as usize + 1;
                if len > SQUASHFS_NAME_LEN {
                    return broken();
                }
                let ino = i64::from(ino_base) + i64::from(ino_add);
                if ino < 0 || ino > i64::from(std::u32::MAX) {
                    return broken();
                }
                ms.read_exact(&mut buf[..len])?;
                if name == &buf[..len] {
                    let addr = self.sb.inode_table_start as u64 + u64::from(start_block);
                    let node = self.inode_at(addr, offset)?;
                    if tmark != node.tmark || ino as u32 != node.ino {
                        return broken();
                    }
                    return Ok(node);
                }
                bytes -= 8 + i32::from(size) + 1;
                if count == 0 {
                    break;
                }
                count -= 1;
            }
        }
        if bytes < 0 {
            return broken();
        }
        not_found()
    }

    pub fn node(&self, de: &DirEntry) -> io::Result<INode> {
        let addr = self.sb.inode_table_start as u64 + u64::from(de.block);
        let node = self.inode_at(addr, de.offset)?;
        if de.tmark != node.tmark || de.ino != node.ino {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "broken directory entry",
            ));
        }
        Ok(node)
    }

    fn root(&self) -> io::Result<INode> {
        if self.sb.inode_table_start < 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "no inode table",
            ));
        }
        if self.sb.root_inode < 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "no root inode",
            ));
        }
        let root = self.sb.root_inode as u64;
        let addr = self.sb.inode_table_start as u64 + (root >> 16);
        self.inode_at(addr, root as u16)
    }

    pub fn lookup(&self, path: &[u8]) -> io::Result<INode> {
        let mut stack = Vec::new();
        stack.push(self.root()?);
        let mut end = 0;
        while end < path.len() {
            //eprintln!("end = {}, node = {}", end, stack.last().unwrap());
            if let INodeType::Dir { .. } = stack.last().unwrap().typ {
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "not a directory",
                ));
            };
            let mut begin = end;
            while begin < path.len() && path[begin] == b'/' {
                begin += 1;
            }
            end = begin;
            while end < path.len() && path[end] != b'/' {
                end += 1;
            }
            let part = &path[begin..end];
            if part.is_empty() || part == b"." {
                continue;
            }
            if part == b".." {
                stack.pop();
                if stack.is_empty() {
                    return not_found();
                }
                continue;
            }

            let child = self.child(stack.last().unwrap(), part)?;
            stack.push(child);
        }
        Ok(stack.pop().unwrap())
    }

    fn fragment_table(&self) -> io::Result<Box<[u64]>> {
        fn corrupt<T>() -> io::Result<T> {
            Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "corrupt fragment table",
                    ))
        }
        let fragments = self.sb.fragments;
        if fragments == 0 {
            return Ok(Box::new([]));
        }

        let indexes = (16 * fragments as usize + SQUASHFS_METADATA_SIZE - 1) / SQUASHFS_METADATA_SIZE;
        let ilen = 8 * indexes;

        let mut buf: [u8; SQUASHFS_METADATA_SIZE] = unsafe { mem::uninitialized() };
        let mut table = Vec::with_capacity(indexes);
        let mut iread = 0;
        while iread < ilen {
            let chunk = std::cmp::min(ilen - iread, buf.len());
            if self.handle.read_at(&mut buf[..chunk], self.offset + self.sb.fragment_table_start as u64 + iread as u64)? != chunk {
                return corrupt();
            }

            let mut pos = 0;
            while pos < chunk {
                let addr = LE::read_i64(&buf[pos..]);
                if addr < 0 {
                    return corrupt();
                }

                table.push(addr as u64);
                pos += 8;
            }
            iread += chunk;
        }
        Ok(table.into())
    }

    fn readblock(&self, raw: &mut [u8], out: &mut [u8], addr: u64, v: u32) -> io::Result<(usize, usize)> {
        let rlen: usize;
        let len: usize;
        if v == 0 { // sparse block
            rlen = 0;
            len = out.len();

            for e in out.iter_mut() {
                *e = 0;
            }
            eprintln!("write: read block {}, {} bytes, sparse", addr, rlen);
        } else if v & 0x100_0000 != 0 { // uncompressed
            rlen = (v & 0xff_ffff) as usize;
            len = rlen;

            if self.handle.read_at(&mut out[..rlen], self.offset + addr)? != rlen {
                return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "short read"
                        ));
            }
            eprintln!("write: read block {}, {} bytes, uncompressed", addr, rlen);
        } else {
            rlen = v as usize;

            if self.handle.read_at(&mut raw[..rlen], self.offset + addr)? != rlen {
                return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "short read"
                        ));
            }

            len = xz::decompress(&raw[..rlen], out)?;
            eprintln!("write: read block {}, {} bytes, compressed {} bytes", addr, rlen, len);
        }
        Ok((rlen, len))
    }

    pub fn write(&self, node: &INode, out: &mut io::Write) -> io::Result<()> {
        if let INodeType::File { mut start_block, fragment, offset, mut file_size, ref block_list } = node.typ {
            eprintln!("write: start_block = {}, fragment = {}, offset = {}, file_size = {}, #blocks = {}",
                     start_block, fragment, offset, file_size, block_list.len());
            eprintln!("write: block_list = {:?}", block_list);
            let block_size = 1usize << self.sb.block_log;
            let mut raw = Vec::with_capacity(block_size);
            unsafe { raw.set_len(block_size) };
            let mut buf = Vec::with_capacity(block_size);
            unsafe { buf.set_len(block_size) };
            for v in block_list.iter() {
                let len = if file_size < block_size as u64 {
                    file_size as usize
                } else {
                    block_size
                };
                let (rlen, len) = self.readblock(&mut raw, &mut buf[..len], start_block, *v)?;
                start_block += rlen as u64;
                file_size -= len as u64;
                out.write_all(&buf[..len])?;
            }
            if fragment != SQUASHFS_INVALID_FRAG && file_size < block_size as u64 {
                let data = self.fragment(fragment)?;
                out.write_all(&data[(offset as usize)..(offset as usize + file_size as usize)])?;
            } else if file_size > 0 {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "broken file entry",
                ));
            }
            Ok(())
        } else {
            Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "not a directory",
            ))
        }
    }
}
