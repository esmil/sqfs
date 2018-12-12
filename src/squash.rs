use std::{io, fs, mem, cmp, time};
use std::collections::BTreeMap;
use std::error::Error;
use std::ops::DerefMut;
use std::io::{Seek, Write};
use byteorder::ByteOrder;
use byteorder::LittleEndian as LE;

use super::SQUASHFS_MAGIC;
use super::SQUASHFS_SUPERBLOCK_SIZE;
use super::SQUASHFS_METADATA_SIZE;

use super::SQUASHFS_NAME_LEN;

use super::SQUASHFS_INVALID_XATTR;
use super::SQUASHFS_INVALID_FRAG;

use super::SQUASHFS_DUPLICATE;
//use super::SQUASHFS_NOI;
use super::SQUASHFS_NO_FRAG;
use super::SQUASHFS_NO_XATTR;

use super::SQUASHFS_DIR_TYPE;
use super::SQUASHFS_FILE_TYPE;
use super::SQUASHFS_SYMLINK_TYPE;
use super::SQUASHFS_BLKDEV_TYPE;
use super::SQUASHFS_CHRDEV_TYPE;
use super::SQUASHFS_FIFO_TYPE;
use super::SQUASHFS_SOCKET_TYPE;
use super::SQUASHFS_LREG_TYPE;

use super::Compression;
use super::Compress;
use super::FileData;

use virtfs;

fn to_timestamp(st: time::SystemTime) -> i32 {
    match st.duration_since(time::SystemTime::UNIX_EPOCH) {
        Ok(n)  => n.as_secs() as i32,
        Err(_) => 0,
    }
}

enum Visit {
    Down(usize),
    Up(usize),
}

enum INodeType<'a, T> {
    Dir { d: &'a virtfs::Dir },
    File {
        f: &'a virtfs::File<T>,
        start_block: u64,
        fragment: u32,
        offset: u32,
        file_size: u64,
        block_list: Vec<u32>,
    },
    Symlink { l: &'a virtfs::Symlink },
    BlockDev { b: &'a virtfs::BlockDev },
    CharDev { c: &'a virtfs::CharDev },
    Fifo { p: &'a virtfs::Fifo },
    Socket { s: &'a virtfs::Socket },
}

struct INode<'a, T> {
    start_block: u32,
    offset: u16,
    typ: INodeType<'a, T>,
}

impl<'a, T> INode<'a, T> {
    fn new(n: &'a virtfs::Node<T>) -> INode<'a, T> {
        INode {
            start_block: 0,
            offset: 0,
            typ: match *n {
                virtfs::Node::Dir(ref d)      => INodeType::Dir { d },
                virtfs::Node::File(ref f)     => INodeType::File {
                    f,
                    start_block: 0,
                    fragment: SQUASHFS_INVALID_FRAG,
                    offset: 0,
                    file_size: 0,
                    block_list: Vec::new(),
                },
                virtfs::Node::Symlink(ref l)  => INodeType::Symlink { l },
                virtfs::Node::BlockDev(ref b) => INodeType::BlockDev { b },
                virtfs::Node::CharDev(ref c)  => INodeType::CharDev { c },
                virtfs::Node::Fifo(ref p)     => INodeType::Fifo { p },
                virtfs::Node::Socket(ref s)   => INodeType::Socket { s },
            },
        }
    }

    fn setpos(&mut self, pos: (u32, u16)) {
        self.start_block = pos.0;
        self.offset = pos.1;
    }

    fn tmark(&self) -> u16 {
        match self.typ {
            INodeType::Dir { .. }      => SQUASHFS_DIR_TYPE,
            INodeType::File { .. }     => SQUASHFS_FILE_TYPE,
            INodeType::Symlink { .. }  => SQUASHFS_SYMLINK_TYPE,
            INodeType::BlockDev { .. } => SQUASHFS_BLKDEV_TYPE,
            INodeType::CharDev { .. }  => SQUASHFS_CHRDEV_TYPE,
            INodeType::Fifo { .. }     => SQUASHFS_FIFO_TYPE,
            INodeType::Socket { .. }   => SQUASHFS_SOCKET_TYPE,
        }
    }
}

trait Push {
    fn push(&mut self, data: &[u8]) -> io::Result<()>;
    fn pos(&self) -> usize;
}

struct ToFile<'a> {
    file: &'a mut fs::File,
    pos: usize,
}

impl<'a> Push for ToFile<'a> {
    fn push(&mut self, data: &[u8]) -> io::Result<()> {
        self.file.write_all(data)?;
        self.pos += data.len();
        Ok(())
    }

    fn pos(&self) -> usize {
        self.pos
    }
}

struct ToBuffer(Vec<u8>);

impl Push for ToBuffer {
    fn push(&mut self, data: &[u8]) -> io::Result<()> {
        self.0.extend_from_slice(data);
        Ok(())
    }

    fn pos(&self) -> usize {
        self.0.len()
    }
}

struct MetaStream<T : Push> {
    out: T,
    offset: usize,
    buf: [u8; SQUASHFS_METADATA_SIZE + 8],
}

impl<'a> MetaStream<ToFile<'a>> {
    fn new(file: &mut fs::File) -> MetaStream<ToFile> {
        MetaStream {
            out: ToFile { file, pos: 0 },
            offset: 0,
            buf: unsafe { mem::uninitialized() },
        }
    }
}

impl Default for MetaStream<ToBuffer> {
    fn default() -> Self {
        MetaStream {
            out: ToBuffer(Vec::new()),
            offset: 0,
            buf: unsafe { mem::uninitialized() },
        }
    }
}

impl MetaStream<ToBuffer> {
    fn new() -> Self {
        Default::default()
    }
}

impl<T : Push> MetaStream<T> {
    fn pos(&self) -> (u32, u16) {
        (self.out.pos() as u32, self.offset as u16)
    }

    fn flush(&mut self, enc: &mut Compress) -> io::Result<()> {
        let len = cmp::min(self.offset, SQUASHFS_METADATA_SIZE);
        let mut header: [u8; 2] = unsafe { mem::uninitialized() };
        match enc.compress(&mut self.buf[..len], SQUASHFS_METADATA_SIZE) {
            Ok(out) if out.len() < len => {
                LE::write_u16(&mut header, out.len() as u16);
                self.out.push(&header)?;
                self.out.push(out)?;
            }
            _ => {
                LE::write_u16(&mut header, len as u16 | 0x8000u16);
                self.out.push(&header)?;
                self.out.push(&self.buf[..len])?;
            }
        };
        let rest = self.offset - len;
        if rest > 0 {
            let (head, tail) = self.buf.split_at_mut(SQUASHFS_METADATA_SIZE);
            head[..rest].copy_from_slice(tail);
        }
        self.offset = rest;
        Ok(())
    }

    fn advance(&mut self, enc: &mut Compress, v: usize) -> io::Result<()> {
        self.offset += v;
        if self.offset < SQUASHFS_METADATA_SIZE {
            return Ok(());
        }
        self.flush(enc)
    }


    fn i16(&mut self, enc: &mut Compress, v: i16) -> io::Result<()> {
        LE::write_i16(&mut self.buf[self.offset..], v);
        self.advance(enc, 2)
    }

    fn u16(&mut self, enc: &mut Compress, v: u16) -> io::Result<()> {
        LE::write_u16(&mut self.buf[self.offset..], v);
        self.advance(enc, 2)
    }

    fn i32(&mut self, enc: &mut Compress, v: i32) -> io::Result<()> {
        LE::write_i32(&mut self.buf[self.offset..], v);
        self.advance(enc, 4)
    }

    fn u32(&mut self, enc: &mut Compress, v: u32) -> io::Result<()> {
        LE::write_u32(&mut self.buf[self.offset..], v);
        self.advance(enc, 4)
    }

    fn i64(&mut self, enc: &mut Compress, v: i64) -> io::Result<()> {
        LE::write_i64(&mut self.buf[self.offset..], v);
        self.advance(enc, 8)
    }

    fn write(&mut self, enc: &mut Compress, mut data: &[u8]) -> io::Result<()> {
        while !data.is_empty() {
            let chunk = cmp::min(data.len(), SQUASHFS_METADATA_SIZE - self.offset);
            self.buf[(self.offset)..(self.offset + chunk)].copy_from_slice(data);
            self.advance(enc, chunk)?;
            data = &data[chunk..];
        }
        Ok(())
    }
}

struct MetaData<'a, T> {
    comp: Compression,
    inodes: Vec<INode<'a, T>>,
    idx_to_ino: Box<[usize]>,
    id_table: BTreeMap<u32, u16>,
    fpos: u64,
    block_log: u16,
    flags: u16,
    fragments: u32,
    root_inode: i64,
    id_table_start: i64,
    xattr_id_table_start: i64,
    inode_table_start: i64,
    directory_table_start: i64,
    fragment_table_start: i64,
    lookup_table_start: i64,
}

impl<'a, T : FileData> MetaData<'a, T> {
    fn new(fs: &virtfs::VirtFS<T>) -> MetaData<T> {
        let mut inodes = Vec::with_capacity(fs.nodes());
        let mut idx_to_ino = vec![0; fs.max_idx()];
        let mut id_table: BTreeMap<u32, u16> = BTreeMap::new();
        let mut stack = vec![Visit::Down(0)];
        let mut flags = SQUASHFS_NO_XATTR | SQUASHFS_NO_FRAG;
        while let Some(n) = stack.pop() {
            match n {
                Visit::Down(idx) => {
                    stack.push(Visit::Up(idx));
                    if let virtfs::Node::Dir(ref d) = *fs.node(idx).unwrap() {
                        for (name, &v) in d.entries().rev() {
                            if name.len() > SQUASHFS_NAME_LEN {
                                /* TODO: return error */
                            }
                            stack.push(Visit::Down(v));
                        }
                    }
                }
                Visit::Up(idx) => {
                    if idx_to_ino[idx] == 0 {
                        let node = fs.node(idx).unwrap();
                        id_table.insert(node.uid(), 0);
                        id_table.insert(node.gid(), 0);
                        inodes.push(INode::new(node));
                        idx_to_ino[idx] = inodes.len();
                    } else {
                        flags |= SQUASHFS_DUPLICATE;
                    }
                }
            }
        }

        for (i, v) in id_table.values_mut().enumerate() {
            *v = i as u16;
        }

        println!("id_table = {:?}", &id_table);

        MetaData {
            //comp: Compression::xz(1usize << 17),
            comp: Compression::zlib(),
            inodes,
            idx_to_ino: idx_to_ino.into_boxed_slice(),
            id_table,
            fpos: SQUASHFS_SUPERBLOCK_SIZE as u64,
            block_log: 17,
            flags,
            fragments: 0,
            root_inode: -1,
            id_table_start: -1,
            xattr_id_table_start: -1,
            inode_table_start: -1,
            directory_table_start: -1,
            fragment_table_start: -1,
            lookup_table_start: -1,
        }
    }

    fn write_file(&mut self, file: &mut fs::File, enc: &mut Compress, f: &'a virtfs::File<T>, buf: &mut [u8]) -> io::Result<INodeType<'a, T>> {
        let blocksize = buf.len();
        let start_block = self.fpos;
        let mut file_size = 0;
        let mut r = f.data.open()?;
        let mut block_list = Vec::new();

        loop {
            let mut len = 0;
            loop {
                let r = r.read(&mut buf[len..])?;
                len += r;
                if r == 0 || len == blocksize {
                    break;
                }
            }
            file_size += len as u64;
            match enc.compress(&mut buf[..len], blocksize) {
                Ok(out) if out.len() < len => {
                    file.write_all(out)?;
                    self.fpos += out.len() as u64;
                    block_list.push(out.len() as u32);
                }
                _ => { /* write it uncompressed */
                    file.write_all(&buf[..len])?;
                    self.fpos += len as u64;
                    block_list.push(len as u32 | 0x100_0000);
                }
            }
            if len < blocksize {
                break;
            }
        }

        Ok(INodeType::File {
            f,
            start_block,
            fragment: SQUASHFS_INVALID_FRAG,
            offset: 0,
            file_size,
            block_list,
        })
    }

    fn write_file_data(&mut self, file: &mut fs::File, enc: &mut Compress) -> io::Result<()> {
        let blocksize = 1usize << self.block_log;
        let mut buf = Vec::with_capacity(blocksize);
        unsafe { buf.set_len(blocksize) };
        for i in 0..self.inodes.len() {
            let f = match self.inodes[i].typ {
                INodeType::File { f, .. } => f,
                _ => continue,
            };
            self.inodes[i].typ = self.write_file(file, enc, f, &mut buf)?;
        }
        Ok(())
    }

    fn write_inode_table(&mut self, file: &mut fs::File, enc: &mut Compress) -> io::Result<()> {
        self.inode_table_start = self.fpos as i64;

        let mut entries: Vec<(usize, &[u8])> = Vec::new();
        let mut itable = MetaStream::<ToFile>::new(file);
        let mut dtable = MetaStream::<ToBuffer>::new();

        for i in 0..self.inodes.len() {
            self.inodes[i].setpos(itable.pos());
            match self.inodes[i].typ {
                INodeType::Dir { d } => {
                    let dpos = dtable.pos();

                    for (name, &idx) in d.entries() {
                        entries.push((self.idx_to_ino[idx], name));
                    }
                    let mut end = 0;
                    let mut size = 0;
                    while end < entries.len() {
                        let start = end;
                        let ino_base = entries[end].0;
                        let start_block = self.inode(ino_base).start_block;
                        end += 1;
                        while end < entries.len() {
                            let ino = entries[end].0;
                            if ino - ino_base > i16::max_value() as usize {
                                break;
                            }
                            if start_block != self.inode(ino).start_block {
                                break;
                            }
                            end += 1;
                        }
                        /* write header */
                        dtable.u32(enc, (end - start - 1) as u32)?; /* count */
                        dtable.u32(enc, start_block)?;              /* start block */
                        dtable.u32(enc, ino_base as u32)?;          /* base ino */
                        size += 12;

                        /* write entries */
                        for &(ino, name) in &entries[start..end] {
                            let node = self.inode(ino);
                            dtable.u16(enc, node.offset)?;             /* offset */
                            dtable.i16(enc, (ino - ino_base) as i16)?; /* ino_add */
                            dtable.u16(enc, node.tmark())?;            /* type mark */
                            dtable.u16(enc, (name.len() - 1) as u16)?; /* name size */
                            dtable.write(enc, name)?;                  /* name */
                            size += 8 + name.len();
                        }
                    }
                    entries.clear();

                    itable.u16(enc, SQUASHFS_DIR_TYPE)?;
                    itable.u16(enc, d.mode)?;
                    itable.u16(enc, self.id_table[&d.uid])?;
                    itable.u16(enc, self.id_table[&d.gid])?;
                    itable.i32(enc, to_timestamp(d.mtime))?;
                    itable.u32(enc, (i + 1) as u32)?;

                    itable.u32(enc, dpos.0)?; /* start_block */
                    itable.u32(enc, d.nlink() as u32)?;
                    itable.u16(enc, size as u16 + 3)?;
                    itable.u16(enc, dpos.1)?; /* offset */
                    let parent = if i == self.inodes.len() - 1 {
                        self.inodes.len() as u32 + 1
                    } else {
                        self.idx_to_ino[d.parent()] as u32
                    };
                    itable.u32(enc, parent)?;
                },
                INodeType::File { f, start_block, fragment, offset, file_size, ref block_list } => {
                    if start_block <= u64::from(u32::max_value())
                        && file_size <= u64::from(u32::max_value())
                            && f.nlink() == 1 {
                        itable.u16(enc, SQUASHFS_FILE_TYPE)?;
                        itable.u16(enc, f.mode)?;
                        itable.u16(enc, self.id_table[&f.uid])?;
                        itable.u16(enc, self.id_table[&f.gid])?;
                        itable.i32(enc, to_timestamp(f.mtime))?;
                        itable.u32(enc, (i + 1) as u32)?;

                        itable.u32(enc, start_block as u32)?;
                        itable.u32(enc, fragment)?;
                        itable.u32(enc, offset)?;
                        itable.u32(enc, file_size as u32)?;
                        for &block in block_list {
                            itable.u32(enc, block)?;
                        }
                    } else {
                        itable.u16(enc, SQUASHFS_LREG_TYPE)?;
                        itable.u16(enc, f.mode)?;
                        itable.u16(enc, self.id_table[&f.uid])?;
                        itable.u16(enc, self.id_table[&f.gid])?;
                        itable.i32(enc, to_timestamp(f.mtime))?;
                        itable.u32(enc, (i + 1) as u32)?;

                        itable.i64(enc, start_block as i64)?;
                        itable.i64(enc, file_size as i64)?;
                        itable.i64(enc, 0)?; /* sparse */
                        itable.u32(enc, f.nlink() as u32)?;
                        itable.u32(enc, fragment)?;
                        itable.u32(enc, offset)?;
                        itable.u32(enc, SQUASHFS_INVALID_XATTR)?;
                        for &block in block_list {
                            itable.u32(enc, block)?;
                        }
                    }
                },
                INodeType::Symlink { l } => {
                    itable.u16(enc, SQUASHFS_SYMLINK_TYPE)?;
                    itable.u16(enc, l.mode)?;
                    itable.u16(enc, self.id_table[&l.uid])?;
                    itable.u16(enc, self.id_table[&l.gid])?;
                    itable.i32(enc, to_timestamp(l.mtime))?;
                    itable.u32(enc, (i + 1) as u32)?;

                    let tgt = l.target();
                    itable.u32(enc, l.nlink() as u32)?;
                    itable.u32(enc, tgt.len() as u32)?;
                    itable.write(enc, tgt)?;
                },
                INodeType::BlockDev { b } => {
                    itable.u16(enc, SQUASHFS_BLKDEV_TYPE)?;
                    itable.u16(enc, b.mode)?;
                    itable.u16(enc, self.id_table[&b.uid])?;
                    itable.u16(enc, self.id_table[&b.gid])?;
                    itable.i32(enc, to_timestamp(b.mtime))?;
                    itable.u32(enc, (i + 1) as u32)?;

                    itable.u32(enc, b.nlink() as u32)?;
                    itable.u32(enc, b.rdev)?;
                },
                INodeType::CharDev { c } => {
                    itable.u16(enc, SQUASHFS_CHRDEV_TYPE)?;
                    itable.u16(enc, c.mode)?;
                    itable.u16(enc, self.id_table[&c.uid])?;
                    itable.u16(enc, self.id_table[&c.gid])?;
                    itable.i32(enc, to_timestamp(c.mtime))?;
                    itable.u32(enc, (i + 1) as u32)?;

                    itable.u32(enc, c.nlink() as u32)?;
                    itable.u32(enc, c.rdev)?;
                },
                INodeType::Fifo { p } => {
                    itable.u16(enc, SQUASHFS_FIFO_TYPE)?;
                    itable.u16(enc, p.mode)?;
                    itable.u16(enc, self.id_table[&p.uid])?;
                    itable.u16(enc, self.id_table[&p.gid])?;
                    itable.i32(enc, to_timestamp(p.mtime))?;
                    itable.u32(enc, (i + 1) as u32)?;

                    itable.u32(enc, p.nlink() as u32)?;
                },
                INodeType::Socket { s } => {
                    itable.u16(enc, SQUASHFS_SOCKET_TYPE)?;
                    itable.u16(enc, s.mode)?;
                    itable.u16(enc, self.id_table[&s.uid])?;
                    itable.u16(enc, self.id_table[&s.gid])?;
                    itable.i32(enc, to_timestamp(s.mtime))?;
                    itable.u32(enc, (i + 1) as u32)?;

                    itable.u32(enc, s.nlink() as u32)?;
                },
            }
        }

        itable.flush(enc)?;
        self.fpos += itable.out.pos() as u64;

        self.directory_table_start = self.fpos as i64;
        dtable.flush(enc)?;
        itable.out.file.write_all(&dtable.out.0)?;
        self.fpos += dtable.out.pos() as u64;

        self.root_inode = {
            let root = &self.inodes[self.inodes.len() - 1];
            i64::from(root.start_block) << 16 | i64::from(root.offset)
        };

        Ok(())
    }

    fn inode(&self, ino: usize) -> &INode<T> {
        &self.inodes[ino - 1]
    }

    fn write_fragment_table(&mut self, _file: &mut fs::File) -> io::Result<()> {
        if self.fragments == 0 {
            /* squashfs-tools sets this even when there are no fragments
             * let's do the same for compatibility */
            self.fragment_table_start = self.fpos as i64;
            return Ok(());
        }

        /* TODO */
        Err(io::Error::new(
            io::ErrorKind::Other,
            "fragment table not implemented"
        ))
    }

    fn write_id_table(&mut self, file: &mut fs::File, enc: &mut Compress) -> io::Result<()> {
        let mut index = {
            let ids = self.id_table.len();
            let len = (4*ids + SQUASHFS_METADATA_SIZE - 1) / SQUASHFS_METADATA_SIZE;
            Vec::with_capacity(len)
        };

        {
            let mut ms = MetaStream::<ToFile>::new(file);
            for &v in self.id_table.keys() {
                if ms.offset == 0 {
                    index.push(self.fpos + ms.out.pos() as u64);
                }
                ms.u32(enc, v)?;
            }
            ms.flush(enc)?;
            self.fpos += ms.out.pos() as u64;
        }

        let mut buf: [u8; 256] = unsafe { mem::uninitialized() };
        let mut pos = 0;
        for v in index {
            LE::write_u64(&mut buf[pos..], v);
            pos += 8;
        }
        self.id_table_start = self.fpos as i64;
        file.write_all(&buf[..pos])?;
        self.fpos += pos as u64;

        Ok(())
    }

    fn write_superblock(&mut self, file: &mut fs::File) -> io::Result<()> {
        let mkfs_time = match time::SystemTime::now().duration_since(time::UNIX_EPOCH) {
            Ok(n) => n.as_secs() as i32,
            Err(e) => return Err(io::Error::new(
                io::ErrorKind::Other,
                e.description()
            )),
        };
        let mut buf: [u8; SQUASHFS_SUPERBLOCK_SIZE] = unsafe { mem::uninitialized() };
        LE::write_u32(&mut buf[ 0..], SQUASHFS_MAGIC);
        LE::write_u32(&mut buf[ 4..], self.inodes.len() as u32); /* inodes */
        LE::write_i32(&mut buf[ 8..], mkfs_time);
        LE::write_u32(&mut buf[12..], 1u32 << self.block_log); /* block_size */
        LE::write_u32(&mut buf[16..], self.fragments);
        LE::write_u16(&mut buf[20..], self.comp.tag()); /* compression */
        LE::write_u16(&mut buf[22..], self.block_log);
        LE::write_u16(&mut buf[24..], self.flags);
        LE::write_u16(&mut buf[26..], self.id_table.len() as u16); /* no_ids */
        LE::write_u16(&mut buf[28..], 4); /* vers_major */
        LE::write_u16(&mut buf[30..], 0); /* vers_minor */
        LE::write_i64(&mut buf[32..], self.root_inode);
        LE::write_i64(&mut buf[40..], self.fpos as i64); /* bytes_used */
        LE::write_i64(&mut buf[48..], self.id_table_start);
        LE::write_i64(&mut buf[56..], self.xattr_id_table_start);
        LE::write_i64(&mut buf[64..], self.inode_table_start);
        LE::write_i64(&mut buf[72..], self.directory_table_start);
        LE::write_i64(&mut buf[80..], self.fragment_table_start);
        LE::write_i64(&mut buf[88..], self.lookup_table_start);
        file.write_all(&buf)
    }
}

pub fn write<T : FileData>(fs: &virtfs::VirtFS<T>, file: &mut fs::File, offset: u64) -> io::Result<()> {
    let mut md = MetaData::new(fs);

    if md.id_table.len() > u16::max_value() as usize {
        return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "too many different ids"
        ));
    }

    let mut enc = md.comp.encoder(1usize << md.block_log)?;

    file.seek(io::SeekFrom::Start(offset + md.fpos))?;
    md.write_file_data(file, enc.deref_mut())?;
    md.write_inode_table(file, enc.deref_mut())?;
    md.write_fragment_table(file)?;
    md.write_id_table(file, enc.deref_mut())?;

    let tail = md.fpos % 4096;
    if tail != 0 {
        let aligned_end = md.fpos - tail + 4095;
        file.seek(io::SeekFrom::Start(offset + aligned_end))?;
        file.write_all(b"\0")?;
    }

    file.seek(io::SeekFrom::Start(offset))?;
    md.write_superblock(file)
}
