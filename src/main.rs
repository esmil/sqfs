use std::{io, fs};
use std::io::Read;
use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;
use std::path::PathBuf;

use time::Timespec;

use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};

use virtfs::VirtFS;
use squashfs::unsquash;
use squashfs::squash;

mod annojson;
mod yaml;
mod plan;

type PResult<T> = Result<T, i32>;

fn failure<T>() -> PResult<T> {
    Err(1)
}

struct FileBlockReader(fs::File);

impl squashfs::ReadBlock for FileBlockReader {
    fn readblock(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut len = 0;
        loop {
            let chunk = self.0.read(&mut buf[len..])?;
            if chunk == 0 {
                break;
            }
            len += chunk;
            if len == buf.len() {
                break;
            }
        }
        Ok(len)
    }
}

struct MemBlockReader<'a> {
    data: &'a [u8],
    idx: usize,
}

impl<'a> squashfs::ReadBlock for MemBlockReader<'a> {
    fn readblock(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let chunk = std::cmp::min(self.data.len() - self.idx, buf.len());
        buf[..chunk].copy_from_slice(&self.data[self.idx..(self.idx + chunk)]);
        self.idx += chunk;
        Ok(chunk)
    }
}

enum FileData {
    Path(PathBuf),
    Static(&'static [u8]),
}

impl squashfs::FileData for FileData {
    fn open(&self) -> io::Result<Box<dyn squashfs::ReadBlock>> {
        match *self {
            FileData::Path(ref p) => {
                println!("Opening '{}'", p.to_str().unwrap_or("<non-utf8>"));
                Ok(Box::new(FileBlockReader(std::fs::File::open(p)?)))
            }
            FileData::Static(buf) => {
                Ok(Box::new(MemBlockReader {
                    data: buf,
                    idx: 0,
                }))
            }
        }
    }
}

fn to_str(buf: &[u8]) -> &str {
    std::str::from_utf8(buf).unwrap_or("<non-utf8>")
}

fn get_input<'a>(m: &'a ArgMatches) -> PResult<(&'a OsStr, unsquash::SquashFS)> {
    let offset = if let Some(s) = m.value_of("offset") {
        match u64::from_str_radix(s, 10) {
            Ok(v)  => v,
            Err(e) => {
                eprintln!("Error parsing offset '{}': {}", s, e);
                return failure();
            }
        }
    } else {
        0
    };
    let path = m.value_of_os("INPUT").unwrap();
    let file = match fs::OpenOptions::new().read(true).open(&path) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Error opening '{}': {}",
                      Path::new(path).display(), e);
            return failure();
        }
    };
    let sqfs = match unsquash::SquashFS::new(Box::new(file), offset) {
        Ok(sqfs) => sqfs,
        Err(e) => {
            eprintln!("Error reading '{}': {}",
                      Path::new(path).display(), e);
            return failure();
        }
    };
    Ok((path, sqfs))
}

fn superblock(m: &ArgMatches) -> PResult<()> {
    let (_, sqfs) = get_input(m)?;
    let sb = &sqfs.sb;

    println!("inodes:                {}", sb.inodes);
    println!("mkfs_time:             {}",
        time::at(Timespec::new(i64::from(sb.mkfs_time), 0)).rfc822());
    println!("block_size:            {}", sb.block_size);
    println!("fragments:             {}", sb.fragments);
    println!("compression:           {}", sb.compression);
    println!("block_log:             {}", sb.block_log);
    print!(  "flags:                ");
    if sb.flags == 0    { print!(" none"); }
    if sb.noi()         { print!(" noi"); }
    if sb.nod()         { print!(" nod"); }
    if sb.check()       { print!(" check"); }
    if sb.nof()         { print!(" nof"); }
    if sb.no_frag()     { print!(" no_frag"); }
    if sb.always_frag() { print!(" always_frag"); }
    if sb.duplicate()   { print!(" duplicate"); }
    if sb.export()      { print!(" export"); }
    if sb.nox()         { print!(" nox"); }
    if sb.no_xattr()    { print!(" no_xattr"); }
    if sb.comp_opt()    { print!(" comp_opt"); }
    println!("\nno_ids:                {}", sb.no_ids);
    println!("vers_major:            {}", sb.vers_major);
    println!("vers_minor:            {}", sb.vers_minor);
    if sb.root_inode > 0 {
        let n = sb.root_inode as u64;
        println!("root_inode:            ({},{})", n >> 16, n & 0xffff);
    } else {
        println!("root_inode:            {}", sb.root_inode);
    }
    println!("bytes_used:            {}", sb.bytes_used);
    println!("id_table_start:        {}", sb.id_table_start);
    println!("xattr_id_table_start:  {}", sb.xattr_id_table_start);
    println!("inode_table_start:     {}", sb.inode_table_start);
    println!("directory_table_start: {}", sb.directory_table_start);
    println!("fragment_table_start:  {}", sb.fragment_table_start);
    println!("lookup_table_start:    {}", sb.lookup_table_start);

    Ok(())
}

fn list_show(sqfs: &unsquash::SquashFS, prefix: &[u8], recursive: bool) -> io::Result<()> {
    let mut dec = sqfs.decompressor()?;
    let mut first = true;
    let ids = dec.ids()?;
    let mut entries = Vec::new();
    let mut path = String::new();
    let mut stack = Vec::new();

    if prefix.is_empty() {
        path.push('/')
    } else {
        path.push_str(to_str(prefix));
    }
    stack.push((Box::new([]) as Box<[u8]>, Box::new(dec.lookup(prefix)?), path.len()));

    while let Some((name, node, len)) = stack.pop() {
        path.truncate(len);
        path.push_str(to_str(name.as_ref()));
        let typc = node.typechar();
        let ino = node.ino();
        let nlink = node.nlink();
        let mode = node.mode();
        let uid = ids[node.uid_idx()];
        let gid = ids[node.gid_idx()];
        match node.typ {
            unsquash::INodeType::Dir { parent, .. } => {
                println!("{} {:3} {:2} {:4} {:4} {:4o} {} {}", typc, ino, nlink, uid, gid, mode,
                         &path, parent);
                if recursive || first {
                    first = false;
                    if !path.ends_with('/') {
                        path.push('/');
                    }
                    for p in dec.readdir(&node)? {
                        entries.push(p?)
                    }
                    while let Some((name, node)) = entries.pop() {
                        stack.push((name, node, path.len()));
                    }
                }
            }
            unsquash::INodeType::File { file_size, .. } => {
                println!("{} {:3} {:2} {:4} {:4} {:4o} {} {}", typc, ino, nlink, uid, gid, mode,
                         &path, file_size );
            }
            unsquash::INodeType::Symlink { ref tgt } => {
                println!("{} {:3} {:2} {:4} {:4} {:4o} {} -> {}", typc, ino, nlink, uid, gid, mode,
                         &path, to_str(tgt));
            }
            unsquash::INodeType::CharDev { rdev } |
            unsquash::INodeType::BlockDev { rdev } => {
                let maj = rdev >> 8;
                let min = rdev & 0xff;
                println!("{} {:3} {:2} {:4} {:4} {:4o} {} {}:{}", typc, ino, nlink, uid, gid, mode,
                         &path, maj, min);
            }
            unsquash::INodeType::Fifo |
            unsquash::INodeType::Socket => {
                println!("{} {:3} {:2} {:4} {:4} {:4o} {}", typc, ino, nlink, uid, gid, mode,
                         &path);
            }
        }
    }

    Ok(())
}

fn list(m: &ArgMatches) -> PResult<()> {
    let (path, sqfs) = get_input(m)?;
    let prefix = m.value_of_os("PATH").map(|p| p.as_bytes()).unwrap_or(b"");
    let recursive = m.is_present("recursive");

    if let Err(e) = list_show(&sqfs, prefix, recursive) {
        eprintln!("Error reading '{}': {}",
                  Path::new(path).display(), e);
        return failure()
    }
    Ok(())
}

fn contents(m: &ArgMatches) -> PResult<()> {
    let (input, sqfs) = get_input(m)?;
    let path = m.value_of_os("PATH").unwrap().as_bytes();
    let mut dec = match sqfs.decompressor() {
        Ok(dec) => dec,
        Err(e) => {
            eprintln!("Error reading '{}': {}",
                      Path::new(input).display(), e);
            return failure();
        }
    };
    let node = match dec.lookup(path) {
        Ok(node) => node,
        Err(e) => {
            eprintln!("Error looking up '{}' in '{}': {}",
                      to_str(path), Path::new(input).display(), e);
            return failure();
        }
    };
    if let Err(e) = dec.write(&node, &mut io::stdout()) {
            eprintln!("Error reading '{}' from '{}': {}",
                      to_str(path), Path::new(input).display(), e);
            return failure();
    }
    Ok(())
}

fn extract_all(sqfs: &unsquash::SquashFS, prefix: &[u8]) -> io::Result<()> {
    let mut dec = sqfs.decompressor()?;
    //let ids = dec.ids()?;
    let mut entries = Vec::new();
    let mut path = PathBuf::from("squashfs-root");
    let mut stack = Vec::new();

    stack.push(Some((Box::new([]) as Box<[u8]>, Box::new(dec.lookup(prefix)?))));

    while let Some(e) = stack.pop() {
        if let Some((name, node)) = e {
            path.push(OsStr::from_bytes(&name));
            println!("{}", path.display());
            match node.typ {
                unsquash::INodeType::Dir { .. } => {
                    std::fs::create_dir(&path)?;
                    for p in dec.readdir(&node)? {
                        entries.push(p?)
                    }
                    stack.push(None);
                    while let Some(e) = entries.pop() {
                        stack.push(Some(e));
                    }
                    continue;
                }
                unsquash::INodeType::File { .. } => {
                    let mut file = fs::OpenOptions::new()
                        .write(true)
                        .create_new(true)
                        .mode(u32::from(node.mode()))
                        .open(&path)?;

                    dec.write(&node, &mut file)?;
                }
                unsquash::INodeType::Symlink { ref tgt, .. } => {
                    std::os::unix::fs::symlink(OsStr::from_bytes(tgt), &path)?;
                }
                _ => {
                }
            }
        }
        path.pop();
    }

    Ok(())
}

fn extract(m: &ArgMatches) -> PResult<()> {
    let (path, sqfs) = get_input(m)?;
    let prefix = m.value_of_os("PATH").map(|p| p.as_bytes()).unwrap_or(b"");

    if let Err(e) = extract_all(&sqfs, prefix) {
        eprintln!("Error extracting from '{}': {}",
                  Path::new(path).display(), e);
        return failure();
    }
    Ok(())
}

fn plan(m: &ArgMatches) -> PResult<()> {
    let path = m.value_of_os("PLAN").unwrap();
    let res = match plan::parsefile(path) {
        Ok(res) => res,
        Err(e) => {
            eprintln!("Error parsing '{}': {}",
                      Path::new(path).display(), e);
            return failure();
        }
    };

    let mut fs = VirtFS::<FileData>::new();
    plan::add(&mut fs, &res)?;

    /*
    fs.newfile(b"/file", FileData::Static(b"xyz\n"))?;
    fs.newfile(b"/Makefile", FileData::Path(PathBuf::from("Makefile")))?;
    fs.hardlink(b"/boot/Makefile", b"/Makefile")?;
    fs.mkdir(b"/dev")?;
    fs.blockdev(b"/dev/loop0", 7 << 8 | 0)?.chmod(0o660);
    fs.chardev(b"/dev/null", 1 << 8 | 3)?.chmod(0o666);
    fs.socket(b"/dev/log")?.chmod(0o666);
    */

    print!("{}", fs);

    let output = "test.sqfs";

    let mut file = match fs::OpenOptions::new()
        .write(true)
        .create(true)
        .open(output) {
            Ok(file) => file,
            Err(e) => {
                eprintln!("Error creating '{}': {}",
                          output, e);
                return failure();
            }
        };

    if let Err(e) = squash::write(&fs, &mut file, 0) {
        eprintln!("Error writing filesystem to '{}': {}",
                  output, e);
        return failure();
    }
    Ok(())
}

fn parse_args<'a>() -> ArgMatches<'a> {
    let offset = Arg::with_name("offset")
        .short("o")
        .long("offset")
        .takes_value(true)
        .value_name("OFFSET")
        .help("Byte offset into INPUT");
    let input = Arg::with_name("INPUT")
        .help("SquashFS file")
        .required(true);
    App::new("sqfs")
        .version("0.0.1")
        //.usage("")
        //.author("Emil Renner Berthing")
        .about("parse squashfs files")
        .setting(AppSettings::ColoredHelp)
        .subcommand(SubCommand::with_name("superblock")
            .about("dump superblock")
            .visible_alias("sb")
            .setting(AppSettings::ColoredHelp)
            .arg(&offset).arg(&input))
        .subcommand(SubCommand::with_name("list")
            .about("list contents")
            .visible_alias("ls")
            .setting(AppSettings::ColoredHelp)
            .arg(&offset).arg(&input)
            .arg(Arg::with_name("recursive")
                 .short("r")
                 .long("recursive")
                 .help("Show recursive contents of all subdirectories"))
            .arg(Arg::with_name("PATH")
                .help("List only PATH (and files below it)")))
        .subcommand(SubCommand::with_name("contents")
            .about("dump contents of a file to stdout")
            .visible_alias("cat")
            .setting(AppSettings::ColoredHelp)
            .arg(&offset).arg(&input)
            .arg(Arg::with_name("PATH")
                .help("path to file")
                .required(true)))
        .subcommand(SubCommand::with_name("extract")
            .about("extract all files from squashfs")
            .visible_alias("x")
            .setting(AppSettings::ColoredHelp)
            .arg(&offset).arg(&input))
        .subcommand(SubCommand::with_name("plan")
            .about("create squashfs from plan")
            .visible_alias("p")
            .setting(AppSettings::ColoredHelp)
            .arg(Arg::with_name("PLAN")
                .help("plan file")
                .required(true)))
        .get_matches()
}

fn main() {
    let ret = match parse_args().subcommand() {
        ("superblock", Some(m)) => superblock(m),
        ("list",       Some(m)) => list(m),
        ("contents",   Some(m)) => contents(m),
        ("extract",    Some(m)) => extract(m),
        ("plan",       Some(m)) => plan(m),
        _ => {
            eprintln!("What to do..");
            failure()
        }
    };
    if let Err(v) = ret {
        ::std::process::exit(v);
    }
}
