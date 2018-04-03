use std::{io, fs};
use std::ffi::OsStr;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::OpenOptionsExt;
use std::path::Path;
use std::path::PathBuf;

extern crate time;
use time::Timespec;

extern crate clap;
use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};

extern crate squashfs;
use squashfs::virtfs;
use squashfs::unsquash;

extern crate yaml_rust;
mod yaml;
mod plan;

fn tostr(buf: &[u8]) -> &str {
    std::str::from_utf8(buf).unwrap_or("<non-utf8>")
}

fn fail<T>() -> T {
    ::std::process::exit(1)
}

fn get_input<'a>(m: &'a ArgMatches) -> Option<(&'a OsStr, u64)> {
    let offset = if let Some(s) = m.value_of("offset") {
        match u64::from_str_radix(s, 10) {
            Ok(v)  => v,
            Err(e) => {
                eprintln!("Error parsing offset: {}", e);
                return None;
            }
        }
    } else {
        0
    };
    Some((m.value_of_os("INPUT").unwrap(), offset))
}

fn superblock(path: &OsStr, offset: u64) -> io::Result<()> {
    let file = fs::File::open(path)?;
    let sqfs = unsquash::SquashFS::new(Box::new(file), offset)?;
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

fn list(path: &OsStr, offset: u64, m: &ArgMatches) -> io::Result<()> {
    let file = fs::File::open(path)?;
    let sqfs = unsquash::SquashFS::new(Box::new(file), offset)?;
    let mut dec = sqfs.decompressor()?;
    let recursive = m.is_present("recursive");
    let prefix = m.value_of_os("PATH").map(|p| p.as_bytes()).unwrap_or(b"");
    let ids = dec.ids()?;
    let mut entries = Vec::new();
    let mut path = Vec::new();
    let mut stack = Vec::new();

    if prefix.is_empty() {
        path.push(b'/')
    } else {
        path.extend_from_slice(prefix);
    }
    stack.push(Some((Box::new([]) as Box<[u8]>, Box::new(dec.lookup(prefix)?))));

    while let Some(e) = stack.pop() {
        if let Some((name, node)) = e {
            path.extend_from_slice(&name);
            let typc = node.typechar();
            let ino = node.ino();
            let nlink = node.nlink();
            let mode = node.mode();
            let uid = ids[node.uid_idx()];
            let gid = ids[node.gid_idx()];
            match node.typ {
                unsquash::INodeType::Dir { parent, .. } => {
                    println!("{} {:3} {:2} {:4} {:4} {:4o} {} {}", typc, ino, nlink, uid, gid, mode,
                             tostr(&path), parent);
                    if recursive || stack.is_empty() {
                        for p in dec.readdir(&node)? {
                            entries.push(p?)
                        }
                        stack.push(None);
                        while let Some(e) = entries.pop() {
                            stack.push(Some(e));
                        }
                        if let Some(&b'/') = path.last() {
                        } else {
                            path.push(b'/');
                        }
                        continue;
                    }
                }
                unsquash::INodeType::File { file_size, .. } => {
                    println!("{} {:3} {:2} {:4} {:4} {:4o} {} {}", typc, ino, nlink, uid, gid, mode,
                             tostr(&path), file_size );
                }
                unsquash::INodeType::Symlink { ref tgt } => {
                    println!("{} {:3} {:2} {:4} {:4} {:4o} {} -> {}", typc, ino, nlink, uid, gid, mode,
                             tostr(&path), tostr(tgt));
                }
                unsquash::INodeType::CharDev { rdev } |
                unsquash::INodeType::BlockDev { rdev } => {
                    let maj = rdev >> 8;
                    let min = rdev & 0xff;
                    println!("{} {:3} {:2} {:4} {:4} {:4o} {} {}:{}", typc, ino, nlink, uid, gid, mode,
                             tostr(&path), maj, min);
                }
                unsquash::INodeType::Fifo |
                unsquash::INodeType::Socket => {
                    println!("{} {:3} {:2} {:4} {:4} {:4o} {}", typc, ino, nlink, uid, gid, mode,
                             tostr(&path));
                }
            }
        } else {
            path.pop();
        }
        while !path.is_empty() && path[path.len() - 1] != b'/' {
            path.pop();
        }
    }

    Ok(())
}

fn contents(path: &OsStr, offset: u64, m: &ArgMatches) -> io::Result<()> {
    let file = fs::File::open(path)?;
    let sqfs = unsquash::SquashFS::new(Box::new(file), offset)?;
    let mut dec = sqfs.decompressor()?;
    let path = m.value_of_os("PATH").map(|p| p.as_bytes()).unwrap();
    let node = dec.lookup(path)?;

    dec.write(&node, &mut io::stdout())?;

    Ok(())
}

fn extract(path: &OsStr, offset: u64, m: &ArgMatches) -> io::Result<()> {
    let file = fs::File::open(path)?;
    let sqfs = unsquash::SquashFS::new(Box::new(file), offset)?;
    let mut dec = sqfs.decompressor()?;
    let prefix = m.value_of_os("PATH").map(|p| p.as_bytes()).unwrap_or(b"");
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

fn plan(path: &OsStr, _m: &ArgMatches) -> io::Result<()> {
    let res = plan::parsefile(path)?;
    if res.len() < 1 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "no YAML document found",
        ));
    }

    let mut fs = virtfs::FS::new();
    plan::add(&mut fs, &res[0])?;

    print!("{}", fs);
    println!("validate = {}", fs.validate());

    Ok(())
}

fn main() {
    let offset = Arg::with_name("offset")
        .short("o")
        .long("offset")
        .takes_value(true)
        .value_name("OFFSET")
        .help("Byte offset into INPUT");
    let input = Arg::with_name("INPUT")
        .help("SquashFS file")
        .required(true);
    let matches = App::new("sqfs")
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
        .get_matches();

    match matches.subcommand() {
        ("superblock", Some(m)) => {
            let (path, offset) = get_input(m).unwrap_or_else(fail);

            if let Err(e) = superblock(path, offset) {
                eprintln!("Error reading '{}': {}", Path::new(path).display(), e);
                fail::<()>();
            }
        }
        ("list", Some(m)) => {
            let (path, offset) = get_input(m).unwrap_or_else(fail);

            if let Err(e) = list(path, offset, m) {
                eprintln!("Error reading '{}': {}", Path::new(path).display(), e);
                fail::<()>();
            }
        }
        ("contents", Some(m)) => {
            let (path, offset) = get_input(m).unwrap_or_else(fail);

            if let Err(e) = contents(path, offset, m) {
                eprintln!("Error reading '{}': {}", Path::new(path).display(), e);
                fail::<()>();
            }
        }
        ("extract", Some(m)) => {
            let (path, offset) = get_input(m).unwrap_or_else(fail);

            if let Err(e) = extract(path, offset, m) {
                eprintln!("Error reading '{}': {}", Path::new(path).display(), e);
                fail::<()>();
            }
        }
        ("plan", Some(m)) => {
            let path = m.value_of_os("PLAN").unwrap();

            if let Err(e) = plan(path, m) {
                eprintln!("Error enacting '{}': {}", Path::new(path).display(), e);
                fail::<()>();
            }
        }
        _ => {
            println!("What to do..");
            fail::<()>();
        }
    }
}
