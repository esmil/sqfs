use std::io;
use std::ffi::OsStr;
use std::fs::File;
use std::path::Path;
use std::os::unix::ffi::OsStrExt;

extern crate time;
use time::Timespec;

extern crate clap;
use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};

extern crate squashfs;

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
    let file = File::open(path)?;
    let sqfs = squashfs::SquashFS::new(Box::new(file), offset)?;
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

fn list_entry(name: &[u8], node: &squashfs::INode, ids: &[u32]) {
    let typc = node.typechar();
    let mode = node.mode();
    let uid = ids[node.uid_idx()];
    let gid = ids[node.gid_idx()];
    let path = tostr(name);
    match node.typ {
        squashfs::INodeType::File { file_size, .. } => {
            println!("{} {:4} {:4} {:4o} {} {}", typc, uid, gid, mode,
                     path, file_size );
        }
        squashfs::INodeType::Symlink { ref tgt, .. } => {
            println!("{} {:4} {:4} {:4o} {} -> {}", typc, uid, gid, mode,
                     path, tostr(tgt));
        }
        squashfs::INodeType::CharDev { rdev, .. } |
        squashfs::INodeType::BlockDev { rdev, .. } => {
            let maj = rdev >> 8;
            let min = rdev & 0xff;
            println!("{} {:4} {:4} {:4o} {} {}:{}", typc, uid, gid, mode,
                     path, maj, min);
        }
        squashfs::INodeType::Fifo { .. } |
        squashfs::INodeType::Socket { .. } |
        squashfs::INodeType::Dir { .. } => {
            println!("{} {:4} {:4} {:4o} {}", typc, uid, gid, mode,
                     path);
        }
    }
}

fn list(path: &OsStr, offset: u64, m: &ArgMatches) -> io::Result<()> {
    let file = File::open(path)?;
    let sqfs = squashfs::SquashFS::new(Box::new(file), offset)?;
    let prefix = m.value_of_os("PATH").map(|p| p.as_bytes()).unwrap_or(b"");
    let ids = sqfs.ids()?;

    let node = sqfs.lookup(prefix)?;
    match node.typ {
        squashfs::INodeType::Dir { .. } => {
            let mut des = sqfs.direntries(&node)?;
            while let Some(de) = des.snext()? {
                let node = sqfs.node(&de)?;
                list_entry(de.name(), &node, &ids);
            }
        }
        _ => {
            list_entry(prefix, &node, &ids);
        }
    }

    Ok(())
}

fn contents(path: &OsStr, offset: u64, m: &ArgMatches) -> io::Result<()> {
    let file = File::open(path)?;
    let sqfs = squashfs::SquashFS::new(Box::new(file), offset)?;
    let path = m.value_of_os("PATH").map(|p| p.as_bytes()).unwrap();
    let node = sqfs.lookup(path)?;

    sqfs.write(&node, &mut io::stdout())?;

    Ok(())
}

fn main() {
    let offset = Arg::with_name("offset")
        .short("o")
        .long("offset")
        .takes_value(true)
        .value_name("OFFSET")
        .help("byte offset into INPUT");
    let input = Arg::with_name("INPUT")
        .help("squashfs file")
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
            .arg(Arg::with_name("PATH")
                .help("list only PATH and files it")))
        .subcommand(SubCommand::with_name("contents")
            .about("dump contents of a file to stdout")
            .visible_alias("cat")
            .setting(AppSettings::ColoredHelp)
            .arg(&offset).arg(&input)
            .arg(Arg::with_name("PATH")
                .help("path to file")
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

        _ => {
            println!("What to do..");
            fail::<()>();
        }
    }
}
