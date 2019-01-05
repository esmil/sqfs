use std::io;
use std::path::{Path, PathBuf};
use std::os::unix::ffi::OsStrExt;

use super::{ProgResult, failure, VirtFS};
use super::annojson::{AnnoJSON, ExpectMap, ExpectError};

impl<'a> std::convert::From<ExpectError<'a>> for super::ProgError {
    fn from(e: ExpectError<'a>) -> Self {
        eprintln!("{}", e);
        super::PROG_FAILURE
    }
}

fn set_attributes<T>(node: &mut virtfs::Node<T>, m: &ExpectMap) -> ProgResult<()> {
    if let Some(n) = m.maybe_int_range("owner", 0, i64::from(u32::max_value()))? {
        node.chown(n as u32);
    }

    if let Some(n) = m.maybe_int_range("group", 0, i64::from(u32::max_value()))? {
        node.chgrp(n as u32);
    }

    if let Some(n) = m.maybe_int_range("mode", 0, i64::from(u16::max_value()))? {
        node.chmod(n as u16);
    }
    Ok(())
}

fn mkdir(fs: &mut VirtFS, m: &ExpectMap) -> ProgResult<()> {
    let dest = m.str("dest")?;
    let n = match fs.mkdir(dest.as_bytes()) {
        Ok(n) => n,
        Err(e) => {
            eprintln!("error creating directory '{}': {}",
                      dest, e);
            return failure();
        }
    };
    set_attributes(n, m)
}

fn symlink(fs: &mut VirtFS, m: &ExpectMap) -> ProgResult<()> {
    let dest = m.str("dest")?;
    let tgt = m.str("target")?;
    let n = match fs.symlink(dest.as_bytes(), tgt.as_bytes()) {
        Ok(n) => n,
        Err(e) => {
            eprintln!("error creating symlink '{}' -> '{}': {}",
                      dest, tgt, e);
            return failure();
        }
    };
    set_attributes(n, m)
}

enum NodeType {
    Block,
    Char,
}

fn mknod(fs: &mut VirtFS, m: &ExpectMap) -> ProgResult<()> {
    let dest = m.str("dest")?;
    let typ = match m.str("type")? {
        "b" | "block" | "blockdev" => NodeType::Block,
        "c" | "char" | "chardev"   => NodeType::Char,
        _ => {
            eprintln!("type must be 'b', 'block', 'c' or 'char'");
            return failure();
        }
    };
    let major = m.int_range("major", 0, 256)? as u32;
    let minor = m.int_range("minor", 0, 256)? as u32;
    let n = match typ {
        NodeType::Block => {
            match fs.blockdev(dest.as_bytes(), major << 8 | minor) {
                Ok(n) => n,
                Err(e) => {
                    eprintln!("error creating block device '{}': {}",
                              dest, e);
                    return failure();
                }
            }
        }
        NodeType::Char => {
            match fs.chardev(dest.as_bytes(), major << 8 | minor) {
                Ok(n) => n,
                Err(e) => {
                    eprintln!("error creating character device '{}': {}",
                              dest, e);
                    return failure();
                }
            }
        }
    };
    set_attributes(n, m)
}

fn treewalk(fs: &mut VirtFS, sdir: &Path, sdest: &str) -> io::Result<()> {
    let mut stack = Vec::new();
    let mut path = PathBuf::from(sdir);
    let mut dest = Vec::new();

    dest.extend_from_slice(sdest.as_bytes());
    while let Some(b'/') = dest.last() {
        dest.pop();
    }
    fs.mkdir(&dest)?;
    dest.push(b'/');

    stack.push((sdir.read_dir()?, dest.len()));
    loop {
        let (next, len) = if let Some((rd, len)) = stack.last_mut() {
            (rd.next(), *len)
        } else {
            break;
        };
        if let Some(res) = next {
            let entry = res?;
            {
                let name = entry.file_name();
                dest.truncate(len);
                dest.extend_from_slice(name.as_os_str().as_bytes());
                path.push(name);
            }
            let md = entry.metadata()?;
            let mtime = md.modified()?;
            let ft = md.file_type();
            if ft.is_file() {
                fs.newfile(&dest, path.clone())?.touch(mtime);
                path.pop();
            } else if ft.is_symlink() {
                let target = path.read_link()?;
                fs.symlink(&dest, target.as_os_str().as_bytes())?.touch(mtime);
                path.pop();
            } else if ft.is_dir() {
                fs.mkdir(&dest)?.touch(mtime);
                dest.push(b'/');
                stack.push((path.read_dir()?, dest.len()));
            } else {
                eprintln!("unknown file type for '{}'",
                          path.display());
                path.pop();
            }
        } else {
            path.pop();
            stack.pop();
        }
    }
    Ok(())
}

pub fn files(fs: &mut VirtFS, m: &ExpectMap) -> ProgResult<()> {
    let dest = m.str("dest")?;
    let path = PathBuf::from(m.str("path")?);
    let md = match path.symlink_metadata() {
        Ok(md) => md,
        Err(e) => {
            eprintln!("error opening '{}': {}",
                      path.display(), e);
            return failure();
        }
    };

    if md.is_dir() {
        return match treewalk(fs, &path, dest) {
            Ok(()) => Ok(()),
            Err(e) => {
                eprintln!("error walking '{}': {}",
                          path.display(), e);
                failure()
            }
        }
    }
    let mtime = match md.modified() {
        Ok(mtime) => mtime,
        Err(e) => {
            eprintln!("error reading mtime from '{}': {}",
                      path.display(), e);
            return failure();
        }
    };

    let n = match fs.newfile(dest.as_bytes(), path) {
        Ok(n) => n,
        Err(e) => {
            eprintln!("error creating file '{}': {}",
                      dest, e);
            return failure();
        }
    };
    n.touch(mtime);
    set_attributes(n, m)
}

pub fn parsefile<P: AsRef<Path>>(path: P) -> io::Result<AnnoJSON> {
    let data = std::fs::read_to_string(path)?;
    match super::yaml::load_from_str(&data) {
        Ok(r) => Ok(r),
        Err(e) => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("{}", e),
        ))
    }
}

pub fn add(fs: &mut VirtFS, doc: &AnnoJSON) -> ProgResult<()> {
    let doc = doc.expect_object()?;

    for v in doc.vec("plan")? {
        let m = v.expect_object()?;

        let action = m.str("do")?;
        match action {
            "mkdir"   => mkdir(fs, &m)?,
            "symlink" => symlink(fs, &m)?,
            "mknod"   => mknod(fs, &m)?,
            "add"     => files(fs, &m)?,
            _ => {
                eprintln!("Unknown action '{}'", action);
                return failure();
            }
        }
    }

    Ok(())
}
