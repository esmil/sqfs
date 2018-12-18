use std::io;
use std::path::Path;

use super::{PResult, failure};
use super::yaml::{Yaml, YVal};

use virtfs::VirtFS;

fn expect_array<'a>(doc: &'a Yaml, idx: &str) -> PResult<&'a Vec<Yaml>> {
    if let YVal::Hash(ref h) = doc.val {
        let k = Yaml { line: 0, col: 0, val: YVal::String(String::from(idx)) };
        match h.get(&k) {
            Some(&Yaml { val: YVal::Array(ref a), .. }) => Ok(a),
            Some(&Yaml { line, col, .. }) => {
                eprintln!("Expected array at line {} column {}",
                          line, col);
                failure()
            }
            None => {
                eprintln!("Expected key '{}' in hash at line {} column {}",
                          idx, doc.line, doc.col);
                failure()
            }
        }
    } else {
        eprintln!("Expected hash at line {} column {}",
                  doc.line, doc.col);
        failure()
    }
}

fn expect_string<'a>(doc: &'a Yaml, idx: &str) -> PResult<&'a str> {
    if let YVal::Hash(ref h) = doc.val {
        let k = Yaml { line: 0, col: 0, val: YVal::String(String::from(idx)) };
        match h.get(&k) {
            Some(&Yaml { val: YVal::String(ref s), .. }) => Ok(s),
            Some(&Yaml { line, col, .. }) => {
                eprintln!("Expected string at line {} column {}",
                          line, col);
                failure()
            }
            None => {
                eprintln!("Expected key '{}' in hash at line {} column {}",
                          idx, doc.line, doc.col);
                failure()
            }
        }
    } else {
        eprintln!("Expected hash at line {} column {}",
                  doc.line, doc.col);
        failure()
    }
}

fn expect_integer(doc: &Yaml, idx: &str) -> PResult<(i64, usize, usize)> {
    if let YVal::Hash(ref h) = doc.val {
        let k = Yaml { line: 0, col: 0, val: YVal::String(String::from(idx)) };
        match h.get(&k) {
            Some(&Yaml { line, col, val: YVal::Integer(v) }) => Ok((v, line, col)),
            Some(&Yaml { line, col, .. }) => {
                eprintln!("Expected integer at line {} column {}",
                          line, col);
                failure()
            }
            None => {
                eprintln!("Expected key '{}' in hash at line {} column {}",
                          idx, doc.line, doc.col);
                failure()
            }
        }
    } else {
        eprintln!("Expected hash at line {} column {}",
                  doc.line, doc.col);
        failure()
    }
}

fn optional_integer(doc: &Yaml, idx: &str) -> PResult<Option<(i64, usize, usize)>> {
    if let YVal::Hash(ref h) = doc.val {
        let k = Yaml { line: 0, col: 0, val: YVal::String(String::from(idx)) };
        match h.get(&k) {
            Some(&Yaml { line, col, val: YVal::Integer(v) }) => Ok(Some((v, line, col))),
            Some(&Yaml { line, col, .. }) => {
                eprintln!("Expected integer at line {} column {}",
                          line, col);
                failure()
            }
            None => Ok(None),
        }
    } else {
        eprintln!("Expected hash at line {} column {}",
                  doc.line, doc.col);
        failure()
    }
}

fn set_attributes<T>(node: &mut virtfs::Node<T>, entry: &Yaml) -> PResult<()> {
    if let Some((v, line, col)) = optional_integer(entry, "owner")? {
        if v < 0 || v > i64::from(<u32>::max_value()) {
            eprintln!("uid out of range at line {} column {}",
                      line, col);
            return failure();
        }
        node.chown(v as u32);
    }

    if let Some((v, line, col)) = optional_integer(entry, "group")? {
        if v < 0 || v > i64::from(<u32>::max_value()) {
            eprintln!("gid out of range at line {} column {}",
                      line, col);
            return failure();
        }
        node.chgrp(v as u32);
    }

    if let Some((v, line, col)) = optional_integer(entry, "mode")? {
        if v < 0 || v > i64::from(<u16>::max_value()) {
            eprintln!("invalid mode at line {} column {}",
                      line, col);
            return failure();
        }
        node.chmod(v as u16);
    }

    Ok(())
}

fn mkdir<T>(fs: &mut VirtFS<T>, entry: &Yaml) -> PResult<()> {
    let dest = expect_string(entry, "dest")?;
    let n = match fs.mkdir(dest.as_bytes()) {
        Ok(n) => n,
        Err(e) => {
            eprintln!("Error creating directory '{}': {}",
                      dest, e);
            return failure();
        }
    };
    set_attributes(n, entry)?;
    Ok(())
}

fn symlink<T>(fs: &mut VirtFS<T>, entry: &Yaml) -> PResult<()> {
    let dest = expect_string(entry, "dest")?;
    let tgt = expect_string(entry, "target")?;
    let n = match fs.symlink(dest.as_bytes(), tgt.as_bytes()) {
        Ok(n) => n,
        Err(e) => {
            eprintln!("Error creating symlink '{}' -> '{}': {}",
                      dest, tgt, e);
            return failure();
        }
    };
    set_attributes(n, entry)?;
    Ok(())
}

enum NodeType {
    Block,
    Char,
}

fn mknod<T>(fs: &mut VirtFS<T>, entry: &Yaml) -> PResult<()> {
    let dest = expect_string(entry, "dest")?;
    let typ = match expect_string(entry, "type")? {
        "b" | "block" | "blockdev" => NodeType::Block,
        "c" | "char" | "chardev"   => NodeType::Char,
        _ => {
            eprintln!("");
            return failure();
        }
    };
    let (v, line, col) = expect_integer(entry, "major")?;
    let major = if v < 0 || v >= 256 {
        eprintln!("major out of range at line {} column {}",
                  line, col);
        return failure();
    } else {
        v as u32
    };
    let (v, line, col) = expect_integer(entry, "minor")?;
    let minor = if v < 0 || v >= 256 {
        eprintln!("minor out of range at line {} column {}",
                  line, col);
        return failure();
    } else {
        v as u32
    };
    let n = match typ {
        NodeType::Block => {
            match fs.blockdev(dest.as_bytes(), major << 8 | minor) {
                Ok(n) => n,
                Err(e) => {
                    eprintln!("Error creating block device '{}': {}",
                              dest, e);
                    return failure();
                }
            }
        }
        NodeType::Char => {
            match fs.chardev(dest.as_bytes(), major << 8 | minor) {
                Ok(n) => n,
                Err(e) => {
                    eprintln!("Error creating character device '{}': {}",
                              dest, e);
                    return failure();
                }
            }
        }
    };
    set_attributes(n, entry)?;
    Ok(())
}

pub fn parsefile<P: AsRef<Path>>(path: P) -> io::Result<Vec<Yaml>> {
    let data = std::fs::read_to_string(path)?;
    match super::yaml::load_from_str(&data) {
        Ok(r) => Ok(r),
        Err(e) => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("{}", e),
        ))
    }
}

pub fn add<T>(fs: &mut VirtFS<T>, doc: &Yaml) -> PResult<()> {
    let plan = expect_array(doc, "plan")?;

    for v in plan.iter() {
        //if let YVal::Hash(_) = v.val {

        let action = expect_string(v, "do")?;
        match action {
            "mkdir"   => mkdir(fs, v)?,
            "symlink" => symlink(fs, v)?,
            "mknod"   => mknod(fs, v)?,
            _ => {
                eprintln!("Unknown action '{}'", action);
                return failure();
            }
        }
    }

    Ok(())
}
