use std::io;
use std::path::Path;

use super::yaml::{Yaml, YVal};

use virtfs::VirtFS;

fn set_attributes<T>(node: &mut virtfs::Node<T>, entry: &Yaml) -> io::Result<()> {
    match entry["owner"].val {
        YVal::Integer(v) => {
            if v < 0 || v > i64::from(<u32>::max_value()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "uid out of range"
                ));
            }
            node.chown(v as u32);
        }
        YVal::BadValue => {}
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "owner should be an integer"
            ));
        }
    };
    match entry["group"].val {
        YVal::Integer(v) => {
            if v < 0 || v > i64::from(<u32>::max_value()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "gid out of range"
                ));
            }
            node.chgrp(v as u32);
        }
        YVal::BadValue => {}
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "owner should be an integer"
            ));
        }
    };
    match entry["mode"].val {
        YVal::Integer(v) => {
            if v < 0 || v > i64::from(<u16>::max_value()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "uid out of range"
                ));
            }
            node.chmod(v as u16);
        }
        YVal::BadValue => {}
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "mode should be an integer"
            ));
        }
    };
    Ok(())
}

fn mkdir<T>(fs: &mut VirtFS<T>, entry: &Yaml) -> io::Result<()> {
    let dest = match entry["dest"].val {
        YVal::String(ref d) => d,
        YVal::BadValue => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "no dest found"
            ));
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "dest should be a string"
            ));
        }
    };
    let n = fs.mkdir(dest.as_bytes())?;
    set_attributes(n, entry)?;
    Ok(())
}

fn symlink<T>(fs: &mut VirtFS<T>, entry: &Yaml) -> io::Result<()> {
    let dest = match entry["dest"].val {
        YVal::String(ref d) => d,
        YVal::BadValue => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "no dest found"
            ));
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "dest should be a string"
            ));
        }
    };
    let tgt = match entry["target"].val {
        YVal::String(ref d) => d,
        YVal::BadValue => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "no target found"
            ));
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "target should be a string"
            ));
        }
    };
    let n = fs.symlink(dest.as_bytes(), tgt.as_bytes())?;
    set_attributes(n, entry)?;
    Ok(())
}

/*
fn mknod<T>(fs: &mut VirtFS<T>, entry: &Yaml) -> io::Result<()> {
    let dest = match entry["dest"] {
        YVal::String(ref d) => d,
        YVal::BadValue => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "no dest found"
            ));
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "dest should be a string"
            ));
        }
    };
    let major = match entry["major"] {
        YVal::Integer(n) if n < 256 => n,
        YVal::BadValue => 0,
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "major should be an integer < 256"
            ));
        }
    };
    let n = fs.symlink(dest.as_bytes(), tgt.as_bytes())?;
    set_attributes(n, entry)?;
    Ok(())
}
*/

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

pub fn add<T>(fs: &mut VirtFS<T>, doc: &Yaml) -> io::Result<()> {
    let plan = match doc["plan"].val {
        YVal::Array(ref a) => a,
        YVal::BadValue => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "no plan found"
            ));
        }
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "plan is not an array"
            ));
        }
    };

    for (i, v) in plan.iter().enumerate() {
        let action = match v["do"].val {
            YVal::String(ref p) => p,
            YVal::BadValue => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("no type specified for plan entry {}", i)
                ));
            }
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "type should be a string"
                ));
            }
        };
        match action.as_str() {
            "mkdir"   => mkdir(fs, v)?,
            "symlink" => symlink(fs, v)?,
            _ => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("unknown action '{}' for plan entry {}", action, i)
                ));
            }
        }
    }

    Ok(())
}
