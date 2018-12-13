use std::io;
use std::path::Path;

use super::yaml::Yaml;

use virtfs::VirtFS;

fn set_attributes<T>(node: &mut virtfs::Node<T>, entry: &Yaml) -> io::Result<()> {
    match entry["owner"] {
        Yaml::Integer(v) => {
            if v < 0 || v > i64::from(<u32>::max_value()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "uid out of range"
                ));
            }
            node.chown(v as u32);
        }
        Yaml::BadValue => {}
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "owner should be an integer"
            ));
        }
    };
    match entry["group"] {
        Yaml::Integer(v) => {
            if v < 0 || v > i64::from(<u32>::max_value()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "gid out of range"
                ));
            }
            node.chgrp(v as u32);
        }
        Yaml::BadValue => {}
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "owner should be an integer"
            ));
        }
    };
    match entry["mode"] {
        Yaml::Integer(v) => {
            if v < 0 || v > i64::from(<u16>::max_value()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "uid out of range"
                ));
            }
            node.chmod(v as u16);
        }
        Yaml::BadValue => {}
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
    let dest = match entry["dest"] {
        Yaml::String(ref d) => d,
        Yaml::BadValue => {
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
    let dest = match entry["dest"] {
        Yaml::String(ref d) => d,
        Yaml::BadValue => {
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
    let tgt = match entry["target"] {
        Yaml::String(ref d) => d,
        Yaml::BadValue => {
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
        Yaml::String(ref d) => d,
        Yaml::BadValue => {
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
        Yaml::Integer(n) if n < 256 => n,
        Yaml::BadValue => 0,
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
    let plan = match doc["plan"] {
        Yaml::Array(ref a) => a,
        Yaml::BadValue => {
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
        let action = match v["do"] {
            Yaml::String(ref p) => p,
            Yaml::BadValue => {
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
