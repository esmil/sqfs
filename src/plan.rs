use std::io;
use std::io::Read;
use std::path::Path;
use std::fs::File;

use super::yaml::{YamlLoader,Yaml};

use squashfs::virtfs::FS;

fn mkdir(fs: &mut FS, entry: &Yaml) -> io::Result<()> {
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
    match entry["owner"] {
        Yaml::Integer(v) => {
            if v < 0 || v > i64::from(<u32>::max_value()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "uid out of range"
                ));
            }
            fs.setuid(&n, v as u32)?;
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
            fs.setgid(&n, v as u32)?;
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
            fs.chmod(&n, v as u16)?;
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

fn symlink(fs: &mut FS, entry: &Yaml) -> io::Result<()> {
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
    fs.symlink(tgt.as_bytes(), dest.as_bytes())?;
    Ok(())
}

pub fn parsefile<P: AsRef<Path>>(path: P) -> io::Result<Vec<Yaml>> {
    let mut data = String::new();
    File::open(path)?.read_to_string(&mut data)?;

    match YamlLoader::load_from_str(&data) {
        Ok(r) => Ok(r),
        Err(e) => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("{}", e),
        ))
    }
}

pub fn add(fs: &mut FS, doc: &Yaml) -> io::Result<()> {
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
        let action = match v["action"] {
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
