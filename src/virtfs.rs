use std::{fmt, str, io};
use std::collections::BTreeMap;

fn tostr(buf: &[u8]) -> &str {
    str::from_utf8(buf).unwrap_or("<non-utf8>")
}

fn not_found<T>() -> io::Result<T> {
    Err(io::Error::new(
            io::ErrorKind::NotFound,
            "path not found"
    ))
}

fn file_exists<T>() -> io::Result<T> {
    Err(io::Error::new(
        io::ErrorKind::AlreadyExists,
        "file exists",
    ))
}

fn basename(path: &[u8]) -> io::Result<(&[u8], &[u8])> {
    let mut end = path.len();
    while end > 0 && path[end - 1] == b'/' {
        end -= 1;
    }
    let mut start = end;
    while start > 0 && path[start - 1] != b'/' {
        start -= 1;
    }

    Ok((&path[..start], &path[start..end]))
}

struct Dir {
    nlink: u32,
    uid: u32,
    gid: u32,
    mode: u16,
    parent: usize,
    entries: BTreeMap<Box<[u8]>,usize>,
}

struct File {
    nlink: u32,
    uid: u32,
    gid: u32,
    mode: u16,
    data: Box<io::Read>,
}

struct Symlink {
    nlink: u32,
    uid: u32,
    gid: u32,
    mode: u16,
    tgt: Box<[u8]>,
}

enum Node {
    Dir(Dir),
    File(File),
    Symlink(Symlink),
    Unused(usize),
}

impl Node {
    /*
    fn is_dir(&self) -> bool {
        if let Node::Dir(_) = *self {
            true
        } else {
            false
        }
    }
    */
    fn nlink(&self) -> u32 {
        match *self {
            Node::Dir(ref d)     => d.nlink,
            Node::File(ref f)    => f.nlink,
            Node::Symlink(ref l) => l.nlink,
            Node::Unused(_)      => 1,
        }
    }
}

pub struct FS {
    nodes: Vec<Node>,
    free: usize,
}

impl FS {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn lookup(&self, path: &[u8]) -> io::Result<usize> {
        let mut idx = 0;
        let mut end = 0;
        while end < path.len() {
            let dir = match self.nodes[idx] {
                Node::Dir(ref d) => d,
                _ => return not_found(),
            };
            let mut start = end;
            while start < path.len() && path[start] == b'/' {
                start += 1;
            }
            end = start;
            while end < path.len() && path[end] != b'/' {
                end += 1;
            }
            let part = &path[start..end];
            if part.is_empty() || part == b"." {
                continue;
            }
            if part == b".." {
                idx = dir.parent;
                continue;
            }
            if let Some(child) = dir.entries.get(part) {
                idx = *child;
            } else {
                return not_found();
            }
        }
        Ok(idx)
    }

    fn freeindex(&mut self) -> usize {
        if self.free == 0 {
            self.free = self.nodes.len();
            self.nodes.push(Node::Unused(0));
        }
        let idx = self.free;
        if let Node::Unused(next) = self.nodes[idx] {
            self.free = next;
        } else {
            panic!("free points to used entry")
        }
        idx
    }

    fn link(&mut self, path: &[u8], idx: usize) -> io::Result<usize> {
        let (prefix, name) = basename(path)?;
        let parent = self.lookup(prefix)?;
        if let Node::Dir(ref mut dir) = self.nodes[parent] {
            if dir.entries.contains_key(name) {
                return file_exists();
            }
            dir.entries.insert(name.into(), idx);
        } else {
            return not_found();
        }
        Ok(parent)
    }

    pub fn unlink(&mut self, path: &[u8]) -> io::Result<()> {
        let (prefix, name) = basename(path)?;
        let parent = self.lookup(prefix)?;
        let idx = if let Node::Dir(ref mut p) = self.nodes[parent] {
            if let Some(idx) = p.entries.remove(name) {
                idx
            } else {
                return not_found();
            }
        } else {
            return not_found();
        };
        if let Node::Dir(_) = self.nodes[idx] {
            if let Node::Dir(ref mut p) = self.nodes[parent] {
                p.nlink -= 1;
            }
        }
        let mut stack = vec![idx];
        while let Some(idx) = stack.pop() {
            match self.nodes[idx] {
                Node::Dir(ref d) => {
                    for v in d.entries.values() {
                        stack.push(*v);
                    }
                }
                Node::File(ref mut f) => {
                    f.nlink -= 1;
                    if f.nlink > 0 {
                        continue;
                    }
                }
                Node::Symlink(ref mut l) => {
                    l.nlink -= 1;
                    if l.nlink > 0 {
                        continue;
                    }
                }
                Node::Unused(_) => panic!(),
            }
            self.nodes[idx] = Node::Unused(self.free);
            self.free = idx;
        }
        debug_assert!(self.validate());
        Ok(())
    }

    pub fn mkdir(&mut self, path: &[u8]) -> io::Result<usize> {
        let idx = self.freeindex();
        let parent = self.link(path, idx)?;
        self.nodes[idx] = Node::Dir(Dir {
            nlink: 2,
            uid: 0,
            gid: 0,
            mode: 0o755,
            parent,
            entries: BTreeMap::new(),
        });
        if let Node::Dir(ref mut p) = self.nodes[parent] {
            p.nlink += 1;
        }
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn newfile(&mut self, path: &[u8], data: Box<io::Read>) -> io::Result<usize> {
        let idx = self.freeindex();
        let _parent = self.link(path, idx)?;
        self.nodes[idx] = Node::File(File {
            nlink: 1,
            uid: 0,
            gid: 0,
            mode: 0o644,
            data,
        });
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn symlink(&mut self, tgt: &[u8], path: &[u8]) -> io::Result<usize> {
        let idx = self.freeindex();
        let _parent = self.link(path, idx)?;
        self.nodes[idx] = Node::Symlink(Symlink {
            nlink: 1,
            uid: 0,
            gid: 0,
            mode: 0o777,
            tgt: tgt.into(),
        });
        debug_assert!(self.validate());
        Ok(idx)
    }

    pub fn hardlink(&mut self, idx: usize, path: &[u8]) -> io::Result<()> {
        if idx >= self.nodes.len() {
            return not_found();
        }
        if let Node::Dir(_) = self.nodes[idx] {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "cannot hardlink directories"
            ));
        }
        let _parent = self.link(path, idx)?;
        match self.nodes[idx] {
            Node::File(ref mut f)    => f.nlink += 1,
            Node::Symlink(ref mut l) => l.nlink += 1,
            _ => panic!(),
        }
        debug_assert!(self.validate());
        Ok(())
    }

    pub fn chown(&mut self, idx: usize, uid: u32, gid: u32) -> io::Result<()> {
        if idx >= self.nodes.len() {
            return not_found();
        }
        match self.nodes[idx] {
            Node::Dir(ref mut d)     => { d.uid = uid; d.gid = gid; }
            Node::File(ref mut f)    => { f.uid = uid; f.gid = gid; }
            Node::Symlink(ref mut l) => { l.uid = uid; l.gid = gid; }
            Node::Unused(_) => { return not_found(); }
        }
        Ok(())
    }

    pub fn setuid(&mut self, idx: usize, uid: u32) -> io::Result<()> {
        if idx >= self.nodes.len() {
            return not_found();
        }
        match self.nodes[idx] {
            Node::Dir(ref mut d)     => { d.uid = uid }
            Node::File(ref mut f)    => { f.uid = uid }
            Node::Symlink(ref mut l) => { l.uid = uid }
            Node::Unused(_) => { return not_found(); }
        }
        Ok(())
    }

    pub fn setgid(&mut self, idx: usize, gid: u32) -> io::Result<()> {
        if idx >= self.nodes.len() {
            return not_found();
        }
        match self.nodes[idx] {
            Node::Dir(ref mut d)     => { d.gid = gid }
            Node::File(ref mut f)    => { f.gid = gid }
            Node::Symlink(ref mut l) => { l.gid = gid }
            Node::Unused(_) => { return not_found(); }
        }
        Ok(())
    }

    pub fn chmod(&mut self, idx: usize, mode: u16) -> io::Result<()> {
        if idx >= self.nodes.len() {
            return not_found();
        }
        match self.nodes[idx] {
            Node::Dir(ref mut d)     => { d.mode = mode; }
            Node::File(ref mut f)    => { f.mode = mode; }
            Node::Symlink(ref mut l) => { l.mode = mode; }
            Node::Unused(_) => { return not_found(); }
        }
        Ok(())
    }

    pub fn validate(&self) -> bool {
        let len = self.nodes.len();
        let mut nlinks: Vec<u32> = vec![0; len];
        { /* walk free list */
            let mut idx = self.free;
            while idx > 0 {
                if idx >= len {
                    return false;
                }
                if let Node::Unused(next) = self.nodes[idx] {
                    nlinks[idx] += 1;
                    idx = next;
                } else {
                    return false;
                }
            }
        }
        { /* count node links and check parent references */
            let mut stack = vec![0];
            while let Some(idx) = stack.pop() {
                if let Node::Dir(ref d) = self.nodes[idx] {
                    if nlinks[idx] != 0 {
                        return false;
                    }
                    nlinks[idx] = 2;
                    for v in d.entries.values() {
                        let child = *v;
                        if child >= len {
                            return false;
                        }
                        match self.nodes[child] {
                            Node::Dir(ref c) => {
                                if c.parent != idx {
                                    return false;
                                }
                                nlinks[idx] += 1;
                                stack.push(child);
                            }
                            Node::Unused(_) => {
                                return false;
                            }
                            _ => {
                                nlinks[child] += 1;
                            }
                        }
                    }
                } else {
                    return false;
                }
            }
        }
        /* check link counts match */
        for (i, nlink) in nlinks.into_iter().enumerate() {
            if nlink != self.nodes[i].nlink() {
                return false;
            }
        }
        true
    }
}

impl Default for FS {
    fn default() -> Self {
        Self {
            nodes: vec![Node::Dir(Dir {
                nlink: 2,
                uid: 0,
                gid: 0,
                mode: 0o755,
                parent: 0,
                entries: BTreeMap::new(),
            })],
            free: 0,
        }
    }
}

impl fmt::Display for FS {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut entries = Vec::new();
        let mut stack: Vec<Option<(&[u8], usize)>> = Vec::new();
        stack.push(Some((b"", 0)));
        let mut path = Vec::new();

        while let Some(e) = stack.pop() {
            if let Some((name, idx)) = e {
                path.extend_from_slice(name);
                match self.nodes[idx] {
                    Node::Dir(ref d) => {
                        writeln!(f, "d {:3} {:2} {:4} {:4} {:4o} {}", idx,
                                 d.nlink, d.uid, d.gid, d.mode, tostr(&path))?;
                        for (name, idx) in &d.entries {
                            entries.push((name.as_ref(), *idx));
                        }
                        stack.push(None);
                        while let Some(e) = entries.pop() {
                            stack.push(Some(e));
                        }
                        path.push(b'/');
                        continue;
                    }
                    Node::File(ref r) => {
                        writeln!(f, "d {:3} {:2} {:4} {:4} {:4o} {}", idx,
                                 r.nlink, r.uid, r.gid, r.mode, tostr(&path))?;
                    }
                    Node::Symlink(ref s) => {
                        writeln!(f, "l {:3} {:2} {:4} {:4} {:4o} {} -> {}", idx,
                                 s.nlink, s.uid, s.gid, s.mode, tostr(&path), tostr(&s.tgt))?;
                    }
                    Node::Unused(_) => panic!("we found inode marked unused")
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
}
