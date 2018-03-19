use std::{fmt, str, io};
use std::rc::Rc;
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
    entries: BTreeMap<Rc<[u8]>,usize>,
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
    tgt: Rc<[u8]>,
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

pub struct INode {
    idx: usize,
}

pub struct FS {
    nodes: Vec<Node>,
    free: usize,
}

impl FS {
    pub fn new() -> Self {
        Default::default()
    }

    fn _lookup(&self, path: &[u8]) -> io::Result<usize> {
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

    pub fn lookup(&self, path: &[u8]) -> io::Result<INode> {
        let idx = self._lookup(path)?;
        Ok(INode { idx })
    }

    fn link(&mut self, path: &[u8], node: Node) -> io::Result<INode> {
        let (prefix, name) = basename(path)?;
        let parent = self._lookup(prefix)?;
        if let Node::Dir(ref mut dir) = self.nodes[parent] {
            if dir.entries.contains_key(name) {
                return file_exists();
            }
        } else {
            return not_found();
        }

        if self.free == 0 {
            self.free = self.nodes.len();
            self.nodes.push(Node::Unused(0));
        }
        let idx = self.free;
        if let Node::Unused(n) = self.nodes[idx] {
            self.free = n;
        } else {
            panic!("free points to used entry")
        }
        self.nodes[idx] = node;

        if let Node::Dir(ref mut dir) = self.nodes[parent] {
            dir.entries.insert(name.into(), idx);
        }
        debug_assert!(self.validate());
        Ok(INode { idx })
    }

    pub fn unlink(&mut self, path: &[u8]) -> io::Result<()> {
        let (prefix, name) = basename(path)?;
        let parent = self._lookup(prefix)?;
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

    pub fn mkdir(&mut self, path: &[u8]) -> io::Result<INode> {
        let (prefix, name) = basename(path)?;
        let parent = self._lookup(prefix)?;
        if let Node::Dir(ref mut dir) = self.nodes[parent] {
            if dir.entries.contains_key(name) {
                return file_exists();
            }
        } else {
            return not_found();
        }

        if self.free == 0 {
            self.free = self.nodes.len();
            self.nodes.push(Node::Unused(0));
        }
        let idx = self.free;
        if let Node::Unused(n) = self.nodes[idx] {
            self.free = n;
        } else {
            panic!("free points to used entry")
        }
        self.nodes[idx] = Node::Dir(Dir {
            nlink: 2,
            uid: 0,
            gid: 0,
            mode: 0o755,
            parent,
            entries: BTreeMap::new(),
        });

        if let Node::Dir(ref mut p) = self.nodes[parent] {
            p.entries.insert(name.into(), idx);
            p.nlink += 1;
        }
        debug_assert!(self.validate());
        Ok(INode { idx })
    }

    pub fn newfile(&mut self, path: &[u8], data: Box<io::Read>) -> io::Result<INode> {
        self.link(path, Node::File(File {
            nlink: 1,
            uid: 0,
            gid: 0,
            mode: 0o644,
            data,
        }))
    }

    pub fn symlink(&mut self, tgt: &[u8], path: &[u8]) -> io::Result<INode> {
        self.link(path, Node::Symlink(Symlink {
            nlink: 1,
            uid: 0,
            gid: 0,
            mode: 0o777,
            tgt: tgt.into(),
        }))
    }

    pub fn hardlink(&mut self, n: &INode, path: &[u8]) -> io::Result<()> {
        if let Node::Dir(_) = self.nodes[n.idx] {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "cannot hardlink directories"
            ));
        }
        let (prefix, name) = basename(path)?;
        let parent = self._lookup(prefix)?;
        if let Node::Dir(ref mut dir) = self.nodes[parent] {
            if dir.entries.contains_key(name) {
                return file_exists();
            }
            dir.entries.insert(name.into(), n.idx);
        } else {
            return not_found();
        }
        match self.nodes[n.idx] {
            Node::File(ref mut f)    => f.nlink += 1,
            Node::Symlink(ref mut l) => l.nlink += 1,
            _ => panic!(),
        }
        debug_assert!(self.validate());
        Ok(())
    }

    pub fn chown(&mut self, n: &INode, uid: u32, gid: u32) -> io::Result<()> {
        if n.idx >= self.nodes.len() {
            return not_found();
        }
        match self.nodes[n.idx] {
            Node::Dir(ref mut d)     => { d.uid = uid; d.gid = gid; }
            Node::File(ref mut f)    => { f.uid = uid; f.gid = gid; }
            Node::Symlink(ref mut l) => { l.uid = uid; l.gid = gid; }
            Node::Unused(_) => { return not_found(); }
        }
        Ok(())
    }

    pub fn setuid(&mut self, n: &INode, uid: u32) -> io::Result<()> {
        if n.idx >= self.nodes.len() {
            return not_found();
        }
        match self.nodes[n.idx] {
            Node::Dir(ref mut d)     => { d.uid = uid }
            Node::File(ref mut f)    => { f.uid = uid }
            Node::Symlink(ref mut l) => { l.uid = uid }
            Node::Unused(_) => { return not_found(); }
        }
        Ok(())
    }

    pub fn setgid(&mut self, n: &INode, gid: u32) -> io::Result<()> {
        if n.idx >= self.nodes.len() {
            return not_found();
        }
        match self.nodes[n.idx] {
            Node::Dir(ref mut d)     => { d.gid = gid }
            Node::File(ref mut f)    => { f.gid = gid }
            Node::Symlink(ref mut l) => { l.gid = gid }
            Node::Unused(_) => { return not_found(); }
        }
        Ok(())
    }

    pub fn chmod(&mut self, n: &INode, mode: u16) -> io::Result<()> {
        if n.idx >= self.nodes.len() {
            return not_found();
        }
        match self.nodes[n.idx] {
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
                if let Node::Unused(n) = self.nodes[idx] {
                    nlinks[idx] += 1;
                    idx = n;
                } else {
                    return false;
                }
            }
        }
        { /* count node links */
            let mut stack = vec![0];
            while let Some(idx) = stack.pop() {
                if idx >= len {
                    return false;
                }
                match self.nodes[idx] {
                    Node::Dir(ref d) => {
                        for v in d.entries.values() {
                            stack.push(*v)
                        }
                        nlinks[idx] += 2;
                        if idx != 0 {
                            if let Node::Dir(_) = self.nodes[d.parent] {
                                nlinks[d.parent] += 1;
                            } else {
                                return false;
                            }
                        }
                    }
                    Node::Unused(_) => {
                        return false;
                    }
                    _ => {
                        nlinks[idx] += 1;
                    }
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
        let mut stack: Vec<Option<(Rc<[u8]>,usize)>> = Vec::new();
        stack.push(Some(((b"" as &[u8]).into(), 0)));
        let mut path = Vec::new();

        while let Some(e) = stack.pop() {
            if let Some((name, n)) = e {
                path.extend_from_slice(&name);
                match self.nodes[n] {
                    Node::Dir(ref d) => {
                        writeln!(f, "d {:3} {:2} {:4} {:4} {:4o} {}", n,
                                 d.nlink, d.uid, d.gid, d.mode, tostr(&path))?;
                        for (name, n) in &d.entries {
                            entries.push((name.clone(), *n));
                        }
                        stack.push(None);
                        while let Some(e) = entries.pop() {
                            stack.push(Some(e));
                        }
                        path.push(b'/');
                        continue;
                    }
                    Node::File(ref r) => {
                        writeln!(f, "d {:3} {:2} {:4} {:4} {:4o} {}", n,
                                 r.nlink, r.uid, r.gid, r.mode, tostr(&path))?;
                    }
                    Node::Symlink(ref s) => {
                        writeln!(f, "l {:3} {:2} {:4} {:4} {:4o} {} -> {}", n,
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

