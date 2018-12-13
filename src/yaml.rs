use yaml_rust::parser::{Parser, Event, MarkedEventReceiver};
use yaml_rust::scanner::{Marker, ScanError, TScalarStyle, TokenType};
use std::collections::BTreeMap;
use std::f64;
use std::i64;
use std::ops::Index;

pub type Array = Vec<Yaml>;
//pub type Hash = LinkedHashMap<Yaml, Yaml>;
pub type Hash = BTreeMap<Yaml, Yaml>;

#[derive(Clone, PartialEq, PartialOrd, Debug, Eq, Ord, Hash)]
pub enum Yaml {
    /// Float types are stored as String and parsed on demand.
    /// Note that f64 does NOT implement Eq trait and can NOT be stored in BTreeMap.
    Real(String),
    /// YAML int is stored as i64.
    Integer(i64),
    /// YAML scalar.
    String(String),
    /// YAML bool, e.g. `true` or `false`.
    Boolean(bool),
    /// YAML array, can be accessed as a `Vec`.
    Array(self::Array),
    /// YAML hash, can be accessed as a `LinkedHashMap`.
    ///
    /// Itertion order will match the order of insertion into the map.
    Hash(self::Hash),
    /// YAML null, e.g. `null` or `~`.
    Null,
    /// Accessing a nonexistent node via the Index trait returns `BadValue`. This
    /// simplifies error handling in the calling code. Invalid type conversion also
    /// returns `BadValue`.
    BadValue,
}

// parse f64 as Core schema
// See: https://github.com/chyh1990/yaml-rust/issues/51
fn parse_f64(v: &str) -> Option<f64> {
    match v {
        ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" => Some(f64::INFINITY),
        "-.inf" | "-.Inf" | "-.INF" => Some(f64::NEG_INFINITY),
        ".nan" | "NaN" | ".NAN" => Some(f64::NAN),
        _ => v.parse::<f64>().ok(),
    }
}

macro_rules! define_as (
    ($name:ident, $t:ident, $yt:ident) => (
        pub fn $name(&self) -> Option<$t> {
            match *self {
                Yaml::$yt(v) => Some(v),
                _ => None
            }
        }
    );
);

macro_rules! define_as_ref (
    ($name:ident, $t:ty, $yt:ident) => (
        pub fn $name(&self) -> Option<$t> {
            match *self {
                Yaml::$yt(ref v) => Some(v),
                _ => None
            }
        }
    );
);

macro_rules! define_into (
    ($name:ident, $t:ty, $yt:ident) => (
        pub fn $name(self) -> Option<$t> {
            match self {
                Yaml::$yt(v) => Some(v),
                _ => None
            }
        }
    );
);

impl Yaml {
    define_as!(as_bool, bool, Boolean);
    define_as!(as_i64, i64, Integer);

    define_as_ref!(as_str, &str, String);
    define_as_ref!(as_hash, &Hash, Hash);
    define_as_ref!(as_vec, &Array, Array);

    define_into!(into_bool, bool, Boolean);
    define_into!(into_i64, i64, Integer);
    define_into!(into_string, String, String);
    define_into!(into_hash, Hash, Hash);
    define_into!(into_vec, Array, Array);

    pub fn is_null(&self) -> bool {
        match *self {
            Yaml::Null => true,
            _ => false,
        }
    }

    pub fn is_badvalue(&self) -> bool {
        match *self {
            Yaml::BadValue => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match *self {
            Yaml::Array(_) => true,
            _ => false,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match *self {
            Yaml::Real(ref v) => parse_f64(v),
            _ => None,
        }
    }

    pub fn into_f64(self) -> Option<f64> {
        match self {
            Yaml::Real(ref v) => parse_f64(v),
            _ => None,
        }
    }

    // Not implementing FromStr because there is no possibility of Error.
    // This function falls back to Yaml::String if nothing else matches.
    fn from_str(v: &str) -> Yaml {
        match v {
            "~" | "null" => return Yaml::Null,
            "true"       => return Yaml::Boolean(true),
            "false"      => return Yaml::Boolean(false),
            _            => {}
        };
        if v.starts_with("0x") {
            if let Ok(n) = i64::from_str_radix(&v[2..], 16) {
                return Yaml::Integer(n);
            }
        }
        if v.starts_with("0o") {
            if let Ok(n) = i64::from_str_radix(&v[2..], 8) {
                return Yaml::Integer(n);
            }
        }
        if v.starts_with('0') {
            if let Ok(n) = i64::from_str_radix(&v[1..], 8) {
                return Yaml::Integer(n);
            }
        }
        if v.starts_with('+') {
            if let Ok(n) = v[1..].parse::<i64>() {
                return Yaml::Integer(n);
            }
        }
        if let Ok(n) = v.parse::<i64>() {
            return Yaml::Integer(n);
        }
        if parse_f64(v).is_some() {
            return Yaml::Real(v.to_owned());
        }
        Yaml::String(v.to_owned())
    }
}

static BAD_VALUE: Yaml = Yaml::BadValue;

impl<'a> Index<&'a str> for Yaml {
    type Output = Yaml;

    fn index(&self, idx: &'a str) -> &Yaml {
        match *self {
            Yaml::Hash(ref h) => {
                let key = Yaml::String(idx.to_owned());
                h.get(&key).unwrap_or(&BAD_VALUE)
            }
            _ => &BAD_VALUE,
        }
    }
}

impl Index<usize> for Yaml {
    type Output = Yaml;

    fn index(&self, idx: usize) -> &Yaml {
        match *self {
            Yaml::Array(ref v) => {
                v.get(idx).unwrap_or(&BAD_VALUE)
            }
            Yaml::Hash(ref h)  => {
                let key = Yaml::Integer(idx as i64);
                h.get(&key).unwrap_or(&BAD_VALUE)
            }
            _ => &BAD_VALUE
        }
    }
}

impl IntoIterator for Yaml {
    type Item = Yaml;
    type IntoIter = YamlIter;

    fn into_iter(self) -> Self::IntoIter {
        YamlIter {
            yaml: self.into_vec().unwrap_or_else(Vec::new).into_iter(),
        }
    }
}

pub struct YamlIter {
    yaml: std::vec::IntoIter<Yaml>,
}

impl Iterator for YamlIter {
    type Item = Yaml;

    fn next(&mut self) -> Option<Yaml> {
        self.yaml.next()
    }
}

#[derive(Debug)]
enum StackEntry {
    Array(self::Array, usize),
    Mapping(self::Hash, usize),
    Key(Yaml),
}

struct Loader {
    err: Option<ScanError>,
    stack: Vec<StackEntry>,
    anchors: BTreeMap<usize, Yaml>,
    docs: Vec<Yaml>,
}

impl MarkedEventReceiver for Loader {
    fn on_event(&mut self, ev: Event, m: Marker) {
        if self.err.is_some() {
            return;
        }
        println!("{:3}:{:2} {:?}", m.line(), m.col(), ev);
        match ev {
            Event::StreamStart => {}
            Event::StreamEnd => {}
            Event::DocumentStart => {}
            Event::DocumentEnd => {}
            Event::SequenceStart(aid) => {
                self.stack.push(StackEntry::Array(Vec::new(), aid));
            }
            Event::SequenceEnd => {
                let (node, aid) = match self.stack.pop() {
                    Some(StackEntry::Array(v, aid)) => (Yaml::Array(v), aid),
                    _ => unreachable!(),
                };
                self.insert(node, aid);
            }
            Event::MappingStart(aid) => {
                self.stack.push(StackEntry::Mapping(BTreeMap::new(), aid));
            }
            Event::MappingEnd => {
                let (node, aid) = match self.stack.pop() {
                    Some(StackEntry::Mapping(h, aid)) => (Yaml::Hash(h), aid),
                    _ => unreachable!(),
                };
                self.insert(node, aid);
            }
            Event::Scalar(v, style, aid, tag) => {
                let node = if style != TScalarStyle::Plain {
                    Yaml::String(v)
                } else if let Some(TokenType::Tag(ref handle, ref suffix)) = tag {
                    // XXX tag:yaml.org,2002:
                    if handle == "!!" {
                        match suffix.as_ref() {
                            "bool" => {
                                match v.as_str() {
                                    "true"  => Yaml::Boolean(true),
                                    "false" => Yaml::Boolean(false),
                                    _ => return self.error(m, "boolean must be 'true' or 'false'"),
                                }
                            }
                            "int" => match v.parse::<i64>() {
                                Ok(v) => Yaml::Integer(v),
                                Err(_) => return self.error(m, "invalid integer"),
                            },
                            "float" => match parse_f64(&v) {
                                Some(_) => Yaml::Real(v),
                                None => return self.error(m, "invalid float"),
                            },
                            "null" => match v.as_ref() {
                                "~" | "null" => Yaml::Null,
                                _ => return self.error(m, "null must be '~' or 'null'"),
                            },
                            _ => Yaml::String(v),
                        }
                    } else {
                        Yaml::String(v)
                    }
                } else {
                    // Datatype is not specified, or unrecognized
                    Yaml::from_str(&v)
                };
                self.insert(node, aid);
            }
            Event::Alias(id) => {
                let node = match self.anchors.get(&id) {
                    Some(v) => v.clone(),
                    None => return self.error(m, "invalid anchor"),
                };
                self.insert(node, 0);
            }
            Event::Nothing => {}
        };
    }
}

impl Loader {
    fn insert(&mut self, node: Yaml, aid: usize) {
        if aid > 0 {
            self.anchors.insert(aid, node.clone());
        }
        match self.stack.pop() {
            Some(StackEntry::Array(mut v, aid)) => {
                v.push(node);
                self.stack.push(StackEntry::Array(v, aid));
            }
            Some(StackEntry::Mapping(h, aid)) => {
                self.stack.push(StackEntry::Mapping(h, aid));
                self.stack.push(StackEntry::Key(node));
            }
            Some(StackEntry::Key(key)) => {
                let h = match self.stack.last_mut() {
                    Some(StackEntry::Mapping(ref mut h, _)) => h,
                    _ => unreachable!(),
                };
                h.insert(key, node);
            }
            None => {
                self.docs.push(node);
            }
        }
    }

    fn error(&mut self, loc: Marker, info: &'static str) {
        self.err = Some(ScanError::new(loc, info));
    }
}

pub fn load_from_str(source: &str) -> Result<Vec<Yaml>, ScanError> {
    let mut loader = Loader {
        err: None,
        stack: Vec::new(),
        anchors: BTreeMap::new(),
        docs: Vec::new(),
    };
    let mut parser = Parser::new(source.chars());
    parser.load(&mut loader, true)?;
    if let Some(err) = loader.err {
        return Err(err);
    }
    Ok(loader.docs)
}

#[cfg(test)]
mod test {
    use std::f64;
    use super::*;
    #[test]
    fn test_coerce() {
        let s = "---
a: 1
b: 2.2
c: [1, 2]
";
        let out = load_from_str(&s).unwrap();
        let doc = &out[0];
        assert_eq!(doc["a"].as_i64().unwrap(), 1i64);
        assert_eq!(doc["b"].as_f64().unwrap(), 2.2f64);
        assert_eq!(doc["c"][1].as_i64().unwrap(), 2i64);
        assert!(doc["d"][0].is_badvalue());
    }

    #[test]
    fn test_empty_doc() {
        let s: String = "".to_owned();
        load_from_str(&s).unwrap();
        let s: String = "---".to_owned();
        assert_eq!(load_from_str(&s).unwrap()[0], Yaml::Null);
    }

    #[test]
    fn test_parser() {
        let s: String = "
# comment
a0 bb: val
a1:
    b1: 4
    b2: d
a2: 4 # i'm comment
a3: [1, 2, 3]
a4:
    - - a1
      - a2
    - 2
a5: 'single_quoted'
a6: \"double_quoted\"
a7: 你好
".to_owned();
        let out = load_from_str(&s).unwrap();
        let doc = &out[0];
        assert_eq!(doc["a7"].as_str().unwrap(), "你好");
    }

    #[test]
    fn test_multi_doc() {
        let s = "
'a scalar'
---
'a scalar'
---
'a scalar'
";
        let out = load_from_str(&s).unwrap();
        assert_eq!(out.len(), 3);
    }

    #[test]
    fn test_anchor() {
        let s = "
a1: &DEFAULT
    b1: 4
    b2: d
a2: *DEFAULT
";
        let out = load_from_str(&s).unwrap();
        let doc = &out[0];
        assert_eq!(doc["a2"]["b1"].as_i64().unwrap(), 4);
    }

    #[test]
    fn test_bad_anchor() {
        let s = "
a1: &DEFAULT
    b1: 4
    b2: *DEFAULT
";
        assert!(load_from_str(&s).is_err());
    }

    #[test]
    fn test_github_27() {
        // https://github.com/chyh1990/yaml-rust/issues/27
        let s = "&a";
        let out = load_from_str(&s).unwrap();
        let doc = &out[0];
        assert_eq!(doc.as_str().unwrap(), "");
    }

    #[test]
    fn test_plain_datatype() {
        let s = "
- 'string'
- \"string\"
- string
- 123
- -321
- 1.23
- -1e4
- ~
- null
- true
- false
- !!str 0
- !!int 100
- !!float 2
- !!null ~
- !!bool true
- !!bool false
- 0xFF
# bad values
- null #- !!int string
- null #- !!float string
- null #- !!bool null
- null #- !!null val
- 0o77
- [ 0xF, 0xF ]
- +12345
- [ true, false ]
";
        let out = load_from_str(&s).unwrap();
        let doc = &out[0];

        assert_eq!(doc[0].as_str().unwrap(), "string");
        assert_eq!(doc[1].as_str().unwrap(), "string");
        assert_eq!(doc[2].as_str().unwrap(), "string");
        assert_eq!(doc[3].as_i64().unwrap(), 123);
        assert_eq!(doc[4].as_i64().unwrap(), -321);
        assert_eq!(doc[5].as_f64().unwrap(), 1.23);
        assert_eq!(doc[6].as_f64().unwrap(), -1e4);
        assert!(doc[7].is_null());
        assert!(doc[8].is_null());
        assert_eq!(doc[9].as_bool().unwrap(), true);
        assert_eq!(doc[10].as_bool().unwrap(), false);
        assert_eq!(doc[11].as_str().unwrap(), "0");
        assert_eq!(doc[12].as_i64().unwrap(), 100);
        assert_eq!(doc[13].as_f64().unwrap(), 2.0);
        assert!(doc[14].is_null());
        assert_eq!(doc[15].as_bool().unwrap(), true);
        assert_eq!(doc[16].as_bool().unwrap(), false);
        assert_eq!(doc[17].as_i64().unwrap(), 255);
        //assert!(doc[18].is_badvalue());
        //assert!(doc[19].is_badvalue());
        //assert!(doc[20].is_badvalue());
        //assert!(doc[21].is_badvalue());
        assert_eq!(doc[22].as_i64().unwrap(), 63);
        assert_eq!(doc[23][0].as_i64().unwrap(), 15);
        assert_eq!(doc[23][1].as_i64().unwrap(), 15);
        assert_eq!(doc[24].as_i64().unwrap(), 12345);
        assert!(doc[25][0].as_bool().unwrap());
        assert!(!doc[25][1].as_bool().unwrap());
    }

    #[test]
    fn test_bad_hypen() {
        // See: https://github.com/chyh1990/yaml-rust/issues/23
        let s = "{-";
        assert!(load_from_str(&s).is_err());
    }

    #[test]
    fn test_issue_65() {
        // See: https://github.com/chyh1990/yaml-rust/issues/65
        let b = "\n\"ll\\\"ll\\\r\n\"ll\\\"ll\\\r\r\r\rU\r\r\rU";
        assert!(load_from_str(&b).is_err());
    }

    #[test]
    fn test_bad_docstart() {
        assert!(load_from_str("---This used to cause an infinite loop").is_ok());
        assert_eq!(
            load_from_str("----"),
            Ok(vec![Yaml::String(String::from("----"))])
        );
        assert_eq!(
            load_from_str("--- #here goes a comment"),
            Ok(vec![Yaml::Null])
        );
        assert_eq!(
            load_from_str("---- #here goes a comment"),
            Ok(vec![Yaml::String(String::from("----"))])
        );
    }

    #[test]
    fn test_plain_datatype_with_into_methods() {
        let s = "
- 'string'
- \"string\"
- string
- 123
- -321
- 1.23
- -1e4
- true
- false
- !!str 0
- !!int 100
- !!float 2
- !!bool true
- !!bool false
- 0xFF
- 0o77
- +12345
- -.INF
- .NAN
- !!float .INF
";
        let mut out = load_from_str(&s).unwrap().into_iter();
        let mut doc = out.next().unwrap().into_iter();

        assert_eq!(doc.next().unwrap().into_string().unwrap(), "string");
        assert_eq!(doc.next().unwrap().into_string().unwrap(), "string");
        assert_eq!(doc.next().unwrap().into_string().unwrap(), "string");
        assert_eq!(doc.next().unwrap().into_i64().unwrap(), 123);
        assert_eq!(doc.next().unwrap().into_i64().unwrap(), -321);
        assert_eq!(doc.next().unwrap().into_f64().unwrap(), 1.23);
        assert_eq!(doc.next().unwrap().into_f64().unwrap(), -1e4);
        assert_eq!(doc.next().unwrap().into_bool().unwrap(), true);
        assert_eq!(doc.next().unwrap().into_bool().unwrap(), false);
        assert_eq!(doc.next().unwrap().into_string().unwrap(), "0");
        assert_eq!(doc.next().unwrap().into_i64().unwrap(), 100);
        assert_eq!(doc.next().unwrap().into_f64().unwrap(), 2.0);
        assert_eq!(doc.next().unwrap().into_bool().unwrap(), true);
        assert_eq!(doc.next().unwrap().into_bool().unwrap(), false);
        assert_eq!(doc.next().unwrap().into_i64().unwrap(), 255);
        assert_eq!(doc.next().unwrap().into_i64().unwrap(), 63);
        assert_eq!(doc.next().unwrap().into_i64().unwrap(), 12345);
        assert_eq!(doc.next().unwrap().into_f64().unwrap(), f64::NEG_INFINITY);
        assert!(doc.next().unwrap().into_f64().is_some());
        assert_eq!(doc.next().unwrap().into_f64().unwrap(), f64::INFINITY);
    }

    /*
    #[test]
    fn test_hash_order() {
        let s = "---
b: ~
a: ~
c: ~
";
        let out = load_from_str(&s).unwrap();
        let first = out.into_iter().next().unwrap();
        let mut iter = first.into_hash().unwrap().into_iter();
        assert_eq!(
            Some((Yaml::String("b".to_owned()), Yaml::Null)),
            iter.next()
        );
        assert_eq!(
            Some((Yaml::String("a".to_owned()), Yaml::Null)),
            iter.next()
        );
        assert_eq!(
            Some((Yaml::String("c".to_owned()), Yaml::Null)),
            iter.next()
        );
        assert_eq!(None, iter.next());
    }
    */

    #[test]
    fn test_integer_key() {
        let s = "
0:
    important: true
1:
    important: false
";
        let out = load_from_str(&s).unwrap();
        let first = out.into_iter().next().unwrap();
        assert_eq!(first[0]["important"].as_bool().unwrap(), true);
    }

    #[test]
    fn test_indentation_equality() {
        let four_spaces = load_from_str(
            r#"
hash:
    with:
        indentations
"#,
        ).unwrap()
        .into_iter()
        .next()
        .unwrap();

        let two_spaces = load_from_str(
            r#"
hash:
  with:
    indentations
"#,
        ).unwrap()
        .into_iter()
        .next()
        .unwrap();

        let one_space = load_from_str(
            r#"
hash:
 with:
  indentations
"#,
        ).unwrap()
        .into_iter()
        .next()
        .unwrap();

        let mixed_spaces = load_from_str(
            r#"
hash:
     with:
               indentations
"#,
        ).unwrap()
        .into_iter()
        .next()
        .unwrap();

        assert_eq!(four_spaces, two_spaces);
        assert_eq!(two_spaces, one_space);
        assert_eq!(four_spaces, mixed_spaces);
    }

    #[test]
    fn test_two_space_indentations() {
        // https://github.com/kbknapp/clap-rs/issues/965

        let s = r#"
subcommands:
  - server:
    about: server related commands
subcommands2:
  - server:
      about: server related commands
subcommands3:
 - server:
    about: server related commands
            "#;

        let out = load_from_str(&s).unwrap();
        let doc = &out.into_iter().next().unwrap();

        println!("{:#?}", doc);
        assert_eq!(doc["subcommands"][0]["server"], Yaml::Null);
        assert!(doc["subcommands2"][0]["server"].as_hash().is_some());
        assert!(doc["subcommands3"][0]["server"].as_hash().is_some());
    }

    #[test]
    fn test_recursion_depth_check_objects() {
        let s = "{a:".repeat(10_000) + &"}".repeat(10_000);
        assert!(load_from_str(&s).is_err());
    }

    #[test]
    fn test_recursion_depth_check_arrays() {
        let s = "[".repeat(10_000) + &"]".repeat(10_000);
        assert!(load_from_str(&s).is_err());
    }
}
