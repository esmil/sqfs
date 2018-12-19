use yaml_rust::parser::{Parser, Event, MarkedEventReceiver};
use yaml_rust::scanner::{Marker, ScanError, TScalarStyle, TokenType};
use std::collections::HashMap;
use std::f64;
use std::i64;
use super::annojson::{self, AnnoJSON, JSON};

// parse f64 as Core schema
// See: https://github.com/chyh1990/yaml-rust/issues/51
fn parse_f64(v: &str) -> Result<f64, std::num::ParseFloatError> {
    match v {
        ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" => Ok(f64::INFINITY),
        "-.inf" | "-.Inf" | "-.INF" => Ok(f64::NEG_INFINITY),
        ".nan" | "NaN" | ".NAN" => Ok(f64::NAN),
        _ => v.parse::<f64>(),
    }
}

fn json_from_str(v: &str) -> JSON {
    match v {
        "~" | "null" => return JSON::Null,
        "true"       => return JSON::Boolean(true),
        "false"      => return JSON::Boolean(false),
        _            => {}
    };
    if v.starts_with("0x") {
        if let Ok(n) = i64::from_str_radix(&v[2..], 16) {
            return JSON::Integer(n);
        }
    }
    if v.starts_with("0o") {
        if let Ok(n) = i64::from_str_radix(&v[2..], 8) {
            return JSON::Integer(n);
        }
    }
    if v.starts_with('0') {
        if let Ok(n) = i64::from_str_radix(&v[1..], 8) {
            return JSON::Integer(n);
        }
    }
    if v.starts_with('+') {
        if let Ok(n) = v[1..].parse::<i64>() {
            return JSON::Integer(n);
        }
    }
    if let Ok(n) = v.parse::<i64>() {
        return JSON::Integer(n);
    }
    if let Ok(n) = parse_f64(v) {
        return JSON::Float(n);
    }
    JSON::String(v.to_owned())
}

#[derive(Debug)]
enum StackEntry {
    Array(usize, usize, annojson::Array, usize),
    Map(usize, usize, annojson::Map, usize),
    Key(String),
}

struct Loader {
    result: Result<AnnoJSON, ScanError>,
    stack: Vec<StackEntry>,
    anchors: HashMap<usize, AnnoJSON>,
}

impl Loader {
    fn insert(&mut self, node: AnnoJSON, loc: Marker, aid: usize) {
        if aid > 0 {
            self.anchors.insert(aid, node.clone());
        }
        match self.stack.pop() {
            Some(StackEntry::Array(line, col, mut v, aid)) => {
                v.push(node);
                self.stack.push(StackEntry::Array(line, col, v, aid));
            }
            Some(StackEntry::Map(line, col, m, aid)) => {
                if let AnnoJSON { v: JSON::String(s), .. } = node {
                    self.stack.push(StackEntry::Map(line, col, m, aid));
                    self.stack.push(StackEntry::Key(s));
                } else {
                    self.error(loc, "non-string keys not allowed");
                }
            }
            Some(StackEntry::Key(key)) => {
                let m = match self.stack.last_mut() {
                    Some(StackEntry::Map(_, _, ref mut m, _)) => m,
                    _ => unreachable!(),
                };
                m.insert(key, node);
            }
            None => {
                self.result = Ok(node);
            }
        }
    }

    fn error(&mut self, loc: Marker, info: &'static str) {
        self.result = Err(ScanError::new(loc, info));
    }
}

impl MarkedEventReceiver for Loader {
    fn on_event(&mut self, ev: Event, m: Marker) {
        if self.result.is_err() {
            return;
        }
        //println!("{:3}:{:2} {:?}", m.line(), m.col(), ev);
        match ev {
            Event::Nothing => {}
            Event::StreamStart => {}
            Event::StreamEnd => {}
            Event::DocumentStart => {}
            Event::DocumentEnd => {}
            Event::Alias(id) => {
                let node = match self.anchors.get(&id) {
                    Some(v) => v.clone(),
                    None => return self.error(m, "invalid anchor"),
                };
                self.insert(node, m, 0);
            }
            Event::Scalar(v, style, aid, tag) => {
                let v = if style != TScalarStyle::Plain {
                    JSON::String(v)
                } else if let Some(TokenType::Tag(ref handle, ref suffix)) = tag {
                    // XXX tag:yaml.org,2002:
                    if handle == "!!" {
                        match suffix.as_ref() {
                            "null" => match v.as_ref() {
                                "~" | "null" => JSON::Null,
                                _ => return self.error(m, "null must be '~' or 'null'"),
                            },
                            "bool" => {
                                match v.as_str() {
                                    "true"  => JSON::Boolean(true),
                                    "false" => JSON::Boolean(false),
                                    _ => return self.error(m, "boolean must be 'true' or 'false'"),
                                }
                            }
                            "int" => match v.parse::<i64>() {
                                Ok(v)  => JSON::Integer(v),
                                Err(_) => return self.error(m, "invalid integer"),
                            },
                            "float" => match parse_f64(&v) {
                                Ok(n)  => JSON::Float(n),
                                Err(_) => return self.error(m, "invalid float"),
                            },
                            "str" => JSON::String(v),
                            _ => return self.error(m, "unknown tag suffix"),
                        }
                    } else {
                        return self.error(m, "unknown tag handle");
                    }
                } else {
                    // Datatype is not specified, or unrecognized
                    json_from_str(&v)
                };
                self.insert(AnnoJSON { line: m.line(), col: m.col(), v }, m, aid);
            }
            Event::SequenceStart(aid) => {
                self.stack.push(StackEntry::Array(m.line(), m.col(), Vec::new(), aid));
            }
            Event::SequenceEnd => {
                let (node, aid) = match self.stack.pop() {
                    Some(StackEntry::Array(line, col, v, aid)) => {
                        (AnnoJSON { line, col, v: JSON::Array(v) }, aid)
                    }
                    _ => unreachable!(),
                };
                self.insert(node, m, aid);
            }
            Event::MappingStart(aid) => {
                self.stack.push(StackEntry::Map(m.line(), m.col(), annojson::Map::new(), aid));
            }
            Event::MappingEnd => {
                let (node, aid) = match self.stack.pop() {
                    Some(StackEntry::Map(line, col, m, aid)) => {
                        (AnnoJSON { line, col, v: JSON::Object(m) }, aid)
                    }
                    _ => unreachable!(),
                };
                self.insert(node, m, aid);
            }
        };
    }
}

pub fn load_from_str(source: &str) -> Result<AnnoJSON, ScanError> {
    let mut loader = Loader {
        result: Ok(AnnoJSON { line: 0, col: 0, v: JSON::Null }),
        stack: Vec::new(),
        anchors: Default::default(),
    };
    let mut parser = Parser::new(source.chars());
    parser.load(&mut loader, false)?;
    loader.result
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
        let doc = &out;
        assert_eq!(doc["a"].as_i64().unwrap(), 1i64);
        assert_eq!(doc["b"].as_f64().unwrap(), 2.2f64);
        assert_eq!(doc["c"][1].as_i64().unwrap(), 2i64);
        assert!(doc["d"][0].is_null());
    }

    #[test]
    fn test_empty_doc() {
        let s: String = "".to_owned();
        load_from_str(&s).unwrap();
        let s: String = "---".to_owned();
        let v = AnnoJSON { line: 0, col: 0, v: JSON::Null };
        assert_eq!(load_from_str(&s).unwrap()[0], v);
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
        let doc = &out;
        assert_eq!(doc["a7"].as_str().unwrap(), "你好");
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
        let doc = &out;
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
        let doc = &out;
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
        let doc = &out;

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

    /*
    #[test]
    fn test_issue_65() {
        // See: https://github.com/chyh1990/yaml-rust/issues/65
        let b = "\n\"ll\\\"ll\\\r\n\"ll\\\"ll\\\r\r\r\rU\r\r\rU";
        assert!(load_from_str(b).is_err());
    }
    */

    #[test]
    fn test_bad_docstart() {
        assert!(load_from_str("---This used to cause an infinite loop").is_ok());
        assert_eq!(
            load_from_str("----"),
            Ok(AnnoJSON { line: 1, col: 0, v: JSON::String(String::from("----")) })
        );
        assert_eq!(
            load_from_str("--- #here goes a comment"),
            Ok(AnnoJSON { line: 2, col: 0, v: JSON::Null })
        );
        assert_eq!(
            load_from_str("---- #here goes a comment"),
            Ok(AnnoJSON { line: 1, col: 0, v: JSON::String(String::from("----")) })
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
        let out = load_from_str(&s).unwrap();
        let mut doc = out.into_iter();

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

    #[test]
    fn test_indentation_equality() {
        let four_spaces = load_from_str(
            r#"
hash:
    with:
        indentations
"#,
        ).unwrap();

        let two_spaces = load_from_str(
            r#"
hash:
  with:
    indentations
"#,
        ).unwrap();

        let one_space = load_from_str(
            r#"
hash:
 with:
  indentations
"#,
        ).unwrap();

        let mixed_spaces = load_from_str(
            r#"
hash:
     with:
               indentations
"#,
        ).unwrap();

        assert_eq!(four_spaces["hash"]["with"].v, JSON::String("indentations".to_string()));
        assert_eq!(four_spaces["hash"]["with"].v, two_spaces["hash"]["with"].v);
        assert_eq!(two_spaces["hash"]["with"].v, one_space["hash"]["with"].v);
        assert_eq!(four_spaces["hash"]["with"].v, mixed_spaces["hash"]["with"].v);
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
        let doc = &out;

        println!("{:#?}", doc);
        assert_eq!(doc["subcommands"][0]["server"],
                   AnnoJSON { line: 4, col: 4, v: JSON::Null });
        assert!(doc["subcommands2"][0]["server"].as_map().is_some());
        assert!(doc["subcommands3"][0]["server"].as_map().is_some());
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
