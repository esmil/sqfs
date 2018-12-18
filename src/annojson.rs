use std::ops::Index;

pub type Array = Vec<AnnoJSON>;
pub type Map = std::collections::HashMap<String, AnnoJSON>;

#[derive(Clone, PartialEq, Debug)]
pub enum JSON {
    Null,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Array(Array),
    Object(Map),
}

#[derive(Clone, PartialEq, Debug)]
pub struct AnnoJSON {
    pub line: usize,
    pub col: usize,
    pub v: JSON,
}

macro_rules! define_as (
    ($name:ident, $t:ty, $e:ident) => (
        #[allow(dead_code)]
        pub fn $name(&self) -> Option<$t> {
            match self.v {
                JSON::$e(v) => Some(v),
                _ => None
            }
        }
    );
);

macro_rules! define_as_ref (
    ($name:ident, $t:ty, $e:ident) => (
        #[allow(dead_code)]
        pub fn $name(&self) -> Option<$t> {
            match self.v {
                JSON::$e(ref v) => Some(v),
                _ => None
            }
        }
    );
);

macro_rules! define_into (
    ($name:ident, $t:ty, $e:ident) => (
        #[allow(dead_code)]
        pub fn $name(self) -> Option<$t> {
            match self.v {
                JSON::$e(v) => Some(v),
                _ => None
            }
        }
    );
);

impl AnnoJSON {
    define_as!(as_bool, bool, Boolean);
    define_as!(as_i64, i64, Integer);
    define_as!(as_f64, f64, Float);

    define_as_ref!(as_str, &str, String);
    define_as_ref!(as_vec, &Array, Array);
    define_as_ref!(as_map, &Map, Object);

    define_into!(into_bool, bool, Boolean);
    define_into!(into_i64, i64, Integer);
    define_into!(into_f64, f64, Float);
    define_into!(into_string, String, String);
    define_into!(into_vec, Array, Array);
    define_into!(into_map, Map, Object);

    #[allow(dead_code)]
    pub fn is_null(&self) -> bool {
        match self.v {
            JSON::Null => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_array(&self) -> bool {
        match self.v {
            JSON::Array(_) => true,
            _ => false,
        }
    }

    #[allow(dead_code)]
    pub fn is_object(&self) -> bool {
        match self.v {
            JSON::Object(_) => true,
            _ => false,
        }
    }
}

static NULL: AnnoJSON = AnnoJSON { line: 0, col: 0, v: JSON::Null };

impl<'a> Index<&'a str> for AnnoJSON {
    type Output = AnnoJSON;

    fn index(&self, idx: &'a str) -> &AnnoJSON {
        match self.v {
            JSON::Object(ref m) => m.get(idx).unwrap_or(&NULL),
            _ => &NULL,
        }
    }
}

impl Index<usize> for AnnoJSON {
    type Output = AnnoJSON;

    fn index(&self, idx: usize) -> &AnnoJSON {
        match self.v {
            JSON::Array(ref v) => v.get(idx).unwrap_or(&NULL),
            _ => &NULL,
        }
    }
}

impl IntoIterator for AnnoJSON {
    type Item = AnnoJSON;
    type IntoIter = std::vec::IntoIter<AnnoJSON>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_vec().unwrap_or_else(Vec::new).into_iter()
    }
}
