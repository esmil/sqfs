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
        #[allow(unused)]
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
        #[allow(unused)]
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
        #[allow(unused)]
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

    #[allow(unused)]
    pub fn is_null(&self) -> bool {
        match self.v {
            JSON::Null => true,
            _ => false,
        }
    }

    #[allow(unused)]
    pub fn is_array(&self) -> bool {
        match self.v {
            JSON::Array(_) => true,
            _ => false,
        }
    }

    #[allow(unused)]
    pub fn is_object(&self) -> bool {
        match self.v {
            JSON::Object(_) => true,
            _ => false,
        }
    }

    pub fn expect_object<'a, 'b>(&'a self) -> ExpectResult<'b, ExpectMap<'a>> {
        match *self {
            AnnoJSON { line, col, v: JSON::Object(ref m) } =>
                Ok(ExpectMap { line, col, m }),
            AnnoJSON { line, col, .. } =>
                Err(ExpectError { line, col, typ: ExpectErrorType::Object }),
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

enum ExpectErrorType<'a> {
    Key(&'a str),
    IntegerRange(i64, i64),
    String,
    Array,
    Object,
}

pub struct ExpectError<'a> {
    line: usize,
    col: usize,
    typ: ExpectErrorType<'a>
}

impl<'a> std::fmt::Display for ExpectError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.typ {
            ExpectErrorType::Key(k) =>
                write!(f, "expected key '{}' at line {} column {}",
                       k, self.line, self.col),
            ExpectErrorType::IntegerRange(min, max) =>
                write!(f, "expected integer between {} and {} at line {} column {}",
                       min, max, self.line, self.col),
            ExpectErrorType::String =>
                write!(f, "expected string at line {} column {}",
                       self.line, self.col),
            ExpectErrorType::Array =>
                write!(f, "expected array at line {} column {}",
                       self.line, self.col),
            ExpectErrorType::Object =>
                write!(f, "expected object at line {} column {}",
                       self.line, self.col),
        }
    }
}

pub type ExpectResult<'a, T> = Result<T, ExpectError<'a>>;

pub struct ExpectMap<'a> {
    pub line: usize,
    pub col: usize,
    pub m: &'a Map,
}

impl<'a> ExpectMap<'a> {
    pub fn int_range<'b>(&self, idx: &'b str, min: i64, max: i64) -> ExpectResult<'b, i64> {
        match self.m.get(idx) {
            Some(&AnnoJSON { v: JSON::Integer(n), .. })
                if n >= min && n <= max => Ok(n),
            Some(&AnnoJSON { line, col, .. }) =>
                Err(ExpectError {
                    line,
                    col,
                    typ: ExpectErrorType::IntegerRange(min, max)
                }),
            None =>
                Err(ExpectError {
                    line: self.line,
                    col: self.col,
                    typ: ExpectErrorType::Key(idx)
                }),
        }
    }

    pub fn u32<'b>(&self, idx: &'b str) -> ExpectResult<'b, u32> {
        let n = self.int_range(idx, 0, i64::from(u32::max_value()))?;
        Ok(n as u32)
    }

    pub fn str<'b>(&self, idx: &'b str) -> ExpectResult<'b, &'a str> {
        match self.m.get(idx) {
            Some(&AnnoJSON { v: JSON::String(ref s), .. }) => Ok(s),
            Some(&AnnoJSON { line, col, .. }) =>
                Err(ExpectError {
                    line,
                    col,
                    typ: ExpectErrorType::String
                }),
            None =>
                Err(ExpectError {
                    line: self.line,
                    col: self.col,
                    typ: ExpectErrorType::Key(idx)
                }),
        }
    }

    pub fn vec<'b>(&self, idx: &'b str) -> ExpectResult<'b, &'a Vec<AnnoJSON>> {
        match self.m.get(idx) {
            Some(&AnnoJSON { v: JSON::Array(ref v), .. }) => Ok(v),
            Some(&AnnoJSON { line, col, .. }) =>
                Err(ExpectError {
                    line,
                    col,
                    typ: ExpectErrorType::Array
                }),
            None =>
                Err(ExpectError {
                    line: self.line,
                    col: self.col,
                    typ: ExpectErrorType::Key(idx)
                }),
        }
    }

    pub fn maybe_int_range<'b>(&self, idx: &'b str, min: i64, max: i64) -> ExpectResult<'b, Option<i64>> {
        match self.m.get(idx) {
            Some(&AnnoJSON { v: JSON::Integer(n), .. })
                if n >= min && n <= max => Ok(Some(n)),
            Some(&AnnoJSON { line, col, .. }) =>
                Err(ExpectError {
                    line,
                    col,
                    typ: ExpectErrorType::IntegerRange(min, max)
                }),
            None => Ok(None),
        }
    }
}

