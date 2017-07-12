// Original file: "Constants.hs"
// File auto-generated using Corollary.

use std::char;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum CChar {
    CChar(char, bool),
    CChars(Vec<char>, bool),
}
pub use self::CChar::*;

pub fn showCharConst(c: char) -> String {
    format!("'{}'", escapeCChar(c))
}

pub fn showWideFlag(flag: bool) -> &'static str {
    if flag { "L" } else { "" }
}

pub fn getCChar(ch: CChar) -> String {
    match ch {
        CChar(c, _) => c.to_string(),
        CChars(cs, _) => cs.into_iter().collect(),
    }
}

pub fn getCCharAsInt(ch: CChar) -> isize {
    match ch {
        CChar(c, _) => c as isize,
        CChars(_, _) => {
            panic!("integer value of multi-character character constants is implementation defined")
        }
    }
}

pub fn isWideChar(ch: CChar) -> bool {
    match ch {
        CChar(_, wideFlag) |
        CChars(_, wideFlag) => wideFlag,
    }
}

pub fn cChar(c: char) -> CChar {
    CChar(c, false)
}

pub fn cChar_w(c: char) -> CChar {
    CChar(c, true)
}

pub fn cChars(b: bool, a: Vec<char>) -> CChar {
    CChars(a, b)
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum CIntRepr {
    DecRepr,
    HexRepr,
    OctalRepr,
}
pub use self::CIntRepr::*;

bitflags! {
    pub struct CIntFlags: u32 {
        const FLAG_UNSIGNED = 0b00000001;
        const FLAG_LONG     = 0b00000010;
        const FLAG_LONGLONG = 0b00000100;
        const FLAG_IMAG     = 0b00001000;
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CInteger(pub isize, pub CIntRepr, pub CIntFlags);


fn parseFlags(mut s: &str) -> Result<CIntFlags, String> {
    let mut flags = CIntFlags::empty();
    loop {
        if s.is_empty() {
            return Ok(flags);
        }
        if s.starts_with("ll") || s.starts_with("LL") {
            s = &s[2..];
            flags |= FLAG_LONGLONG;
            continue;
        }
        match &s[0..1] {
            "l" => flags |= FLAG_LONG,
            "L" => flags |= FLAG_LONG,
            "u" => flags |= FLAG_UNSIGNED,
            "U" => flags |= FLAG_UNSIGNED,
            "i" => flags |= FLAG_IMAG,
            "I" => flags |= FLAG_IMAG,
            "j" => flags |= FLAG_IMAG,
            "J" => flags |= FLAG_IMAG,
            _ => return Err(format!("Unexpected flags {}", s)),
        }
        s = &s[1..];
    }
}

pub fn readCInteger(repr: CIntRepr, s: &str) -> Result<CInteger, String> {
    let base = match repr {
        DecRepr => 10,
        HexRepr => 16,
        OctalRepr => 8,
    };
    let end = s.chars().position(|x| !x.is_digit(base)).unwrap_or(s.len());
    let number = match isize::from_str_radix(&s[..end], base) {
        Ok(n) => n,
        Err(_) => return Err(format!("Bad Integer literal: {:?}", s)),
    };
    let flags = parseFlags(&s[end..])?;
    Ok(CInteger(number, repr, flags))
}

/// Fix the 'octal' lexing of '0'
pub fn readCOctal(s: &str) -> Result<CInteger, String> {
    if s.chars().nth(0) == Some('0') {
        if s.len() > 1 && s.chars().nth(1).unwrap().is_digit(8) {
            readCInteger(OctalRepr, &s[1..])
        } else {
            readCInteger(DecRepr, &s)
        }
    } else {
        panic!("ReadOctal: string does not start with `0'")
    }
}

pub fn getCInteger(CInteger(i, _, _): CInteger) -> isize {
    i
}

pub fn cInteger(i: isize) -> CInteger {
    CInteger(i, DecRepr, CIntFlags::empty())
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CFloat(pub String);


pub fn cFloat(input: f32) -> CFloat {
    CFloat(input.to_string())
}

pub fn readCFloat(input: &str) -> CFloat {
    CFloat(input.to_string())
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ClangCVersion(pub String);


pub fn readClangCVersion(input: &str) -> ClangCVersion {
    ClangCVersion(input.to_string())
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CString(pub String, pub bool);

impl Display for CString {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let CString(ref s, b) = *self;
        write!(f, "{}{}", showWideFlag(b), showStringLit(s))
    }
}

pub fn cString(__str: String) -> CString {
    CString(__str, false)
}

pub fn cString_w(__str: String) -> CString {
    CString(__str, true)
}

pub fn getCString(s: CString) -> String {
    s.0
}

pub fn isWideString(s: &CString) -> bool {
    s.1
}

pub fn concatCStrings(cs: Vec<CString>) -> CString {
    let wideflag = cs.iter().any(isWideString);
    let mut new_str = String::new();
    for s in cs {
        new_str.push_str(&s.0);
    }
    CString(new_str, wideflag)
}

pub fn showStringLit(s: &str) -> String {
    let mut new_s = String::with_capacity(s.len());
    new_s.push('"');
    for ch in s.chars() {
        if isSChar(ch) {
            new_s.push(ch);
        } else if ch == '"' {
            new_s.push_str("\\\"");
        } else {
            new_s.push_str(&escapeChar(ch));
        }
    }
    new_s.push('"');
    new_s
}

pub fn isAsciiSourceChar(c: char) -> bool {
    c >= ' ' && c <= '~'
}

pub fn isCChar(ch: char) -> bool {
    match ch {
        '\\' => false,
        '\'' => false,
        '\n' => false,
        c => isAsciiSourceChar(c),
    }
}

pub fn escapeCChar(ch: char) -> String {
    match ch {
        '\'' => "\\\'".to_string(),
        ch if isSChar(ch) => ch.to_string(),
        _ => escapeChar(ch)
    }
}

pub fn isSChar(ch: char) -> bool {
    match ch {
        '\\' => false,
        '\"' => false,
        '\n' => false,
        c => isAsciiSourceChar(c),
    }
}

pub fn escapeChar(ch: char) -> String {
    match ch {
        '\\' => "\\\\".to_string(),
        '\u{7}' => "\\a".to_string(),
        '\u{8}' => "\\b".to_string(),
        '\u{1b}' => "\\e".to_string(),
        '\u{c}' => "\\f".to_string(),
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        '\u{b}' => "\\v".to_string(),
        c => if (c as u32) < 512 {
            format!("\\{:03o}", c as u32)
        } else {
            format!("\\x{:x}", c as u32)
        }
    }
}

pub fn unescapeChar(s: &str) -> (char, &str) {
    let mut iter = s.chars();
    match iter.next() {
        None => panic!("unescapeChar: empty string"),
        Some('\\') => match iter.next() {
            None => ('\\', ""),
            Some(ch) => match ch {
                'n'  => ('\n',   &s[2..]),
                't'  => ('\t',   &s[2..]),
                'v'  => ('\x0b', &s[2..]),
                'b'  => ('\x08', &s[2..]),
                'r'  => ('\r',   &s[2..]),
                'f'  => ('\x0c', &s[2..]),
                'a'  => ('\x07', &s[2..]),
                'e'  => ('\x1b', &s[2..]),
                'E'  => ('\x1b', &s[2..]),
                '\\' => ('\\',   &s[2..]),
                '?'  => ('?',    &s[2..]),
                '\'' => ('\'',   &s[2..]),
                '\"' => ('\"',   &s[2..]),
                'x'  => {
                    let digits = iter.position(|x| !x.is_digit(16)).unwrap_or(s.len() - 2);
                    if digits == 0 {
                        panic!("bad hex escape sequence"); // TODO should be a Result
                    }
                    (char::from_u32(u32::from_str_radix(&s[2..2+digits], 16).unwrap()).unwrap(),
                     &s[1+digits..])
                }
                dig0 => {
                    if !dig0.is_digit(8) {
                        panic!("bad hex escape sequence"); // TODO should be a Result
                    }
                    let digits = iter.position(|x| !x.is_digit(8)).unwrap_or(s.len() - 2);
                    (char::from_u32(u32::from_str_radix(&s[1..2+digits], 8).unwrap()).unwrap(),
                     &s[2+digits..])
                }
            }
        },
        Some(ch) => (ch, iter.as_str())
    }
}

pub fn unescapeString(mut cs: &str) -> String {
    let mut new_str = String::with_capacity(cs.len());
    while !cs.is_empty() {
        let (ch, newcs) = unescapeChar(cs);
        cs = newcs;
        new_str.push(ch);
    }
    new_str
}

pub fn unescapeMultiChars(mut cs: &str) -> Vec<char> {
    let mut new_vec = Vec::with_capacity(cs.len());
    while !cs.is_empty() {
        let (ch, newcs) = unescapeChar(cs);
        cs = newcs;
        new_vec.push(ch);
    }
    new_vec
}
