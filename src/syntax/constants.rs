// Original file: "Constants.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Data::Bits;
// use Data::char;
// use Numeric;
// use showOct;
// use Data::Generics;

use num::ToPrimitive;
use std::marker::PhantomData;
use data::error::Error;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum CChar {
    CChar(char, bool),
    CChars(Vec<char>, bool),
}
pub use self::CChar::*;

pub fn showCharConst(c: char) -> Box<ShowS> {
    sQuote(escapeCChar(c))
}

pub fn _showWideFlag(flag: bool) -> Box<ShowS> {
    box if flag {
        showString("L".to_string())
    } else {
        showString("".to_string())
    }
}

pub fn getCChar(_0: CChar) -> String {
    match (_0) {
        CChar(c, _) => c.to_string(),
        CChars(cs, _) => cs.into_iter().collect(),
    }
}

pub fn getCCharAsInt(_0: CChar) -> isize {
    match (_0) {
        CChar(c, _) => fromIntegral(c as isize),
        CChars(_cs, _) => {
            panic!("integer value of multi-character character constants is implementation defined")
        }
    }
}

pub fn isWideChar(_0: CChar) -> bool {
    match (_0) {
        CChar(_, wideFlag) => wideFlag,
        CChars(_, wideFlag) => wideFlag,
    }
}

pub fn cChar(c: char) -> CChar {
    CChar(c, false)
}

pub fn cChar_w(c: char) -> CChar {
    CChar(c, true)
}

pub fn cChars(a: String, b: bool) -> CChar {
    CChars(a.chars().collect(), b)
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
        Err(e) => return Err(format!("Bad Integer literal: {:?}", s)),
    };
    let flags = parseFlags(&s[end..])?;
    Ok(CInteger(number, repr, flags))
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
    CFloat(show(input))
}

pub fn readCFloat(input: String) -> CFloat {
    CFloat(input)
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ClangCVersion(pub String);


pub fn readClangCVersion(input: String) -> ClangCVersion {
    ClangCVersion(input)
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CString(pub String, pub bool);

impl Display for CString {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let CString(s, b) = self.clone();
        write!(f, "{}", _showWideFlag(b).show_s(showStringLit(s).show_s("".to_string())))
    }
}

pub fn cString(__str: String) -> CString {
    CString(__str, false)
}

pub fn cString_w(__str: String) -> CString {
    CString(__str, true)
}

pub fn getCString(CString(__str, _): CString) -> String {
    __str
}

pub fn isWideString(CString(_, wideflag): CString) -> bool {
    wideflag
}

pub fn concatCStrings(cs: Vec<CString>) -> CString {
    CString(cs.clone().into_iter()
        .map(getCString)
        .collect::<Vec<_>>().join(""), (any(isWideString, cs)))
}

pub fn showStringLit(s: String) -> Box<ShowS> {
    dQuote(s.chars().map(|c| {
        if isSChar(c) {
            format!("{}", c)
        } else if c == '"' {
            "\\\"".to_string()
        } else {
            escapeChar(c)
        }
    }).collect::<Vec<_>>().join(""))
}

pub fn isAsciiSourceChar(c: char) -> bool {
    (isAscii(c) && isPrint(c))
}

pub fn isCChar(_0: char) -> bool {
    match (_0) {
        '\\' => false,
        '\'' => false,
        '\n' => false,
        c => isAsciiSourceChar(c),
    }
}

pub fn escapeCChar(_0: char) -> String {
    match (_0) {
        '\'' => "\\\'".to_string(),
        c => {
            if isSChar(c) {
                c.to_string()
            } else {
                escapeChar(c)
            }
        }
    }
}

pub fn isSChar(_0: char) -> bool {
    match (_0) {
        '\\' => false,
        '\"' => false,
        '\n' => false,
        c => isAsciiSourceChar(c),
    }
}

pub fn escapeChar(_0: char) -> String {
    match (_0) {
        '\\' => "\\\\".to_string(),
        '\u{7}' => "\\a".to_string(),
        '\u{8}' => "\\b".to_string(),
        '\u{1b}' => "\\e".to_string(),
        '\u{c}' => "\\f".to_string(),
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        '\u{b}' => "\\v".to_string(),
        c => {
            if ord(c) < 512 {
                format!("\\{:03o}", ord(c))
            } else {
                format!("\\x{:x}", ord(c))
            }
        }
    }
}

pub fn unescapeChar(_0: String) -> (char, String) {
    let v = _0.chars().collect::<Vec<_>>();
    // ['\\', c, cs..]
    if v.len() > 2 && v[0] == '\\' {
        let c = v[1];
        let cs: String = v[2..].into_iter().collect();
        return match c {
            'n' => ('\n', cs),
            't' => ('\t', cs),
            'v' => ('\u{b}', cs),
            'b' => ('\u{8}', cs),
            'r' => ('\r', cs),
            'f' => ('\u{c}', cs),
            'a' => ('\u{7}', cs),
            'e' => ('\u{1b}', cs),
            'E' => ('\u{1b}', cs),
            '\\' => ('\\', cs),
            '?' => ('?', cs),
            '\'' => ('\'', cs),
            '\"' => ('\"', cs),
            'x' => {
                match head_q("bad escape sequence",
                             readHex(cs).read_s()) {
                    (i, cs_q) => (i, cs_q),
                }
            }
            _ => {
                match head_q("bad escape sequence",
                             readOct_q(format!("{}{}", c, cs)).read_s()) {
                    (i, cs_q) => (i, cs_q),
                }
            }
        }
    }

    // [c, cs..]
    if v.len() > 0 {
        let c = v[0];
        let cs: String = v[1..].into_iter().collect();
        return (c, cs)
    }

    // []
    panic!("unescape char: empty string")
}

pub fn readOct_q(s: String) -> Box<ReadS<char>> {

    let octStr = takeWhile_str(isOctDigit, take_str(3, s));

    // TODO
    unreachable!()
    // let rest = drop_str((length(octStr)), s);

    // box readOct(octStr).map(|(i, cs)| { (i, __op_addadd(cs, rest)) })
}

pub fn unescapeString(cs: String) -> String {
    if cs.len() == 0 {
        cs
    } else {
        match unescapeChar(cs) {
            (c, cs_q) => format!("{}{}", c, unescapeString(cs_q)),
        }
    }
}

pub fn sQuote(s: String) -> Box<ShowS> {
    box showString(format!("\'{}\'", s))
}

pub fn dQuote(s: String) -> Box<ShowS> {
    box showString(format!("\"{}\"", s))
}

pub fn head_q<a>(msg: &str, mut _1: Vec<a>) -> a {
    if _1.is_empty() {
        panic!("{}", msg);
    } else {
        _1.remove(0)
    }
}

pub fn head_q_str(msg: &str, _1: String) -> char {
    if let Some(c) = _1.chars().next() {
       c
    } else {
        panic!("{}", msg);
    }
}
