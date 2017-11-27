// Original file: "Constants.hs"
// File auto-generated using Corollary.

use std::fmt::{self, Write};
use std::char;
use std::str::FromStr;


// ------------------------------------------------------------------------------
// Formatting utilities

fn format_wide_flag(flag: bool) -> &'static str {
    if flag { "L" } else { "" }
}

fn escape(f: &mut fmt::Formatter, ch: char, quote: char) -> fmt::Result {
    match ch {
        '\\'   => f.write_str("\\\\"),
        '\x07' => f.write_str("\\a"),
        '\x08' => f.write_str("\\b"),
        '\x0b' => f.write_str("\\v"),
        '\x0c' => f.write_str("\\f"),
        '\x1b' => f.write_str("\\e"),
        '\n'   => f.write_str("\\n"),
        '\r'   => f.write_str("\\r"),
        '\t'   => f.write_str("\\t"),
        _ if ch == quote => { f.write_char('\\')?; f.write_char(quote) },
        _ if (ch >= ' ' && ch <= '~') => f.write_char(ch),
        _ if (ch as u32) < 512 => write!(f, "\\{:03o}", ch as u32),
        _ => write!(f, "\\x{:x}", ch as u32),
    }
}


// ------------------------------------------------------------------------------
// Character constants

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum CChar {
    Char(char, bool),
    Chars(Vec<char>, bool),
}

impl fmt::Display for CChar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}'", format_wide_flag(self.is_wide()))?;
        match *self {
            CChar::Char(ch, _) => escape(f, ch, '\'')?,
            CChar::Chars(ref chars, _) => {
                for &ch in chars.iter() {
                    escape(f, ch, '\'')?;
                }
            }
        }
        f.write_str("'")
    }
}

impl CChar {
    pub fn new(ch: char) -> CChar {
        CChar::Char(ch, false)
    }

    pub fn new_wide(ch: char) -> CChar {
        CChar::Char(ch, true)
    }

    pub fn new_multi(chars: Vec<char>, wide: bool) -> CChar {
        CChar::Chars(chars, wide)
    }

    pub fn into_string(self) -> String {
        match self {
            CChar::Char(c, _) => c.to_string(),
            CChar::Chars(cs, _) => cs.into_iter().collect(),
        }
    }

    pub fn as_int(&self) -> Option<isize> {
        match *self {
            CChar::Char(ch, _) => Some(ch as isize),
            CChar::Chars(_, _) => None,
        }
    }

    pub fn is_wide(&self) -> bool {
        match *self {
            CChar::Char(_, wide) | CChar::Chars(_, wide) => wide,
        }
    }

    pub fn unescape(s: &str) -> Result<(char, &str), String> {
        let mut iter = s.chars();
        match iter.next() {
            None => Err("No character to unescape".into()),
            Some('\\') => match iter.next() {
                None => Ok(('\\', "")),
                Some(ch) => Ok(match ch {
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
                            return Err("Bad hex escape sequence".into());
                        }
                        let codepoint = u32::from_str_radix(&s[2..2+digits], 16).ok()
                            .and_then(char::from_u32)
                            .ok_or("Bad hex escape sequence")?;
                        (codepoint, &s[2+digits..])
                    }
                    dig0 => {
                        if !dig0.is_digit(8) {
                            return Err("Bad octal escape sequence".into());
                        }
                        let digits = iter.position(|x| !x.is_digit(8)).unwrap_or(s.len() - 2);
                        let codepoint = u32::from_str_radix(&s[1..2+digits], 8).ok()
                            .and_then(char::from_u32)
                            .ok_or("Bad octal escape sequence")?;
                        (codepoint, &s[2+digits..])
                    }
                })
            },
            Some(ch) => Ok((ch, iter.as_str()))
        }
    }

    pub fn unescape_multi(mut cs: &str) -> Result<Vec<char>, String> {
        let mut new_vec = Vec::with_capacity(cs.len());
        while !cs.is_empty() {
            let (ch, newcs) = Self::unescape(cs)?;
            cs = newcs;
            new_vec.push(ch);
        }
        Ok(new_vec)
    }
}


// ------------------------------------------------------------------------------
// Integer constants

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum CIntRepr {
    Dec,
    Hex,
    Octal,
}

bitflags! {
    pub struct CIntFlags: u32 {
        const FLAG_UNSIGNED = 0b00000001;
        const FLAG_LONG     = 0b00000010;
        const FLAG_LONGLONG = 0b00000100;
        const FLAG_IMAG     = 0b00001000;
    }
}

impl FromStr for CIntFlags {
    type Err = String;

    fn from_str(mut s: &str) -> Result<CIntFlags, String> {
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
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CInteger(pub u128, pub CIntRepr, pub CIntFlags);

impl fmt::Display for CInteger {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.1 {
            CIntRepr::Dec => write!(f, "{}", self.0)?,
            CIntRepr::Hex => write!(f, "{:#x}", self.0)?,
            CIntRepr::Octal => write!(f, "0{:o}", self.0)?,
        }
        if self.2.contains(FLAG_UNSIGNED) { write!(f, "u")?; }
        if self.2.contains(FLAG_LONG)     { write!(f, "L")?; }
        if self.2.contains(FLAG_LONGLONG) { write!(f, "LL")?; }
        if self.2.contains(FLAG_IMAG)     { write!(f, "i")?; }
        Ok(())
    }
}

impl CInteger {
    pub fn new(i: u128) -> CInteger {
        CInteger(i, CIntRepr::Dec, CIntFlags::empty())
    }

    // Not a FromStr because of the extra argument.
    pub fn parse(repr: CIntRepr, s: &str) -> Result<CInteger, String> {
        let base = match repr {
            CIntRepr::Dec => 10,
            CIntRepr::Hex => 16,
            CIntRepr::Octal => 8,
        };
        let end = s.chars().position(|x| !x.is_digit(base)).unwrap_or(s.len());
        let number = match u128::from_str_radix(&s[..end], base) {
            Ok(n) => n,
            Err(_) => return Err(format!("Bad Integer literal: {:?}", s)),
        };
        let flags = s[end..].parse()?;
        Ok(CInteger(number, repr, flags))
    }

    /// Fix the 'octal' lexing of '0'
    pub fn parse_octal(s: &str) -> Result<CInteger, String> {
        if s.chars().nth(0) == Some('0') {
            if s.len() > 1 && s.chars().nth(1).unwrap().is_digit(8) {
                Self::parse(CIntRepr::Octal, &s[1..])
            } else {
                Self::parse(CIntRepr::Dec, &s)
            }
        } else {
            panic!("CInteger::parse_octal: string does not start with '0'")
        }
    }

    pub fn as_int(&self) -> u128 {
        self.0
    }
}


// ------------------------------------------------------------------------------
// Floating-point constants

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CFloat(pub String);

impl fmt::Display for CFloat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl CFloat {
    pub fn new(input: f64) -> CFloat {
        CFloat(input.to_string())
    }

    pub fn parse(input: &str) -> CFloat {
        CFloat(input.to_string())
    }
}


// ------------------------------------------------------------------------------
// String constants

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CString(pub String, pub bool);

impl fmt::Display for CString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\"", format_wide_flag(self.1))?;
        for ch in self.0.chars() {
            escape(f, ch, '"')?;
        }
        f.write_str("\"")
    }
}

impl CString {
    pub fn new(s: String) -> CString {
        CString(s, false)
    }

    pub fn new_wide(s: String) -> CString {
        CString(s, true)
    }

    pub fn concat(cs: Vec<CString>) -> CString {
        let wideflag = cs.iter().any(CString::is_wide);
        let mut new_str = String::new();
        new_str.extend(cs.into_iter().map(|v| v.0));
        CString(new_str, wideflag)
    }

    pub fn into_string(self) -> String {
        self.0
    }

    pub fn is_wide(&self) -> bool {
        self.1
    }

    pub fn unescape(mut cs: &str) -> Result<String, String> {
        let mut new_str = String::with_capacity(cs.len());
        while !cs.is_empty() {
            let (ch, newcs) = CChar::unescape(cs)?;
            cs = newcs;
            new_str.push(ch);
        }
        Ok(new_str)
    }
}


// ------------------------------------------------------------------------------
// Special Clang constants

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ClangCVersion(pub String);

impl ClangCVersion {
    pub fn parse(input: &str) -> ClangCVersion {
        ClangCVersion(input.to_string())
    }
}
