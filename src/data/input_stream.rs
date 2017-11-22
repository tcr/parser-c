// Original file: "InputStream.hs"
// File auto-generated using Corollary.

use std::str;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use data::position::Position;

#[derive(Debug)]
pub struct InputStream {
    src: Vec<u8>,
    rpos: usize,
    tpos: usize,
}

impl InputStream {

    pub fn from_file<P: AsRef<Path>>(p: P) -> InputStream {
        let mut src = vec![];
        File::open(p.as_ref()).unwrap().read_to_end(&mut src).unwrap();
        InputStream { src: src, rpos: 0, tpos: 0 }
    }

    pub fn from_string(src: String) -> InputStream {
        InputStream { src: src.into_bytes(), rpos: 0, tpos: 0 }
    }

    pub fn to_string(self) -> String {
        String::from_utf8_lossy(&self.src[self.tpos..]).into_owned()
    }

    pub fn is_done(&self) -> bool {
        self.rpos == self.src.len()
    }

    pub fn read_byte(&mut self) -> u8 {
        let byte = self.src[self.rpos];
        self.rpos += 1;
        byte
    }

    pub fn last_char(&self) -> char {
        // TODO correct unicode char handling
        self.src[self.tpos] as char
    }

    pub fn mark_read(&mut self, len: usize, pos: &mut Position) {
        for &byte in &self.src[self.tpos..self.tpos+len] {
            match byte {
                b'\n' => pos.inc_newline(),
                // TODO: handle other control chars
                b'\r' => pos.inc_offset(1),
                // UTF-8 start bytes
                0...127 | 192...255 => pos.inc_chars(1),
                // UTF-8 continuation bytes
                _ => pos.inc_offset(1),
            }
        }
        self.tpos += len;
        self.rpos = self.tpos;
    }

    pub fn last_string(&self, len: usize) -> &str {
        str::from_utf8(&self.src[self.tpos-len..self.tpos]).unwrap() // TODO UNICODE
    }
}
