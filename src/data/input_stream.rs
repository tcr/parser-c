// Original file: "InputStream.hs"
// File auto-generated using Corollary.

use std::str;
use std::char;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use data::position::Position;

#[derive(Debug)]
pub struct InputStream {
    src: Vec<u8>,
    peek_pos: usize,
    tok_pos: usize,
}

impl InputStream {

    pub fn from_file<P: AsRef<Path>>(p: P) -> InputStream {
        let mut src = vec![];
        File::open(p.as_ref()).unwrap().read_to_end(&mut src).unwrap();
        InputStream { src: src, peek_pos: 0, tok_pos: 0 }
    }

    pub fn from_string(src: String) -> InputStream {
        InputStream { src: src.into_bytes(), peek_pos: 0, tok_pos: 0 }
    }

    pub fn to_string(self) -> String {
        String::from_utf8_lossy(&self.src[self.tok_pos..]).into_owned()
    }

    pub fn is_done(&self) -> bool {
        self.peek_pos == self.src.len()
    }

    pub fn peek_byte(&mut self) -> Option<u8> {
        if self.is_done() {
            None
        } else {
            let byte = self.src[self.peek_pos];
            self.peek_pos += 1;
            Some(byte)
        }
    }

    pub fn last_char(&self) -> Option<char> {
        char::decode_utf8(self.src[self.tok_pos..].iter().cloned()).next()?.ok()
    }

    pub fn last_string(&self, len: usize) -> &str {
        str::from_utf8(&self.src[self.tok_pos - len..self.tok_pos]).unwrap()
    }

    pub fn move_token(&mut self, len: usize, pos: &mut Position) {
        let tok_str = &self.src[self.tok_pos..self.tok_pos+len];
        for &byte in tok_str {
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
        self.tok_pos += len;
        self.peek_pos = self.tok_pos;
    }
}
