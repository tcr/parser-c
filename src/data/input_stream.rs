// Original file: "InputStream.hs"
// File auto-generated using Corollary.

use corollary_support::FilePath;

use std::str;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub struct InputStream {
    src: Vec<u8>,
    rpos: usize,
    tpos: usize,
}

impl InputStream {

    pub fn from_file(f: &FilePath) -> InputStream {
        let mut src = vec![];
        File::open(&f.path).unwrap().read_to_end(&mut src).unwrap();
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

    // TODO correct unicode char handling

    pub fn read_byte(&mut self) -> u8 {
        let byte = self.src[self.rpos];
        self.rpos += 1;
        byte
    }

    pub fn read_char(&mut self) -> char {
        let ch = self.src[self.rpos] as char;
        self.rpos += 1;
        ch
    }

    pub fn mark_read(&mut self, len: usize) {
        self.tpos += len;
        self.rpos = self.tpos;
    }

    pub fn last_string(&self, len: usize) -> &str {
        str::from_utf8(&self.src[self.tpos-len..self.tpos]).unwrap() // TODO UNICODE
    }

    pub fn count_lines(self) -> isize {
        // TODO
        self.to_string().lines().count() as isize
    }
}
