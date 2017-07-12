// Original file: "InputStream.hs"
// File auto-generated using Corollary.

use corollary_support::FilePath;

use std::str;
use std::rc::Rc;
use std::fs::File;
use std::io::Read;

#[derive(Clone, Debug)]
pub struct InputStream {
    src: Rc<Vec<u8>>,
    pos: usize,
}

impl InputStream {

    pub fn from_file(f: &FilePath) -> InputStream {
        let mut src = vec![];
        File::open(&f.path).unwrap().read_to_end(&mut src).unwrap();
        InputStream { src: Rc::new(src), pos: 0 }
    }

    pub fn from_string(src: String) -> InputStream {
        InputStream { src: Rc::new(src.into_bytes()), pos: 0 }
    }

    pub fn to_string(self) -> String {
        String::from_utf8_lossy(&self.src[self.pos..]).into_owned()
    }

    pub fn is_empty(&self) -> bool {
        self.pos == self.src.len()
    }

    pub fn take_byte(mut self) -> (u8, InputStream) {
        let pos = self.pos;
        let byte = self.src[pos];
        self.pos += 1;
        (byte, self)
    }

    // TODO correct unicode char handling

    pub fn take_char(mut self) -> (char, InputStream) {
        let pos = self.pos;
        let ch = self.src[pos] as char;
        self.pos += 1;
        (ch, self)
    }

    pub fn take_string(&self, n: isize) -> &str {
        str::from_utf8(&self.src[self.pos..self.pos + n as usize]).unwrap() // TODO UNICODE
    }

    pub fn count_lines(self) -> isize {
        // TODO
        self.to_string().lines().count() as isize
    }
}
