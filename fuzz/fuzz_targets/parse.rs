#![no_main]
#[macro_use] extern crate libfuzzer_sys;
extern crate parser_c;

use std::str;

fuzz_target!(|data: &[u8]| {
    if let Ok(data) = str::from_utf8(&data) {
        let _ = parser_c::parse(&data, "input");
    }
});
