extern crate parser_c;
extern crate walkdir;

use parser_c::parse;
use std::fs::File;
use walkdir::WalkDir;
use std::io::prelude::*;

#[test]
fn eval_smoke() {
    for item in WalkDir::new("tests/smoke") {
        if let Ok(entry) = item {
            if entry.path().display().to_string().ends_with(".c") {
                let mut input = String::new();
                File::open(entry.path()).unwrap().read_to_string(&mut input).unwrap();
                match parse(&input, &entry.path().display().to_string()) {
                    Err(err) => {
                        panic!("error: {}", err);
                    }
                    Ok(_) => {
                        println!("smoke test passed: {}", entry.path().display());
                    }
                }
            }
        }
    }
}
