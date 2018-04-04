extern crate parser_c;
extern crate walkdir;

use std::fs::read_to_string;
use parser_c::parse_str;
use walkdir::WalkDir;

#[test]
fn eval_smoke() {
    for item in WalkDir::new("smoke") {
        if let Ok(entry) = item {
            if entry.path().extension().map_or(false, |v| v == "c") {
                let input = read_to_string(entry.path()).unwrap();
                match parse_str(&input, &entry.path().display().to_string()) {
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
