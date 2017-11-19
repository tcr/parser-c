//! Generate individual test cases for all files in the gcc_pre directory.

use std::collections::HashSet;
use std::env;
use std::fs::{File, read_dir};
use std::io::{BufRead, BufReader, Write};
use std::path::Path;

fn read_set(name: &str) -> HashSet<String> {
    let file = File::open(format!("./gcc_expect_{}.txt", name)).unwrap();
    let mut set = HashSet::new();
    for line in BufReader::new(file).lines() {
        let line = line.unwrap();
        set.insert(line);
    }
    set
}

fn main() {
    let mut test_files = read_dir("./gcc_pre")
        .expect("Could not find gcc-dg tests (unpack into parser-c-tests/gcc_pre)")
        .filter_map(|v| v.ok())
        .collect::<Vec<_>>();
    test_files.sort_by_key(|d| d.path());

    let fail_list = read_set("fail");

    let out_path = Path::new(&env::var("OUT_DIR").unwrap()).join("gcc_dg_tests.rs");
    let mut out = File::create(out_path).unwrap();

    for entry in test_files {
        let file_str = entry.file_name();
        let file_str = file_str.to_string_lossy();
        if !file_str.ends_with(".i") {
            continue;
        }

        // custom skips

        // stack overflows
        if file_str == "inline-24.i" { continue; }
        if file_str == "20020425-1.i" { continue; }
        // invalid (256-bit) integer literal
        if file_str == "unnamed-1.i" { continue; }
        if file_str == "ms_unnamed-1.i" { continue; }
        // strange Unicode pragma
        if file_str == "pragma-darwin-2.i" { continue; }

        let test_name = file_str.trim_right_matches(".i").replace(&['-', '.', '+'][..], "__");
        let should_fail = fail_list.contains(&*file_str);
        let should_roundtrip = true;//roundtrip_list.contains(&*file_str);

        write!(out, "\
#[test]
#[allow(non_snake_case)]
fn gcc_dg_{}() {{
    check_gcc_dg_file({:?}, {}, {});
}}

", test_name, file_str, should_fail, should_roundtrip).unwrap();
    }
}
