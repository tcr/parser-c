// cargo-deps: command-macros="0.1" error-chain="*" regex="*" maplit="*"

//! Description

#[macro_use] extern crate command_macros;
#[macro_use] extern crate error_chain;
#[macro_use] extern crate maplit;
extern crate regex;

use regex::{Captures, Regex};

// Error chain
mod errors {
    error_chain! {
        foreign_links {
            Io(::std::io::Error);
        }
    }
}
use errors::*;

quick_main!(run);

fn select(input: &str, rev: bool) -> String {
    let mut j = 0;
    let mut first = true;
    let mut matcher = hashmap![];
    let idx = if rev { -1 } else { 1 };
    for c in input.chars() {
        match c {
            '(' | '[' | '{' => {
                *matcher.entry(c).or_insert(0 as isize) += idx;
            }
            ')' | ']' | '}' => {
                *matcher.entry(c).or_insert(0) -= idx;
            }
            _ => {}
        }
        if !first {
            if matcher.values().map(|x| *x).sum::<isize>() == 0 {
                break;
            }
        }
        j += 1;
        first = false;
    }
    if matcher.values().map(|x| *x).sum::<isize>() != 0 {
        panic!("unbalanced");
    }
    input.chars().take(j + 1).collect()
}

fn run() -> Result<()> {
    // let stat = cmd!( ls ).status().unwrap();
    // println!("ls {:?}", stat);

    use std::fs::File;
    use std::io::Read;
    let mut r_str = String::new();
    File::open("src/parser/parser.rs")?.read_to_string(&mut r_str)?;

    // let r_str = "HappyStk(a, box HappyStk(b, box rest))    ))()";

    let mut r = r_str.chars().collect::<Vec<_>>();
    let mut s = String::new();
    let mut i = 0;
    while i < r.len() {
        s.push(r[i]);
        if s.ends_with("HappyStk(") {
            let rest: String = r[i..].iter().map(|x| *x).collect();
            let mut span = select(&rest, false);

            // Do magic on the span
            let naps = span.chars().rev().collect::<String>();
            // println!("naps {:?}, naps", naps);
            let mut c = match naps.chars().nth(1).unwrap() {
                ')' | ']' | '}' => {
                    let mut span2 = select(&naps[1..], true);
                    // println!("OK {:?}", span2.chars().rev().collect::<String>());
                    let l = span.len() - span2.len();
                    span.chars().take(l).collect::<String>().rfind(',')
                }
                _ => {
                    span.rfind(',')
                }
            }.unwrap();

            while span.chars().nth(c + 1).unwrap() == ' ' {
                c += 1;
            }

            let mut j = span[0..c+1].to_string();
            j.push_str("Some(");
            j.push_str(&span[c + 1..]);
            j.push_str(")");

            let rest_rest = &rest[span.len()..];
            j.push_str(rest_rest);
            r.truncate(i);
            r.extend(j.chars().collect::<Vec<_>>());
        }
        i += 1;
    }

    println!("{}", s);

    // let s = Regex::new(r#"HappyStk\("#).unwrap().replace_all(r, |cap: &Captures| {
    //     println!("{:?}", cap);
    //     cap[0].to_string()
    // });
    // println!("S {:?}", s);


    // let sub_r = r.char_indices().skip(8).next().unwrap().0;

    // println!("j {}", select(&r[sub_r..]));

    Ok(())
}