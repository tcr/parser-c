extern crate parser_c;

use std::any::Any;
use std::path::Path;
use std::fs::File;
use std::io::Read;

use parser_c::{parse_str, parse_file_pre};
use parser_c::pretty::pretty_to_string;
use parser_c::syntax::ast::{Equiv, Traverse, CStat, CCompound, CBlockStmt};

/// Replace single-statement CCompound statements by the single statement.
fn normalize(node: &mut Any) {
    if let Some(stat) = node.downcast_mut::<CStat>() {
        let mut new_stat = None;
        if let CCompound(ref labels, ref mut stmts, _) = *stat {
            if labels.is_empty() && stmts.len() == 1 {
                if let CBlockStmt(_) = stmts[0] {
                    let stmtitem = stmts.pop().unwrap();
                    if let CBlockStmt(stmt) = stmtitem {
                        new_stat = Some(stmt);
                    }
                }
            }
        }
        if let Some(new_stat) = new_stat {
            *stat = new_stat;
        }
    }
}

fn check_gcc_dg_file(file_str: &str, should_fail: bool) {
    let path = Path::new("./gcc_pre").join(file_str);
    let mut ast = match parse_file_pre(&path) {
        Ok(mut ast) => if !should_fail {
            ast
        } else {
            panic!("*** Parse unexpectedly successful");
        },
        Err(e) => if should_fail {
            return;
        } else {
            panic!("*** Parse fail: {}", e);
        }
    };
    let pretty = pretty_to_string(&ast);
    let mut ast2 = match parse_str(&pretty, file_str) {
        Ok(mut ast) => ast,
        Err(e) => {
            let mut s = String::new();
            File::open(&path).unwrap().read_to_string(&mut s).unwrap();
            println!("--- Original source:");
            println!("{}", s);
            println!("--- Pretty-printed source:");
            println!("{}", pretty);
            println!("---");
            panic!("*** Parse pretty-printed source fail: {}", e);
        }
    };
    ast.traverse(&normalize);
    ast2.traverse(&normalize);
    if !ast.equiv(&ast2) {
        let mut s = String::new();
        File::open(&path).unwrap().read_to_string(&mut s).unwrap();
        println!("--- Original source:");
        println!("{}", s);
        println!("--- Pretty-print of original AST:");
        println!("{}", pretty_to_string(&ast));
        println!("--- Pretty-print of reparsed AST:");
        println!("{}", pretty_to_string(&ast2));
        println!("---");
        println!("*** Parsed ASTs are not equivalent");
    }
}

include!(concat!(env!("OUT_DIR"), "/gcc_dg_tests.rs"));
