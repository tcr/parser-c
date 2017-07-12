//! Very work-in-progress C parser, ported from Haskell's language-c.
//!
//! ```rust,no_run
//! extern crate parser_c;
//!
//! use parser_c::parse;
//!
//! const INPUT: &'static str = r#"
//!
//! int main() {
//!     printf("hello world!");
//!     return 0;
//! }
//!
//! "#;
//!
//! fn main() {
//!     match parse(INPUT, "simple.c") {
//!         Err(err) => {
//!             panic!("error: {:?}", err);
//!         }
//!         Ok(ast) => {
//!             println!("success: {:?}", ast);
//!         }
//!     }
//! }
//! ```

#![feature(box_syntax, box_patterns, fnbox)]
#![allow(unused_parens)]
// Cut down on number of warnings until we manage it.
#![allow(non_snake_case, non_camel_case_types, unused_imports, unused_variables)]

extern crate num;
extern crate either;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate num_derive;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate parser_c_macro;

// pub mod analysis;
#[macro_use] pub mod support;
pub mod data;
pub mod parser;
pub mod syntax;

use std::thread;

use support as corollary_support;
use corollary_support::FilePath;
use syntax::preprocess::{Preprocessor, isPreprocessed, runPreprocessor, rawCppArgs};
use syntax::ast::CTranslUnit;
use data::input_stream::InputStream;
use data::position::Position;
use parser::{ParseError, parseC};


pub fn parseCFile<C: Preprocessor>(cpp: C,
                                   tmp_dir_opt: Option<FilePath>,
                                   args: Vec<String>,
                                   input_file: FilePath)
                                   -> Result<CTranslUnit, ParseError> {

    let handleCppError = |_0| match (_0) {
        Err(exitCode) => panic!("Preprocessor failed with {:?}", exitCode),
        Ok(ok) => ok,
    };

    let input_stream = if !isPreprocessed(&input_file.path) {
        let cpp_args = __assign!((rawCppArgs(args, input_file.clone())), {
            cppTmpDir: tmp_dir_opt
        });

        handleCppError(runPreprocessor(cpp, cpp_args))
    } else {
        InputStream::from_file(&input_file)
    };

    parseC(input_stream, Position::from_file(input_file))
}

pub fn parseCFilePre(file: FilePath) -> Result<CTranslUnit, ParseError> {
    thread::Builder::new().stack_size(32 * 1024 * 1024).spawn(move || {
        let input_stream = InputStream::from_file(&file);
        parseC(input_stream, Position::from_file(file))
    }).unwrap().join().unwrap()
}

/// Basic public API. Accepts C source and a filename.
pub fn parse(input: &str, filename: &str) -> Result<CTranslUnit, ParseError> {
    // This doesn't represent possible final functionality of the crate,
    // but makes it usable at this early stage.

    let input = input.to_string();
    let filename = filename.to_string();

    // TODO which stack size is necessary? Can we eliminate this?
    thread::Builder::new().stack_size(32 * 1024 * 1024).spawn(move || {
        let input_stream = InputStream::from_string(input);

        parseC(input_stream, Position::from_file(FilePath::from(filename)))
    }).unwrap().join().unwrap()
}
