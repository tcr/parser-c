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
// Cut down on number of warnings until we manage it.
#![allow(non_snake_case)]

extern crate either;
extern crate tempdir;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate parser_c_macro;

// pub mod analysis;
#[macro_use] pub mod support;
pub mod data;
pub mod parser;
pub mod system;
pub mod syntax;

use std::path::{Path, PathBuf};

use support as corollary_support;

use system::preprocess::{CppArgs, Preprocessor, isPreprocessed, runPreprocessor};
use syntax::ast::CTranslUnit;
use data::input_stream::InputStream;
use data::position::Position;
use parser::{ParseError, parseC};


pub fn parseCFile<C: Preprocessor>(cpp: C,
                                   tmp_dir_opt: Option<PathBuf>,
                                   args: Vec<String>,
                                   input_file: PathBuf)
                                   -> Result<CTranslUnit, ParseError> {
    let pos = Position::from_file(&input_file);

    let input_stream = if !isPreprocessed(&input_file) {
        let mut cpp_args = CppArgs::raw(args, input_file);
        cpp_args.cppTmpDir = tmp_dir_opt;
        match runPreprocessor(cpp, cpp_args) {
            Ok(stream) => stream,
            Err(e) => panic!("Preprocessor failed: {}", e),
        }
    } else {
        InputStream::from_file(&input_file)
    };

    parseC(input_stream, pos)
}

pub fn parseCFilePre<P: AsRef<Path>>(file: P) -> Result<CTranslUnit, ParseError> {
    let input_stream = InputStream::from_file(file.as_ref());
    parseC(input_stream, Position::from_file(file.as_ref()))
}

/// Basic public API. Accepts C source and a filename.
pub fn parse<P: AsRef<Path>>(input: &str, file: P) -> Result<CTranslUnit, ParseError> {
    // This doesn't represent possible final functionality of the crate,
    // but makes it usable at this early stage.

    let input_stream = InputStream::from_string(input.into());
    parseC(input_stream, Position::from_file(file.as_ref()))
}
