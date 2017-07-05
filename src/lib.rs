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

// Original file: "C.hs"

#![feature(proc_macro)]
#![feature(slice_patterns, box_syntax, box_patterns, fnbox)]
#![allow(unused_parens)]
#![recursion_limit="500"]

extern crate num;
#[macro_use] extern crate matches;
#[macro_use] extern crate num_derive;
extern crate parser_c_macro;
#[macro_use] extern crate lazy_static;

// pub mod analysis;
#[macro_use] pub mod support;
pub mod data;
pub mod parser;
pub mod syntax;

use support as corollary_support;
use corollary_support::*;
use syntax::preprocess::*;
use syntax::ast::*;
use data::input_stream::readInputStream;
use data::position::initPos;
use parser::parser_monad::ParseError;
use parser::parser::parseC;
use data::input_stream::inputStreamFromString;

// NOTE: These imports are advisory. You probably need to change them to support Rust.
// use Language::C::Data;
// use Language::C::Syntax;
// use Language::C::Pretty;
// use Language::C::Parser;
// use Language::C::System::Preprocess;

fn parseCFile<C: Preprocessor>(cpp: C,
                                   tmp_dir_opt: Option<FilePath>,
                                   args: Vec<String>,
                                   input_file: FilePath)
                                   -> Either<ParseError, CTranslUnit> {

    let handleCppError = |_0| match (_0) {
        Left(exitCode) => __error!(__op_addadd("Preprocessor failed with ".to_string(), show(exitCode))),
        Right(ok) => ok,
    };

    /*do*/
    {
        let input_stream = if !isPreprocessed(input_file.clone().into()) {
            {
                let cpp_args = __assign!((rawCppArgs(args, input_file.clone())), {
                    cppTmpDir: tmp_dir_opt
                });

                handleCppError(runPreprocessor(cpp, cpp_args))
            }
        } else {
            readInputStream(input_file.clone())
        };

        parseC(input_stream, (initPos(input_file)))
    }
}

fn parseCFilePre(file: FilePath) -> Either<ParseError, CTranslUnit> {
    /*do*/
    {
        let input_stream = readInputStream(file.clone());

        parseC(input_stream, (initPos(file)))
    }
}

/// Basic public API. Accepts C source and a filename.
pub fn parse(input: &str, filename: &str) -> Result<CTranslUnit, ParseError> {
    // This doesn't represent possible final functionality of the crate,
    // but makes it usable at this early stage.

    use std::thread;

    let input = input.to_string();
    let filename = filename.to_string();

    // TODO which stack size is necessary? Can we eliminate this?
    thread::Builder::new().stack_size(32 * 1024 * 1024).spawn(move || {
        let input_stream = inputStreamFromString(input);

        let todo = parseC(input_stream, (initPos(FilePath::from(filename))));

        match todo {
            Left(err) => {
                Err(err)
            }
            Right(ast) => {
                Ok(ast)
            }
        }
    }).unwrap().join().unwrap()
}