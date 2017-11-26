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
//!             panic!("error: {}", err);
//!         }
//!         Ok(ast) => {
//!             println!("success: {:?}", ast);
//!         }
//!     }
//! }
//! ```

#![feature(box_syntax, box_patterns, fnbox, vec_remove_item, i128_type,
           slice_patterns, advanced_slice_patterns, conservative_impl_trait,
           decode_utf8, ascii_ctype)]
// Cut down on number of warnings until we manage it.
#![allow(non_snake_case)]

extern crate either;
extern crate tempdir;
#[macro_use] extern crate bitflags;
#[macro_use] extern crate parser_c_macro;

// pub mod analysis;
pub mod data;
pub mod parser;
pub mod system;
pub mod syntax;
pub mod pretty;

use std::path::{Path, PathBuf};

use system::preprocess::{CppArgs, Preprocessor, isPreprocessed, runPreprocessor};
use syntax::ast::CTranslUnit;
use data::input_stream::InputStream;
use data::position::Position;
use parser::{ParseError, parseC};

/// Parse a C source file.
///
/// If the file name does not end in `.i`, use the given preprocessor
/// before parsing.
///
/// You can select a custom temporary directory if necessary.
pub fn parseCFile<C, P>(cpp: C, tmp_dir: Option<PathBuf>,
                        args: Vec<String>, input_file: P)
                        -> Result<CTranslUnit, ParseError>
    where C: Preprocessor, P: Into<PathBuf>
{
    let input_file = input_file.into();
    let pos = Position::from_file(&input_file);

    let input_stream = if !isPreprocessed(&input_file) {
        let mut cpp_args = CppArgs::raw(args, input_file);
        cpp_args.cppTmpDir = tmp_dir;
        runPreprocessor(cpp, cpp_args).map_err(ParseError::input)?
    } else {
        InputStream::from_file(&input_file)?
    };

    parseC(input_stream, pos)
}

/// Parse an already preprocessed C source file.
pub fn parseCFilePre<P: AsRef<Path>>(file: P) -> Result<CTranslUnit, ParseError> {
    let input_stream = InputStream::from_file(file.as_ref())?;
    parseC(input_stream, Position::from_file(file.as_ref()))
}

/// Basic public API. Accepts C source and a filename.
pub fn parse<P: AsRef<Path>>(input: &str, file: P) -> Result<CTranslUnit, ParseError> {
    // This doesn't represent possible final functionality of the crate,
    // but makes it usable at this early stage.

    let input_stream = InputStream::from_string(input.into());
    parseC(input_stream, Position::from_file(file.as_ref()))
}
