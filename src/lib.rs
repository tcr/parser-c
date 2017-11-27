//! Very work-in-progress C parser, ported from Haskell's language-c.
//!
//! ```rust,no_run
//! extern crate parser_c;
//!
//! use parser_c::parse_str;
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
//!     match parse_str(INPUT, "simple.c") {
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
           decode_utf8, ascii_ctype, from_ref)]

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

use system::preprocess::{CppArgs, Preprocessor, is_preprocessed, run_preprocessor};
use syntax::ast::CTranslUnit;
use data::input_stream::InputStream;
use data::position::Position;
use parser::{ParseError, parse};

/// Parse a C source file.
///
/// If the file name does not end in `.i`, use the given preprocessor
/// before parsing.
///
/// You can select a custom temporary directory if necessary.
pub fn parse_file<C, P>(cpp: C, tmp_dir: Option<PathBuf>,
                        args: Vec<String>, input_file: P)
                        -> Result<CTranslUnit, ParseError>
    where C: Preprocessor, P: Into<PathBuf>
{
    let input_file = input_file.into();
    let pos = Position::from_file(&input_file);

    let input_stream = if !is_preprocessed(&input_file) {
        let mut cpp_args = CppArgs::raw(args, input_file);
        cpp_args.cpp_tmp_dir = tmp_dir;
        run_preprocessor(cpp, cpp_args).map_err(ParseError::input)?
    } else {
        InputStream::from_file(&input_file)?
    };

    parse(input_stream, pos)
}

/// Parse an already preprocessed C source file.
pub fn parse_file_pre<P: AsRef<Path>>(file: P) -> Result<CTranslUnit, ParseError> {
    let input_stream = InputStream::from_file(file.as_ref())?;
    parse(input_stream, Position::from_file(file.as_ref()))
}

/// Basic public API. Accepts C source and a filename.
pub fn parse_str<P: AsRef<Path>>(input: &str, file: P) -> Result<CTranslUnit, ParseError> {
    // This doesn't represent possible final functionality of the crate,
    // but makes it usable at this early stage.

    let input_stream = InputStream::from_string(input.into());
    parse(input_stream, Position::from_file(file.as_ref()))
}
