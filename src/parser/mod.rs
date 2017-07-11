// Original file: "Parser.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

pub mod builtin;
pub mod lexer;
pub mod parser;
pub mod parser_utils;
pub mod tokens;

use parser::parser::{Parser, translUnitP};
use parser::parser_utils::{ParseError, execParser};
use parser::builtin::*;
use syntax::ast::CTranslUnit;
use data::name::new_name_supply;
use data::position::Position;
use data::input_stream::InputStream;

pub fn parseC(input: InputStream, initialPosition: Position) -> Result<CTranslUnit, ParseError> {
    execParser(input, initialPosition, builtinTypeNames(), new_name_supply(), translUnitP).map(|x| x.0)
}

pub fn execParser_<T, F>(do_parse: F, input: InputStream, pos: Position) -> Result<T, ParseError>
    where T: 'static, F: Fn(&mut Parser) -> Result<T, ParseError>
{
    execParser(input, pos, builtinTypeNames(), new_name_supply(), do_parse).map(|x| x.0)
}
