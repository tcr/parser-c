// Original file: "Parser.hs"
// File auto-generated using Corollary.

#[macro_use]
use corollary_support::*;

pub mod builtin;
pub mod lexer;
pub mod parser_monad;
pub mod parser;
pub mod tokens;

use parser::parser_monad::{P, Parser, execParser};
use parser::builtin::*;
use data::name::new_name_supply;
use parser::parser_monad::ParseError;
use data::position::Position;
use data::input_stream::InputStream;

pub fn execParser_<T, F>(do_parse: F, input: InputStream, pos: Position) -> Result<T, ParseError>
    where T: 'static, F: Fn(&mut Parser) -> P<T>
{
    execParser(input, pos, builtinTypeNames(), new_name_supply(), do_parse).map(|x| x.0)
}
