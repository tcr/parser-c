extern crate parser_c;

use std::env;

use parser_c::parser::exec_parser_simple;
use parser_c::parser::lexer::lex;
use parser_c::data::input_stream::InputStream;
use parser_c::data::position::Position;
use parser_c::parser::tokens::CTokEof;

fn main() {
    let mut args = env::args();
    let input_file = args.nth(1).unwrap();
    let dump = args.nth(0).as_ref().map(|x| &**x) == Some("-d");
    let input_stream = InputStream::from_file(&input_file).unwrap();
    let init_pos = Position::from_file(&input_file);
    let res = exec_parser_simple(|p| loop {
        let tok = lex(p)?;
        if dump {
            println!("{:?}", tok);
        }
        if let CTokEof = tok {
            return Ok(());
        }
    }, input_stream, init_pos);
    if let Err(e) = res {
        eprintln!("{}", e);
    }
}
