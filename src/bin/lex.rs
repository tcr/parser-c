extern crate parser_c;

use std::env;

use parser_c::parser::execParser_;
use parser_c::parser::lexer::lexC;
use parser_c::data::input_stream::InputStream;
use parser_c::data::position::Position;
use parser_c::parser::tokens::CTokEof;

fn main() {
    let mut args = env::args();
    let input_file = args.nth(1).unwrap();
    let input_stream = InputStream::from_file(&input_file);
    let init_pos = Position::from_file(&input_file);
    let res = execParser_(|p| loop {
        if let CTokEof = lexC(p)? {
            return Ok(());
        }
    }, input_stream, init_pos);
    if let Err(e) = res {
        eprintln!("{}", e);
    }
}
