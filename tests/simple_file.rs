extern crate parser_c;

use parser_c::parser::parser::parseC;
use parser_c::data::position::initPos;
use parser_c::support::FilePath;
use parser_c::data::input_stream::readInputStream;

#[test]
fn simple() {
    let input_file = FilePath {
        path: "./tests/simple_file.c".to_owned(),
    };
    let input_stream = readInputStream(input_file.clone());

    let todo = parseC(input_stream, (initPos(input_file)));

    println!("OUT {:#?}", todo);
}