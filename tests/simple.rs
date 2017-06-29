extern crate parser_c;

use parser_c::parser::parser::parseC;
use parser_c::data::position::initPos;
use parser_c::support::FilePath;
use parser_c::support::Either::*;
use parser_c::data::input_stream::inputStreamFromString;

const INPUT: &'static str = r#"

int main() {
    return 0;
}

"#;

#[test]
fn simple() {
    let input_stream = inputStreamFromString(INPUT.to_string());

    let todo = parseC(input_stream, (initPos(FilePath::from("simple.c".to_string()))));

    match todo {
        Left(err) => {
            panic!("error: {:?}", err);
        }
        Right(ast) => {
            println!("success: {:?}", ast);
        }
    }
}