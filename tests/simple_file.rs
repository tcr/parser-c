extern crate parser_c;

use parser_c::parseCFilePre;
use parser_c::support::FilePath;

#[test]
fn simple_file() {
    let input_file = FilePath {
        path: "./tests/simple_file.c".to_owned(),
    };

    let todo = parseCFilePre(input_file);

    println!("OUT {:#?}", todo);
}
