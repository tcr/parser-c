extern crate parser_c;

use parser_c::parseCFilePre;

#[test]
fn simple_file() {
    let input_file = "./tests/simple_file.c";
    let todo = parseCFilePre(input_file);

    println!("OUT {:#?}", todo);
}
