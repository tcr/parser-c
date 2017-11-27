extern crate parser_c;

use parser_c::parse_file_pre;

#[test]
fn simple_file() {
    let input_file = "./tests/simple_file.c";
    let todo = parse_file_pre(input_file);

    println!("OUT {:#?}", todo);
}
