extern crate parser_c;

use parser_c::parse;

const INPUT: &'static str = r#"

int main() {
    return 0;
}

"#;

#[test]
fn simple() {
    match parse(INPUT, "simple.c") {
        Err(err) => {
            panic!("error: {:?}", err);
        }
        Ok(ast) => {
            println!("success: {:?}", ast);
        }
    }
}