extern crate parser_c;

use parser_c::parse_str;

const INPUT: &'static str = r#"

int main() {
    printf("hello world!\n");
    return 0;
}

"#;

#[test]
fn simple() {
    match parse_str(INPUT, "simple.c") {
        Err(err) => {
            panic!("error: {:?}", err);
        }
        Ok(ast) => {
            println!("success: {:?}", ast);
        }
    }
}
