# parser-c

Rust module for parsing C code. Port of Haskell's [language-c](https://github.com/visq/language-c), semi-automatically translated using [Corollary](https://github.com/tcr/corrode-but-in-rust).

A lot of work remains:

* Building up an equivalent test bed to language-c's, then automatically cross-check
* Fix errors in the ported code
* Figure out a porting story for alex/happy generated parser
* Converting portions of the code into Rust idioms

Example:

```rust
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
```

Result is:

```
OUT Right(
    CTranslationUnit(
        [
            CFDefExt(
                CFunctionDef(
                    [
                        CTypeSpec(
                            CIntType(
                                NodeInfo(
                                    Position {
                                        posOffset: 0,
                                        posFile: "./tests/simple.c",
                                        posRow: 1,
                                        posColumn: 1
                                    },
                                    (
                                        Position {
                                            posOffset: 0,
                                            posFile: "./tests/simple.c",
                                            posRow: 1,
                                            posColumn: 1
                                        },
                                        3
                                    ),
                                    Name(
                                        1
                                    )
                                )
                            )
                        )
                    ],
                    CDeclarator(
                        Some(
                            Ident(
                                "main",
                                124382170,
                                ...
```

## License

MIT or Apache-2.0, at your option.