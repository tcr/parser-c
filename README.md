# parser-c

[![https://img.shields.io/crates/v/parser-c.svg](https://img.shields.io/crates/v/parser-c.svg)](http://crates.io/crates/parser-c/)

Rust module for parsing C code. Port of Haskell's [language-c](https://github.com/visq/language-c), semi-automatically translated using [Corollary](https://github.com/tcr/corrode-but-in-rust).

**This port is a work in progress.** A lot of work remains to parse anything but very simple C files; while most source code has been translated from Haskell, errors in translation prevent it from matching language-c's functionality yet. Here are the next steps for achieving parity, in order:

1. Building up an equivalent test bed to language-c's, then automatically cross-check
1. Fix errors in the ported code to support those test cases
1. Converting portions of the code into Rust idioms without breaking tests
1. Figure out a porting story for the alex/happy generated parser output

`parser-c` requires nightly (for now). See `tests/` for some working examples, or try this example:

```rust
extern crate parser_c;

use parser_c::parse;

const INPUT: &'static str = r#"

int main() {
    printf("hello world!\n");
    return 0;
}

"#;

fn main() {
    match parse(INPUT, "simple.c") {
        Err(err) => {
            panic!("error: {:?}", err);
        }
        Ok(ast) => {
            println!("success: {:#?}", ast);
        }
    }
}
```

Result is:

```
success: Right(
    CTranslationUnit(
        [
            CFDefExt(
                CFunctionDef(
                    [
                        CTypeSpec(
                            CIntType(
                                ..
                            )
                        )
                    ],
                    CDeclarator(
                        Some(
                            Ident(
                                "main",
                                124382170,
                                ..
                            )
                        ),
                        ...
```

## License

MIT or Apache-2.0, at your option.
