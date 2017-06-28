# parser-c

Rust module to parse C code. Port of Haskell's language-C to Rust. WIP

```rust
extern crate parser_c;

use parser_c::parser::parser::parseC;
use parser_c::data::position::initPos;
use parser_c::support::FilePath;
use parser_c::data::input_stream::readInputStream;

fn main() {
    let input_file = FilePath {
        path: "./tests/simple.c".to_owned(),
    };
    let input_stream = readInputStream(input_file.clone());

    let todo = parseC(input_stream, (initPos(input_file)));

    println!("Parsed code: {:#?}", todo);
}
```

`simple.c` is:

```c
int main() {
    return 0;
}
```

## License

MIT or Apache-2.0, at your option.