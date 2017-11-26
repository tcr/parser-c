extern crate parser_c;

use std::env;

use parser_c::parseCFile;
use parser_c::system::gcc;

fn main() {
    let mut args: Vec<_> = env::args().skip(1).collect();
    let mut dump = false;
    if let Some("-d") = args.last().map(|v| &**v) {
        args.pop();
        dump = true;
    }
    if let Some(input_file) = args.pop() {
        let preprocessor = gcc::newGCC("gcc");
        match parseCFile(preprocessor, None, args, input_file) {
            Err(e) => eprintln!("{}", e),
            Ok(unit) => if dump { println!("{:?}", unit) },
        }
    } else {
        eprintln!("usage: parse [CPP_OPT ...] inputfile [-d]");
    }
}
