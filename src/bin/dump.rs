extern crate parser_c;

use std::env;

use parser_c::parseCFilePre;
use parser_c::support::FilePath;

fn main() {
    let mut args = env::args();
    let input_file = FilePath { path: args.nth(1).unwrap() };
    let out = parseCFilePre(input_file);
    if args.nth(0).as_ref().map(|x| &**x) == Some("-p") {
        println!("{:#?}", out);
    }
}
