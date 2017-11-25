extern crate parser_c;

use std::env;

use parser_c::parseCFilePre;

fn main() {
    let mut args = env::args();
    let input_file = args.nth(1).unwrap();
    match parseCFilePre(input_file) {
        Err(e) => eprintln!("{}", e),
        Ok(unit) => if args.nth(0).as_ref().map(|x| &**x) == Some("-p") {
            println!("{:?}", unit);
        }
    }
}
