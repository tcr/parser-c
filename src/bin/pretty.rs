extern crate parser_c;

use std::io::{self, BufWriter};
use std::env;

use parser_c::parse_file;
use parser_c::system::gcc;
use parser_c::pretty::pretty;

fn main() {
    let mut args: Vec<_> = env::args().skip(1).collect();
    if let Some(input_file) = args.pop() {
        let preprocessor = gcc::GCC::new("gcc");
        match parse_file(preprocessor, None, args, input_file) {
            Err(e) => eprintln!("{}", e),
            Ok(unit) => {
                let out = io::stdout();
                let mut writer = BufWriter::new(out);
                pretty(&unit, &mut writer);
            }
        }
    } else {
        eprintln!("usage: pretty [CPP_OPT ...] inputfile");
    }
}
