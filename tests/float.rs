extern crate parser_c;

use parser_c::parser::parser::parseC;
use parser_c::data::position::initPos;
use parser_c::support::FilePath;
use parser_c::support::Either::*;
use parser_c::data::input_stream::inputStreamFromString;
use std::thread;

const INPUT: &'static str = r#"
 
int main()
{
   int n, first = 0, second = 1, next, c;
 
   printf("Enter the number of terms\n");
   scanf("%d",&n);
 
   printf("First %d terms of Fibonacci series are :-\n",n);
 
   for ( c = 0 ; c < n ; c++ )
   {
      if ( c <= 1 )
         next = c;
      else
      {
         next = first + second;
         first = second;
         second = next;
      }
      printf("%d\n",next);
   }
 
   return 0;
}

"#;

#[test]
fn simple() {
    let _ = thread::Builder::new().stack_size(32 * 1024 * 1024).spawn(move || {
        let input_stream = inputStreamFromString(INPUT.to_string());

        let todo = parseC(input_stream, (initPos(FilePath::from("float.c".to_string()))));

        match todo {
            Left(err) => {
                panic!("error: {:?}", err);
            }
            Right(ast) => {
                println!("success: {:?}", ast);
            }
        }
    }).unwrap().join();
}