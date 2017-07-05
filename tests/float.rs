extern crate parser_c;

use parser_c::parse;

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
fn float() {
    match parse(INPUT, "float.c") {
        Err(err) => {
            panic!("error: {:?}", err);
        }
        Ok(ast) => {
            println!("success: {:?}", ast);
        }
    }
}