/* #include <stdio.h> */
int printf(const char * restrict format, ...);

/* Test if-else pretty-printing */
int main () {
  int inp = 0;
  
  if(inp == 0) inp=1;
  else         inp=2;
  
  if(inp == 0) {
    inp = 2;
    inp = 3;
  }
  
  if(inp == 0) {
    inp=1;
  } 
  else inp=2;
  
  if(0 == inp) {
    if(1 == inp) ;
    else if(2 == inp) ;
    else if(3 == inp) {
      if(3 == inp) {
        ;
      } else if(4 == inp) ;
      else ;
    }
  }
  else if (1 == inp) { } 
  else if (2 == inp) { } 
  else if (3 == inp) { printf("inp=3\n"); } 
  else if (4 == inp) { } 
  else if (5 == inp) { } 
  else if (6 == inp) { }
}
