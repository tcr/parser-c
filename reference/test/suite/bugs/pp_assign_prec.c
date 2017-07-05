/* Pretty printer: assignment has higher precedence than comma  */
int main() {
  int b;
  int a = b, c;         /* Block Decl int */
  int y = (y,0);        /* InitExpr (y,0) */
  int x = ( (y = 3), y - 2);
  int z = ( (y ? 2 : 3) , 4 ) ;
  int u_ = ( (y = 2) ? 3 : 0 );
  int v = ( y = (2 ? 3 : 0));
  /* no longer supported by gcc */
  /* int w = ( (2 ? x : y) = x);   // Warning (not really an lvalue) */
  int s = ( 2 ? 3 : (y = x));
}
