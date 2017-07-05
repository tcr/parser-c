int x;          /* external linkage, tentative definition */
static int y;   /* internal linkage, tentative definition */
int f(int z) {
  extern int x; /* external linkage, (same object as x:1) */
  extern int y; /* internal linkage (same object as y:2) */

  /* refers to a global variable in another translation unit */
  extern int u; /* external linkage, declaration */
  /* refers to a global variable defined later in this translation unit*/
  extern int v; /* external linkage, declaration */

  static int s; /* no linkage, implicit initializer */

  register int a;
  int b;
  return x+y+u+v+s;
}
int x = 3;     /* external linkage, definition */
int v;

int main() {
  f(2);
}