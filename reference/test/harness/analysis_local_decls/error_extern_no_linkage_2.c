int f(int z) {
  static int s; /* no linkage, implicit initializer */

  /* Error, s has no linkage */
  extern int s; 
  return z+s;
}
