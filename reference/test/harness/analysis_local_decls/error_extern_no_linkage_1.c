int f(int z) {
  /* There is no z with internal or external linkage visible, so this is
     a new declaration conflicting with the parameter z */
  extern int z;  /* error */
  return z+1;
}
