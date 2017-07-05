int f(int z) {
  /* Both extern keyword and initializer is an error */
  extern int z2 = 3; /* error */
  return z+z2;
}
