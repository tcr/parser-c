int x; /* file scope */
int f() { /* file scope */
  int x; /* block-1 scope */
  while(x<3) {
    double x=4; /* block-2 scope */
  }
}