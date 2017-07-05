int foo() {
  int x = 5;
  void *p;
c1:
  x--;
  goto c3;
c2:
  x = 4;
  goto c4;
c3:
  p = (x > 3) ? &&c1 : &&c2;  
  goto *(p + 1);
c4: 
  goto c1;
}
