/* Note: It is incorrect to drop the identifier list in an old-style function declaration 
  (it determines the order of the arguments) */
long 
foo1(y,x)
  register int x;
  register long y;
{
  return x-y;
}

long 
foo2(x,y)
  register int x;
  register long y;
{
  return x-y;
}
int main() {
  printf("%d // %d\n", foo1(2,3), foo2(2,3));
}