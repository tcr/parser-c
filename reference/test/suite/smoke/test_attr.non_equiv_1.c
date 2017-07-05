int my_wait(int *) __asm("_" "wait" "$UNIX2003");
typedef int bar;
int foo() {
  return 2+ (bar) 3.0;
}
