/* x will be a typdef at this point */
typedef struct point { int x; int y; } x; 
int foo() {
  struct point y = (struct point) { .x = 2, .y = 3 };
  x z = y; 
  int x_off = __builtin_offsetof( struct point , x );
  int x_off_2 = __builtin_offsetof( x , x );
}