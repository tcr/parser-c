/* Different kind of identifier */
typedef int x; // x is now a typedef-ident
struct mystruct { x x; struct mystruct* y; }; // Members
x bar() {
  return  __builtin_offsetof( struct mystruct, x )
    /* avoid gcc error */
    /*    + __builtin_offsetof( struct mystruct, y[0].y[dyn()].x ); */
        + __builtin_offsetof( struct mystruct, y );

}
