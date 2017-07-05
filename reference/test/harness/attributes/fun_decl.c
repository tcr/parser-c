/* function attributes for old-style function declarations are special
   - if we move the attribute to the right of the declarator, we get a syntax error */
#define DEPR __attribute__((deprecated))
#define CONST __attribute__((const))
#define UNUSED __attribute__((unused))
/* a pointer to a deprecated function returning int - does not work in current gcc ! */
extern int (DEPR *f_0_a)(int x);

/* this applies to the function prototype */
extern int f_0_b(int x) DEPR;

/* new style  */ 
static int (CONST f_1)(int x) { return (*f_0_a)(x)+f_0_b(x); }
/* this should be ok, but is a syntax error in current gcc */

/* static int f_2(int x) UNUSED   { return x; } */

/* old-style */
static int (CONST f_3)(int x) { return x; }

/* Below: according to the gcc docs, DEPR might belong to f_3 in future implementations, */
/* but this makes the grammar even more tricky. currently it is a syntax error */
/*
static int f_4(x) UNUSED
  int x; 
  {
    return x;
  }
*/
