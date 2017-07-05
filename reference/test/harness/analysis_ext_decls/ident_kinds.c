enum { e1 = 4 } xe1;
typedef enum E2 { e2 = e1, e3 } ENUM2;
extern int a;        /* obj decl, static external */
static long long b;  /* obj tentative def, static internal */
long c = 4;          /* obj def, static external */
static int f1();      /* fun decl f1?, static internal */
extern int f1(void) { return 0; }  /* fun def f1(), static internal !! */
extern int f2(void);  /* fun decl f2(), static external */
static int g(char**); /* fun decl (static) */
int g(char** a) { return 0; } /* fun def, static internal !!  */
int export() { return f1()+b+f2()+g(0); } /* fun def, external */