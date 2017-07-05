/* paragraph 6 */
void __t() {
  label1: __attribute__((unused))
  return;  
}

/* paragraph 7 */
struct __attribute__((unused)) s_1 { int m_1; } ;
struct s_2 { int m_2; } __attribute__((unused)) a;

/* p8-11: any list of specifiers and qualifiers at the start of a declaration
   may contain attribute specifiers. */

/* Not supported */
// __attribute__((deprecated)) l_1;             /* Defaults to int */

typedef struct s_1 S_1;
register __attribute__((deprecated)) l_2;
const __attribute__((deprecated))  l_4;
long __attribute__((deprecated))  l_6;
struct s_1 __attribute__((deprecated))  l_8;
S_1 __attribute__((deprecated))  l_10;
__attribute__((deprecated)) register l_3;
__attribute__((deprecated)) const l_5;
__attribute__((deprecated)) long l_7;
__attribute__((deprecated)) struct s_1 l_9;
__attribute__((deprecated)) S_1 l_11;

/* applies to all declarations */
static __attribute__((unused)) int *l_12_a, l_12_b(), *l_12_c;    
__attribute__((unused)) unsigned *l_13_a, l_13_b(), *l_13_c;    
const __attribute__((deprecated)) long __attribute__((deprecated)) long l_14;


/* Applies to decl */
__attribute__((noreturn)) int f_1_a_1();  
int __attribute__((noreturn)) f_1_a_2();  

/* Applies to declarator */
int __p, __attribute__((noreturn)) f_1_b();  

/* deprecated applies to all, noreturn only to e */
int 
  __attribute__((deprecated)) f_1_c(),
                              f_1_d(),
  __attribute__((noreturn))   f_1_e();

/* GCC test */
int f_1_c() { return f_1_d();  }; /* deprecation warning  */
int f_1_d() { return 2;  };       /* ok  */
int f_1_e() { return 3; };       /* warning: no-return returns */

int f_2() __attribute__((noreturn));  /* FunDeclr */

/* p16: belongs to preceeding declarator */

/* For illustration, the semantics of the const qualifier */

/* pointer to constant char  */
char const * c_1 = "abcde"; 
void mod_c_1_a() { c_1++; }     /* ok */
void mod_c_1_b() { (*c_1)++; } /* not ok */
/* constant pointer to char  */
char * const c_2;
void mod_c_2_a() { c_2++; }     /* not ok */
void mod_c_2_b() { (*c_2)++; } /* ok */

/* constant pointer to constant char */
char const * const c_3;
void mod_c_3_a() { c_3++; }     /* not ok */
void mod_c_3_b() { (*c_3)++; } /*  not ok */

/* using a (pseudo) attribute */
char * c_4_a;
char * __attribute__((const)) c_4_b;
char __attribute__((const)) * __attribute__((const)) c_4_c;
__attribute__((const)) char * __attribute__((const)) 
  c_4_d __asm__("c_4_" "$$" "b") __attribute((const));


/* constant pointer to a 0-ary function returning a pointer to constant char */
char const * (* const c_5) (void);
void mod_c_5_a() { c_5++; }     /* not ok */
void mod_c_5_c() { (*c_5())++; }  /* not ok */

/* From the GNU examples:
    pointer to non-returning function returning void */
void (__attribute__((noreturn)) *f) (void); 

/* A function declarator has the form   (attrs outer_declr)(params)   
                                  or    f (params) */


/* This would be a pointer to a 0-ary function returning a packed s_1 */
struct s_1 __attribute__((packed)) (*f) (void);

int __attribute__((packed)) g_1_a;

int (__attribute__((packed))  g_1);

int (__attribute__((packed)) *g_2) (void); 

/* pointer to non-returning function returning void */
void (__attribute__((noreturn)) *f) (void); 
/* y is a packed pointer to a packed pointer to a non-returning function returning int */
int x, 
  __attribute__((packed)) /* applies to `the next identifier',i.e. y */ 
   ( __attribute__((noreturn)) /* applies to the function */
    * __attribute__((packed))  /* applies to the function pointer */ 
    *
    *
    *y) () = 3;
/* A packed, non-aliased pointer to a packed, constant pointer to an
   function that doesn't return int */
void ( __attribute__((noreturn))
     * const __attribute__((packed))      
     * __restrict __attribute__((packed)) 
     fp) (void); 

/* p12: attribute specifier may appear imm. before the comma, = or semicolon
   terminating the declaration of an identifier.
   Such attributes apply to the declared object or function 
   The attribute must follow assembler names */
__attribute__((deprecated)) int i_1;
int i_2 __attribute__((deprecated));
void t_1() { if(l_1 > 0) ; }                /* warning: deprecation */

void foo() __attribute__((noreturn, noreturn))
           __attribute__((noreturn));
void * bar() __attribute__((noreturn));

int (__attribute__((noreturn)) function) (a, b)
     int a;
     int b;
{
  return a + b;
}

/* GNU example enumerator attributes */
enum E {
  oldval __attribute__((deprecated)),
  newval
};
