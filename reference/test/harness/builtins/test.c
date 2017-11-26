/* Builtin regression tests */

/* (1) __builtin_convertvector */
typedef double vector4double __attribute__((__vector_size__(32)));
typedef float vector4float __attribute__((__vector_size__(16)));
typedef short vector4short __attribute__((__vector_size__(8)));
vector4float vf;
vector4short vs;
vector4double vd;

int test(void)
{
    vf = __builtin_convertvector (vs, vector4float);
    vd = (vector4double) {(double) vf[0], (double) vf[1], (double) vf[2], (double) vf[3]};
}
