/* _Noreturn */
inline void f(void);
inline void nf(void);
static _Noreturn void nnf(void);
__attribute__((noreturn)) void nnnf(void);

inline void f(void)
{
}
_Noreturn inline void nf(void)
{
    while (1)
    {
    }
}
static _Noreturn void nnf(void)
{
    while (1)
    {
    }
}


