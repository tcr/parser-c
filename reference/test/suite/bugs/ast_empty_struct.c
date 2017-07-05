/* 20080614: In a definition ... */
struct c;      /* .. is a forward decl. */
struct c { };  /* .. is an empty struct def. */
/* They must not be represented using the same AST */