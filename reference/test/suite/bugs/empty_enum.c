/* This also _might_ indicate a principle design bug in the AST : 
 * The decl `enum empty_enum;`
 * and the first parameter of `void foo(enum empty_enum);` have the same AST.
 * Not sure yet.
 */
enum empty_enum;
void foo (enum empty_enum);
enum non_empty { E1, E2 = 3 };
void bar (enum non_empty);
