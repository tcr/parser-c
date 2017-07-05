/* Non-Standard attribute syntax used in OS X (no proper support, but should parse) */
int f1(char ** restrict) __attribute__((availability(macosx,introduced=10.7)));
