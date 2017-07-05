/* Not really a bug (isn't valid C99), but a discrepancy to gcc */
int bar(int w) { return ( w ? w : w = w); } // not really an lvalue
