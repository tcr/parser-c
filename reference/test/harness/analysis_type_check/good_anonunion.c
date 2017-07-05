typedef struct foo {
    int x;
    union {
        int y;
        int z;
    };
} s;

int f(s *ps) {
    return ps->y;
}
