typedef struct _s {
    int x;
    int y;
} s;

s s1 = { 1, 2 };
s s2 = { .x = 1, .y = 2};
s s3 = { .y = 1, .x = 2};
s s4 = { .x = 1, 2};
s s5 = { 1, .y = 2};
