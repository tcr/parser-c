typedef struct _s {
    int x;
    union {
        int y;
        int z;
    };
} s;

s s1 = {1, 2};

s s2 = { .x = 1, .y = 2 };

s s3 = { .x = 1, .z = 2 };
