/* Yes ! The first 'bug' I found in CIL :) */
int x __attribute__((deprecated));
struct s0 { int x __attribute__((deprecated)); };
struct s { int x; } __attribute__((packed)) 
const __attribute__((deprecated)) S_CONST = { 3 };
struct t { int x; } f() __attribute__((deprecated)), g();
struct u1 { int x; } (__attribute__((deprecated)) h)(void);
struct u2 { int y; } i(void) __attribute__((deprecated));
struct u3 { int y; } j __attribute__((deprecated));
int main() { f(); g(); return S_CONST.x; }
/* Expected -Wall warnings: S_CONST is deprecated, f() is deprecated */
