#define D __attribute__((deprecated))
int x_1,x_2 D;
int x_3 D, x_4;
int x_5, D x_6;
int D x_7, x_8;
struct { int a; } D x_9, x_10;
D struct { int a; } x_11, x_12;
struct { int a; } volatile D x_13, x_14;
struct {
  int a;
  struct { int a_1; } D x_16, x_17;
  D struct { int a_2; } x_18, x_19;
  struct { int a_3; } const D x_20, x_21;  
  struct { int a_4; } x_22 D, x_23;
  struct { int a_5; } x_24, D x_25;
  struct { int a_6; } x_26, x_27 D;
  } x_15;
enum E {
  ev_0     = 0,
  ev_1 D   = 1,
  ev_2 D D = 2,
  ev_3,
  ev_4 D,
  ev_5 D D
};
int main() {
  /* x_2, x_3, x_6 */
  return x_1+x_2+x_3+x_4+x_5+x_6+x_7+x_8+x_9.a+x_10.a+x_11.a+x_12.a
         + x_13.a + x_14.a + x_15.x_16.a_1 + x_15.x_17.a_1 +  x_15.x_18.a_2 + x_15.x_19.a_2 + 
         x_15.x_20.a_3 + x_15.x_21.a_3 + x_15.x_22.a_4 + x_15.x_23.a_4 + x_15.x_24.a_5 + x_15.x_25.a_5
         + x_15.x_26.a_6 + x_15.x_27.a_6; // + x_15.x_28.a;
}
