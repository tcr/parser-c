int printf(const char * restrict format, ...);
#define DBG(fmt,val) (printf(#val ": " fmt "\n",val))
enum a { e0, e1, e2, e3 };
enum b { e4 = e3 + 1, e5, e9 = e3*3, e10 };
enum c { e20 = 20, e15 = 15, e16, e17, emm = -2, em, e0a };
int main() {
  DBG("%d",e0);
  DBG("%d",e1);
  DBG("%d",e2);
  DBG("%d",e3);
  DBG("%d",e4);
  DBG("%d",e5);
  DBG("%d",e9);
  DBG("%d",e10);
  DBG("%d",e20);
  DBG("%d",e15);
  DBG("%d",e16);
  DBG("%d",e17);
  DBG("%d",emm);
  DBG("%d",em);
  DBG("%d",e0a);
}
