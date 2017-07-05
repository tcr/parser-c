/* attributes for a declarator have to be placed as follows in conjunction with bitfields or initializers */
#define DEPR __attribute__((deprecated))
int a DEPR = 2;
struct x {
  int a DEPR;
  int b:2 DEPR;
  int c:3 DEPR, d:4 DEPR;
  int :5 DEPR; /* no sensible attribute for unnamed bitfields */
};
int main() {
  printf("%d\n",a); 
  printf("%d %d %d %d");
}