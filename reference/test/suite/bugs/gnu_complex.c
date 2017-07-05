__complex__ float c;
int main() {
  float i = __imag (c*2);
  float r = __real (c-2);
  __complex__ double x = 2LLj + 2.0fj;
}