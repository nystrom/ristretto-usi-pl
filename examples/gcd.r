extern void println(int x);

int gcd(int a, int b) {
  while (b != 0) {
    int t = b;
    b = a % b;
    a = t;
  }
  return a;
}

void main() {
  println(gcd(12, 10));
}
