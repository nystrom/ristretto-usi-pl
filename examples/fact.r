extern void println(int x);

int fact(int n) {
  if (n == 0)
    return 1;
  else
    return n * fact(n-1);
}

void main() {
  println(fact(10));
}
