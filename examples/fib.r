extern void println(int x);

int fib(int n) {
  if (n <= 1)
    return 1;
  else
    return fib(n-1) + fib(n-2);
}

void main() {
  println(fib(10));
}
