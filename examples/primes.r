// Print all the prime numbers from 2 to 10000
void main() {
  boolean[] composite = new boolean[10000];

  int i = 2;
  while (i < composite.length) {
    if (! composite[i]) {
      int j = i+i;
      while (j < composite.length) {
        composite[j] = true;
        j = j + i;
      }
      println(i);
    }
    i = i + 1;
  }
}
