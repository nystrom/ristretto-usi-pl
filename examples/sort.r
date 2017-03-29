extern void println(int[] s);

void sort(int[] a) {
  qsort(a, 0, a.length-1);
}

void qsort(int[] a, int lo, int hi) {
  int i = lo;
  int j = hi;
  int pivot = a[lo + (hi-lo) / 2];

  // partition around the pivot
  while (i <= j) {
    while (a[i] < pivot) {
      i = i + 1;
    }
    while (a[j] > pivot) {
      j = j - 1;
    }
    if (i <= j) {
      int t = a[i];
      a[i] = a[j];
      a[j] = t;
    }
  }

  if (lo < j)
    qsort(a, lo, j);
  if (i < hi)
    qsort(a, i, hi);
}

void assert(boolean b) {
  if (! b) {
    println("assertion failed");
  }
}

void main() {
  int[] s = "the quick brown fox jumped over the lazy dogs";
  sort(s);
  assert(s == "        abcddeeeefghhijklmnoooopqrrsttuuvwxyz");
  println(s);
}
