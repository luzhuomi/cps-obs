int g(int x) { return x; }

int f(int x) {
  x = g(1);
  if (x > 0) {
    x = g(2);
  }
  x = x + 1;
  return x;
}

