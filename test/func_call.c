int g(int x) { return x; }

int f(int x) {
  x = g(1);
  x = x + 1;
  return x;
}

