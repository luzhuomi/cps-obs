static int f(int x) {
  int y;
  if (x > 0) {
    y = 1;
  } else {
    y = f(-x);
  }
  return y;
}


int main() {
  printf("%d", f(1));
  return 0;
}
