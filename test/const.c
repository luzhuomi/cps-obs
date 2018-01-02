int f(int x) {
  const int i = x;
  if (x > 0) {
    x = x + i;
  }
  return x;
}

int main() {
  printf("%d",f(1));
  return 0;
}
