int *f(int x) {
  int *y = &x;
  return y;
}


int main() {
  int x = 1;
  int* z = f(x);
  printf("%d",*z);
  return 0;
}
