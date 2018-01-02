int f() {
  int x = 1;
  int *p = (int *)malloc(sizeof(int));
  if (x > 0) {
    *p = x;
  } else {
    *p = 2;
  }
  return *p;
}

int main() {
  printf("%d",f());
  return 0;
}
