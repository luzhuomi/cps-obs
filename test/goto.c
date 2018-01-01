int f() {
  int n = 0;
 foo:
  n++;
  if (n < 10) {
    goto foo;
  } else {
  }
  return n;
}


int main() {
  printf("%d", f());
}
