int f() {
  int c = 0;
  while (c<10) {
    if (c % 2 == 0)
      c++;
    if (c % 3 == 0) {
      c++;
    } else {
      c = c + 2;
    }
    // c++;      
  }
  return c;
}

int main() {
  printf("%d\n", f());
}

