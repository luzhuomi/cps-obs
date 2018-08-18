int f(int x ) {
  for (int i = 0; i < x ; i++) {
    if (i == 5) {
      return i;
    }
  }
}

int main() {
  printf("%d\n", f(6));
}
