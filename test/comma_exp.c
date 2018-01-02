int f(int x) {
  int i,j=0;
  if (x >0) {
    for ((i = 0, j = 1); i < 10; (i++,j = j*2)) {
      // for ( i=0; i< 10; i++) {
      printf("%d,%d\n", i,j);
    }
  }
  return i;
}

int main() {
  printf("%d",f(1));
  return 0;
}
