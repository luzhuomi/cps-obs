int f(int x) {
  // 0
  if (x == 0) { // 1
    return x; // 2
  } else {
    int a[3]; // 3
    a[0] = -x; 
    return a[0];
  }
}


int main() {
  printf("%d\n", f(10));
  return 0;
}
