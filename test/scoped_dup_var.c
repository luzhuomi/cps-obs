int f() {
  int i = 0;
  
  if (i > 0) {
    int j = 1;
    return j;
  } else {
    char j = '2';
    return j;
  }
  return i;
}


int main() {
  printf("%d", f());
  return 0;
}
