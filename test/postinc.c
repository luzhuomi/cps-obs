int f() {
  int i = 1;
  int j = 0;
  
  // j = (i++);
  j = (i++);
  i = i+1;
  return j;
}


int main() {
  printf ("%d\n", f());
  return 0;
}
