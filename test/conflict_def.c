int f(int n) {
  int i, l;
  switch (n) {
  case 1:
    return 0;
  case 2:
    l = 0;
    for (i = 0; i<3; i++) {
      l = l + i;
    }
    return l;
  }
  return 0;
}
