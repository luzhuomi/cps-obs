int f(int n) {
  int i, l; // 0
  switch (n) { // 1
  case 1: 
    return 0;  // 2
  case 2: 
    l = 0;  // 3
    for (i = 0; i<3; i++) { // 4
      l = l + i; // 5  
    }
    return l;
  }
  return 0;
}
