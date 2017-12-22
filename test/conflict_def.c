/*
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
*/
int g(x) {
  int v;
  int i;
  if (v > 0) {
    for (i = 1; i < 10; i++) {
      if (v == 0) {
	v = v +1;
      } else {
	v = v * 2;
	if (v == 0)
	  goto onError;
      }
    }
  }
  return v;
 onError:
  return 0;
}
