
int search(int a[], int size, int x) {
  if (a) { // 0
    int i;  // 1
    for (i=0 /* // 2 */; i< size; i++) // 3
      {
	// 4
	if (a[i] == x) 
	  { 
	    return i; // 5
	  }
	// 6
	// 7 i++
      }
  }
  // 8 implicit else
  // 9
  return -1;
}

int main() {
  int a[] = {2,7,4,5,1,0};
  int size = 6;
  printf("%d\n", search(a, size, 5));
  return 0;

}

