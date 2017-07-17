#include <stdio.h>
// selection sort

void sort(int a[], int size) {
  int i = 0;
  while (i < size) {
    int j = i+1;
    while (j < size) {
      if (a[j] < a[i]) {
	int t = a[i];
	a[i] = a[j];
	a[j] = t;
      }
      j++;
    }
    i++;
  }
  return;
}



/*
int main()
{
  int a[] = {2,7,4,5,1,0};
  int size = 6;
  sort(a, size);
  for (int i = 0; i < size; i++)
    {
      printf("%d ", a[i]);
    }
  return 0;
}
*/

/*
first we rewrite the above program with explicit goto instead of for-loop, 
refer to sort_goto.c
 */
