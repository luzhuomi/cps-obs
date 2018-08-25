int f() {
  int i = 1;
  int j = 0;
  
  // j = (i++)++ // gcc: expression is not assignable;
  // (j++) = i++ // gcc: expression is not assignable;

  // j = (i++) + (i+=1); // case 1
  /*
    the desugaring need to take place on the binary operator experession statement, maybe if statement too?
   */

  // step 1 desugar i++
  // j = ((i=i+1)-1) + (i=i+1);

  // step 2 move out the first nested assignment;
  // i = i + 1;
  // j = (i-1) + (i= i+1);

  // step 3 move out the second nested assignment;

  i = i + 1;
  int i1 = i; // we need to save the state of i1 here, // no renaming is required if the expression is linear?!
  i = i1 + 1;
  int i2 = i;
  j = (i1-1) + i2;


  
  
  // (i++) + (j=i++); // case 2
  
  i = i+1;
  return j;
}


int main() {
  printf ("%d\n", f());
  return 0;
}
