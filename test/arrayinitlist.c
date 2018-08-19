void f() {
  static const unsigned a[5] = {1,2,3,4,5};
  /*
  int a[5];
  *(a+0) = 1;
  *(a+1) = 2;
  *(a+2) = 3;
  *(a+3) = 4;
  *(a+4) = 5;
  */
  
  for (int i = 0; i < 5; i++) {
    int b[1] = {0};
    printf("%d ", a[i]);
  }
  
}


int main(){
  f();
  return 0;
}


