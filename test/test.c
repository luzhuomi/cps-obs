/*typedef struct S {
            int n_nchildren;
        } ctxt;

typedef void (*ctxt_arr_void)(ctxt*);
*/
/*
void f () {
  void (*f)(ctxt*) = rhs;
}
*/

void f(int x) {
  switch (x) {
  case 1:
    x = x + 1;
    break;
  default:
    int y = 2;
    break;
  }
}
