typedef struct S {
  int f1;
} ST;


int f() {
  ST* u = (ST*)malloc(sizeof(ST));
  ((ST*)u)->f1 = 1;
  return u->f1;
}

int main() {
  printf("%d", f());
  return 0;
}
