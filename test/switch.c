int f(int x) {
  switch(x) {
  case 1:
    x = x + 1;
    break;
  case 2:
    x = x + 2;
    return x;
  default:
    x = x * 2;
    break;
  }
  return x;
}

int main() {
  printf("%d",f(1));
  printf("%d",f(2));
}

