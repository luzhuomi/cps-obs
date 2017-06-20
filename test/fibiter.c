int fib(int x)
{
  int f1 = 1;
  int f2 = 1;
  for (int i=3; i<=x; i++)
    {
      int t = f1 + f2;
      f1 = f2;
      f2 = t;
    }
  return f2;
}
