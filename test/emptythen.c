/*
int f(int y) {
  int x = 0;
  while (x < y) {
    if (x == y) {
      
    } else {
      x = x + 1;
    }
  }
  return x;
}
*/


int f(int y) {
  int x = 0;
  while (x < y)
    {
      if (x == 100)
	{
	  x = y;
	  if (y >= 100)
	    {
	      x = x+y;
	    }
	}
      if (y == 100)
	{
	  if (x ==  0)
	    {
	      do
		{
		  if (y != 0)
		    {
		      ;
		    }
		  else
		    {
		      y = y - 1;
		    }
		}
	      while (0);
	      return 0;
	    }

	  for (int i = 0; i < y; i += 4)
	    {
	      x = x + i;
	    }
	  do
	    {
	      if ((--x) != 10)
		{
		  ;
		}
	      else
		{
		  x = x - 10;
		}
	    }
	  while (0);
	}
      else
	{
	  x = y;
	}
    }
  return x;
}

int main() {
  printf("%d\n", f(1));
}

