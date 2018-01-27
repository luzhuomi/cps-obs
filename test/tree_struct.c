typedef struct _node {
            short n_type;
            char * n_str;
            int n_lineno;
            int n_col_offset;
            int n_nchildren;
            struct _node * n_child;
        } node;


static char * ast_for_expr(const node * n)
{
  int i; // 0
 loop: // loop
  switch (n->n_type) // 1
    {
    case 304:
    case 302:
      return n->n_str; // 2
    case 305:
    case 306:
      if (n->n_nchildren == 1) // 3
        { 
	  n = &n->n_child[0]; // 4
            goto loop;
        }
      // 5
    }
  return 0; // 6
}

int main() {
  node * p = (node *) malloc(sizeof(node));
  node * c1 = (node *) malloc(sizeof(node));
  //cnode * gc1 = (node *) malloc(sizeof(node));
    
  c1->n_str = "hello";
  c1->n_type = 302;
  p->n_str = "world";
  p->n_type = 306;
  p->n_nchildren = 1;
  p->n_child = c1;
  printf("%s",ast_for_expr(p));
  return 0;
}

/*
  0
  |
  v
  loop<----
  |       |
  v       |
  1-->3-->4
  |   |
  v   v
  2   5
  |   |
  v   v
 -1<--6
     

  
dom tree

    0
   / \
loop -1
  |
  1
 / \
2   3
   / \
  4   5
      |
      6
 */
