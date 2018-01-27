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


----------------------------------------------------------
   | succ | idom | DF1 | DFup | DF 
----------------------------------------------------------
 0 |loop  |      |     |      | 
----------------------------------------------------------
 l | 1    |  0   |     |      |
----------------------------------------------------------
 1 | 2,3  | loop |     |      |
----------------------------------------------------------
 2 | -1   |  1   |     |      |
----------------------------------------------------------
 3 |4,5   |  1   |     |      | 
----------------------------------------------------------
 4 |loop  |  3   |loop |      |loop 
----------------------------------------------------------
 5 | 6    |  3   |     |      |  
----------------------------------------------------------
 6 | -1   |  5   |     |      |  
----------------------------------------------------------
-1 |      |      |     |      |  


def: b \in child(a) iff a=idom(b).


DF1(a) is the local dominance frontier for node a. 
DF1(a) = { b | b \in succ(a), not (a sdom b) }
e.g. DF1(3) = { 2 } because 2 \in succ(3), but not (3 sdom 2)

Lemma: Let b \in succ(a), a = idom(b) iff a sdom b

Hence DF1 can be computed easily via the following
 DF1(a) = { b | b \in succ(a), a != idom(b) }

DFup(a) is the upwards dominance frontier for node a.
DFup(a) = { b | b \in DF(a), not (idom(a) sdom b) }

 */
