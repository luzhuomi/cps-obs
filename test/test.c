typedef struct _node {
            short n_type;
            char * n_str;
            int n_lineno;
            int n_col_offset;
            int n_nchildren;
            struct _node * n_child;
        } node;



static int num_stmts(const node * n)
{
    int i, l;
    node * ch;
    switch (n->n_type)
    {
    case 256:
        if ((&n->n_child[0])->n_type == 4)
        {
            return 0;
        }
        else
        {
            return num_stmts(&n->n_child[0]);
        }
    case 257:
        l = 0;
        for (i = 0; i < n->n_nchildren; i++)
        {
            ch = &n->n_child[i];
            if (ch->n_type == 267)
            {
                l += num_stmts(ch);
            }
        }
        return l;
    case 267:
        return num_stmts(&n->n_child[0]);
    case 292:
        return 1;
    case 268:
        return n->n_nchildren / 2;
    case 300:
        if (n->n_nchildren == 1)
        {
            return num_stmts(&n->n_child[0]);
        }
        else
        {
            l = 0;
            for (i = 2; i < n->n_nchildren - 1; i++)
                l += num_stmts(&n->n_child[i]);
            return l;
        }
    default:
        {
            char buf[128];
	    /*
            __builtin___sprintf_chk(buf,
                                    0,
                                    __builtin_object_size(buf, 2 > 1 ? 1 : 0),
                                    "Non-statement found: %d %d",
                                    n->n_type,
                                    n->n_nchildren);
            Py_FatalError(buf);
	    */
        }
    }
    /*
    __builtin_expect(!0, 0) ? __assert_rtn(__func__,
                                           "ast_orig.c",
                                           208,
                                           "0") : (void) 0;
    */
    return 0;
}

int main() {
  node * p = (node *) malloc(sizeof(node));
  node * c1 = (node *) malloc(sizeof(node));
  c1->n_str = "hello";
  c1->n_type = 4;
  p->n_str = "world";
  p->n_type = 256;
  p->n_nchildren = 1;
  p->n_child = (node *)malloc(sizeof(node)*1);
  p->n_child = c1;
  printf("%d",num_stmts(p));
  return 0;
}
