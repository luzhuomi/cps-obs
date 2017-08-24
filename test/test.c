#include <stdlib.h>
#include <stdio.h>
typedef struct fibCtxt {
            int x;
            int f1_myLabel0;
            int f1_myLabel1;
            int f1_myLabel2;
            int f1_myLabel3;
            int f2_myLabel0;
            int f2_myLabel1;
            int f2_myLabel2;
            int f2_myLabel3;
            int i_myLabel0;
            int i_myLabel1;
            int i_myLabel2;
            int i_myLabel3;
            int t_myLabel0;
            int t_myLabel1;
            int t_myLabel2;
            int t_myLabel3;
            void (* loop_ks[20])(struct fibCtxt *);
            int (* loop_conds[20])(struct fibCtxt *);
            void (* loop_visitors[20])(void (* kParam)(struct fibCtxt *),
                                       struct fibCtxt *);
            void (* loop_exits[20])(void (* kParam)(struct fibCtxt *),
                                    struct fibCtxt *);
            int curr_stack_size;
            int func_result;
        } fibctxt;
void fib_myLabel0(void (* kParam)(fibctxt *), fibctxt * ctxtParam);
void fib_myLabel1(void (* kParam)(fibctxt *), fibctxt * ctxtParam);
void fib_myLabel2(void (* kParam)(fibctxt *), fibctxt * ctxtParam);
void fib_myLabel3(void (* kParam)(fibctxt *), fibctxt * ctxtParam);
int fib_cond_myLabel1(fibctxt * ctxtParam);
void fib_loop(int (* condParam)(fibctxt *),
              void (* visitorParam)(void (* kParam)(fibctxt *), fibctxt *),
              void (* exitParam)(void (* kParam)(fibctxt *), fibctxt *),
              void (* kParam)(fibctxt *),
              fibctxt * ctxtParam);
void fib_lambda_loop(fibctxt * ctxtParam);
void fib_id(fibctxt * ctxtParam);
void fib_push(int (* condParam)(fibctxt *),
              void (* visitorParam)(void (* kParam)(fibctxt *), fibctxt *),
              void (* exitParam)(void (* kParam)(fibctxt *), fibctxt *),
              void (* kParam)(fibctxt *),
              fibctxt * ctxtParam);
void fib_pop(fibctxt * ctxtParam);
void fib_myLabel0(void (* kParam)(fibctxt *), fibctxt * ctxtParam)
{
    ctxtParam->f1_myLabel0 = 1;
    ctxtParam->f2_myLabel0 = 1;
    ctxtParam->i_myLabel0 = 3;
    ctxtParam->i_myLabel1 = ctxtParam->i_myLabel0;
    ctxtParam->t_myLabel1 = ctxtParam->t_myLabel0;
    ctxtParam->f1_myLabel1 = ctxtParam->f1_myLabel0;
    ctxtParam->f2_myLabel1 = ctxtParam->f2_myLabel0;
    fib_myLabel1(kParam, ctxtParam);
}
void fib_myLabel1(void (* kParam)(fibctxt *), fibctxt * ctxtParam)
{
    fib_push(&fib_cond_myLabel1,
             &fib_myLabel2,
             &fib_myLabel3,
             kParam,
             ctxtParam);
    fib_loop(ctxtParam->loop_conds[ctxtParam->curr_stack_size],
             ctxtParam->loop_visitors[ctxtParam->curr_stack_size],
             ctxtParam->loop_exits[ctxtParam->curr_stack_size],
             ctxtParam->loop_ks[ctxtParam->curr_stack_size],
             ctxtParam);
}
void fib_myLabel2(void (* kParam)(fibctxt *), fibctxt * ctxtParam)
{
    ctxtParam->t_myLabel2 = ctxtParam->f1_myLabel1 + ctxtParam->f2_myLabel1;
    ctxtParam->f1_myLabel2 = ctxtParam->f2_myLabel1;
    ctxtParam->f2_myLabel2 = ctxtParam->t_myLabel2;
    ctxtParam->i_myLabel2 = ctxtParam->i_myLabel1 + 1;
    ctxtParam->i_myLabel1 = ctxtParam->i_myLabel2;
    ctxtParam->t_myLabel1 = ctxtParam->t_myLabel2;
    ctxtParam->f1_myLabel1 = ctxtParam->f1_myLabel2;
    ctxtParam->f2_myLabel1 = ctxtParam->f2_myLabel2;
    (*kParam)(ctxtParam);
}
void fib_myLabel3(void (* kParam)(fibctxt *), fibctxt * ctxtParam)
{
    fib_pop(ctxtParam);
    ctxtParam->func_result = ctxtParam->f2_myLabel1;
    (*kParam)(ctxtParam);
}
int fib_cond_myLabel1(fibctxt * ctxtParam)
{
    return ctxtParam->i_myLabel1 <= ctxtParam->x;
}
void fib_loop(int (* condParam)(fibctxt *),
              void (* visitorParam)(void (* kParam)(fibctxt *), fibctxt *),
              void (* exitParam)(void (* kParam)(fibctxt *), fibctxt *),
              void (* kParam)(fibctxt *),
              fibctxt * ctxtParam)
{
    if ((*condParam)(ctxtParam))
    {
        (*visitorParam)(&fib_lambda_loop, ctxtParam);
    }
    else
    {
        (*exitParam)(kParam, ctxtParam);
    }
}
void fib_lambda_loop(fibctxt * ctxtParam)
{
    fib_loop(ctxtParam->loop_conds[ctxtParam->curr_stack_size],
             ctxtParam->loop_visitors[ctxtParam->curr_stack_size],
             ctxtParam->loop_exits[ctxtParam->curr_stack_size],
             ctxtParam->loop_ks[ctxtParam->curr_stack_size],
             ctxtParam);
}
void fib_id(fibctxt * ctxtParam)
{
    return;
}
void fib_push(int (* condParam)(fibctxt *),
              void (* visitorParam)(void (* kParam)(fibctxt *), fibctxt *),
              void (* exitParam)(void (* kParam)(fibctxt *), fibctxt *),
              void (* kParam)(fibctxt *),
              fibctxt * ctxtParam)
{
    ctxtParam->curr_stack_size = ctxtParam->curr_stack_size + 1;
    ctxtParam->loop_conds[ctxtParam->curr_stack_size] = condParam;
    ctxtParam->loop_visitors[ctxtParam->curr_stack_size] = visitorParam;
    ctxtParam->loop_exits[ctxtParam->curr_stack_size] = exitParam;
    ctxtParam->loop_ks[ctxtParam->curr_stack_size] = kParam;
}
void fib_pop(fibctxt * ctxtParam)
{
    ctxtParam->curr_stack_size = ctxtParam->curr_stack_size - 1;
}
int fib(int x)
{
    fibctxt * ctxtParam = (fibctxt *) malloc(sizeof(fibctxt));
    ctxtParam->x = x;
    fib_myLabel0(&fib_id, ctxtParam);
    return ctxtParam->func_result;
}


int main() {
  printf("%d",fib(10));
}
