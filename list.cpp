#include "list.h"
#include "stdlib.h"

paramlist_t* push_param(paramlist_t *head, const char *name, Type* type)
{
  paramlist_t * node = (paramlist_t*) malloc(sizeof(paramlist_t));
  node->next = head;
  node->name = name;
  node->type = type;
  return node;
}

paramlist_t* pop_param(paramlist_t *head)
{
  paramlist_t *next = head->next;
  free(head);
  return next;
}

paramlist_t* next_param(paramlist_t *head)
{
  paramlist_t *next = head->next;
  return next;
}

int paramlist_size(paramlist_t *head)
{
  int cnt=0;
  while(head)
    {
      cnt++;
      head=head->next;
    }
  
  return cnt;
}

typedef struct loop_list {
  loop_info_t info;
  struct loop_list *next;
} loop_list_t;

static loop_list_t *head = NULL;

void push_loop(BasicBlock* expr,
	       BasicBlock* body,
	       BasicBlock* reinit,
	       BasicBlock* exit)
{
  loop_list_t *n = (loop_list_t*) malloc(sizeof(loop_list_t));
  n->info.expr = expr;
  n->info.body = body;
  n->info.reinit = reinit;
  n->info.exit = exit;

  n->next = head;
  head = n;
}

void pop_loop()
{
  loop_list_t *tmp = head;
  head = head->next;
  free(tmp);
}

loop_info_t get_loop()
{
  loop_info_t n;
  n.expr = n.body = n.reinit = n.exit = NULL;
  
  if (head)
    return head->info;
  else
    return n;
}


//NEW ADDITION - IF

typedef struct if_list {
  if_info_t if_info;
  struct if_list *next;
} if_list_t;

static if_list_t *if_head = NULL;

void push_if(BasicBlock* then,
	       BasicBlock* els,
	       BasicBlock* join)
{
  if_list_t *ni = (if_list_t*) malloc(sizeof(if_list_t));
  ni->if_info.then = then;
  ni->if_info.els = els;
  ni->if_info.join = join;

  ni->next = if_head;
  if_head = ni;
}

void pop_if()
{
  if_list_t *tmp = if_head;
  if_head = if_head->next;
  free(tmp);
}

if_info_t get_if()
{
  if_info_t ni;
  ni.then = ni.els = ni.join = NULL;
  
  if (if_head)
    return if_head->if_info;
  else
    return ni;
}


// NEW ADDITION - SWITCH

typedef struct switch_list {
  switch_info_t switch_info;
  struct switch_list *next;
} switch_list_t;

static switch_list_t *switch_head = NULL;

void push_switch(BasicBlock* defaultCase,
	         SwitchInst *body,
                 BasicBlock *lastCase,
                 int broken              )
{
  switch_list_t *ns = (switch_list_t*) malloc(sizeof(switch_list_t));
  ns->switch_info.defaultCase = defaultCase;
  ns->switch_info.body = body;
  ns->switch_info.lastCase = lastCase;
  ns->switch_info.broken = broken;
  ns->next = switch_head;
  switch_head = ns;
}

void pop_switch()
{
  switch_list_t *tmp = switch_head;
  switch_head = switch_head->next;
  free(tmp);
}

switch_info_t get_switch()
{
  switch_info_t ns;
  ns.defaultCase = NULL;
  ns.body  = NULL;
  ns.lastCase = NULL;
  ns.broken = 0;
  
  if (switch_head)
    return switch_head->switch_info;
  else
    return ns;
}
