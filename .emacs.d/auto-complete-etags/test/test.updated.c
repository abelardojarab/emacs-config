void simple_func(void)
{
}

void simple_func2(int a,
                  int b)
{
  return;
}

int
multiple_line_func(void)
{
  return 1;
}

const char*
multiple_line_va_arg_func(int a,
                          int b,
                          ...)
{
  return NULL;
}

#define MACRO1
#define MACRO2

MACRO1
MACRO2 const char *
macro_func(int a)
{
  return NULL;
}

void
old_style_func(a, b)
  int a;
  int b;
{
}
/*
 * This is comment just before fucntion.
 */
void
comment_func(int a, int b)
{
  /* do something */
}
/*
 * This is comment just before fucntion.
 */
const char * comment_func2(void)
{
}
void crammed_func(void)
{
}

enum { dummy1 = 1, dummy2, };

int updated_func(void)
{
  return 0;
}
