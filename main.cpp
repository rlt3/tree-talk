#include "instance.hpp"

Variable make_var (const char *name, int val)
{
  Variable var;

  var.type = t_int;
  var.name = name;
  var.value.i = val;
  
  return var;
}

int main (void)
{
  Instance ins("test");

  Component c1("draw.lua");
  c1.add_env_var(make_var("x", 600));
  c1.add_env_var(make_var("y", 800));
  c1.load();

  Component c2("draw.lua");
  c2.add_env_var(make_var("x", 1));
  c2.add_env_var(make_var("y", 3));
  c2.load();

  ins.add_component(c1);
  ins.add_component(c2);

  ins.message(1, "message");

  return 0;
}
