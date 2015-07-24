#include "instance.hpp"
#include "aligned_storage.hpp"

#include <stdio.h>

int main (void)
{
  Component c1("draw.lua");
  c1.add_env_var(Variable("x", 600));
  c1.add_env_var(Variable("y", 800));
  c1.load();

  Component c2("draw.lua");
  c2.add_env_var(Variable("x", 1));
  c2.add_env_var(Variable("y", 3));
  c2.load();

  AlignedStorage<Instance, 20> instances;

  Instance &head = instances.add("head");

  head
    .add_component(c1)
    .add_component(c2)
    .message(1, "message");

  head.components[0].clear_env_vars();
  head.components[1].clear_env_vars();

  head.components[0].add_env_var(Variable("x", 20));
  head.components[0].add_env_var(Variable("y", "milllion"));
  head.components[0].load();

  head.components[1].add_env_var(Variable("x", 1818));
  head.components[1].add_env_var(Variable("y", 9191));
  head.components[1].load();

  head.message(1, "message");

  //instances.last().each_component([] (Component &c) {
  //  printf("%s\n", c.filename.c_str());
  //});

  return 0;
}
