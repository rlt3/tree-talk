#include "instance.h"
#include "parser.h"

int main (void)
{
  Parser p = parser_create("load.lua");
  parser_create_tree(p);

  Instance *ins;
  Component *cmp;

  while ((ins = instance_each_child(instance_by_id(0))) != NULL) {
    printf("%s: ", ins->name);

    while ((cmp = instance_each_component(ins)) != NULL) {
      component_message(cmp, "message");
    }
  }

  instances_cleanup();
  parser_destroy(p);
  return 0;
}
