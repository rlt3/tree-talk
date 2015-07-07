#include "component.h"
#include <string.h>

int component_message(Component *c, const char *message)
{
  return (script_call_function(c->script, message) ? 1 : 0);
}

void component_destroy(Component *c)
{
  script_destroy(c->script);
}

int component_create(Component *cmp, 
    const char* filename, 
    int varc, 
    Variable *vars)
{
  Script S = script_new();

  if (script_load(S, filename)) {
    script_destroy(S);
    return 1;
  }

  Variable var;

  int i;
  for (i = 0; i < varc; i++) {
    var = vars[i];

    switch(vars[i].type) {
      case t_int:
        script_set_int(S, var.name, var.value.i);
        break;

      case t_float:
        script_set_float(S, var.name, var.value.f);
        break;

      case t_string:
        script_set_string(S, var.name, var.value.s);
        break;

      case t_bool:
        script_set_boolean(S, var.name, var.value.i);
        break;

      case t_null:
        script_set_null(S, var.name);
        break;

      default:
        break;
    }
  }

  cmp->filename = filename;
  cmp->script = S;
  memcpy(cmp->variables, vars, varc * sizeof(vars));

  return 0;
}
