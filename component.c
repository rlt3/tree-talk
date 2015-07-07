#include "component.h"
#include <string.h>

Component component_create(const char* filename, int varc, Variable *vars)
{
  Script S = script_new();
  script_load(S, filename);

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

  Component cmp;
  cmp.filename = filename;
  cmp.script = S;
  memcpy(cmp.variables, vars, varc * sizeof(vars));

  return cmp;
}
