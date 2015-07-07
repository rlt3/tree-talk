#ifndef TT_COMPONENT_H
#define TT_COMPONENT_H

#define MAX_ENV_VARS 20

#include "script.h"

enum ScriptTypes {
  t_int, t_float, t_string, t_bool, t_null
};

struct Variable {
  const char *name;
  enum ScriptTypes type;
  union {
    int i;
    float f;
    const char *s;
  } value;
};

struct Component {
  const char *filename;
  Script script;
  struct Variable variables[MAX_ENV_VARS];
};

typedef struct Component Component;
typedef struct Variable Variable;

/* 
 * Have functions here for accessing, running, or reloading components here.
 * This is effectively the wrapper around our script. We shouldn't any much
 * `low-level' script.h functions being used above this level (or at this tier
 * for other files).
 */

int component_message(Component *c);
int component_reload(Component *c);

Component component_create(const char* filename, int varc, Variable *vars);

#endif
