#ifndef SCRIPT_H
#define SCRIPT_H

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <stdlib.h>

typedef lua_State* Script;

#include "instance.h"

Script       load_script_into_memory (const char *filename);
void         script_bail (Script S, const char *fmt, ...);
void         script_call_function (Script S, const char * function);
void         script_set_table (Script S, const char *name);
const char * script_get_table_field (Script S, const char *key);
void         script_destroy (Script S);

/* our loading functions */
void script_load_classes(const char *filename);
void script_load_tree(const char *filename);

#endif
