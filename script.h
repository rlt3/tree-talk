#ifndef TT_SCRIPT_H
#define TT_SCRIPT_H

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

typedef lua_State* Script;

Script script_new ();

void script_bail (Script S, const char *fmt, ...);
void script_destroy (Script S);

/* return 0 if no errors, 1 if errors */
int script_load (Script S, const char *filename);
int script_call_function (Script S, const char *function);

void script_set_int(Script S, const char *name, int value);
void script_set_float(Script S, const char *name, float value);
void script_set_string(Script S, const char *name, const char *value);
void script_set_boolean(Script S, const char *name, int value);
void script_set_null(Script S, const char *name);

#endif
