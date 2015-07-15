#include "script.h"

#include <stdlib.h>
#include <stdio.h>

void script_bail (Script S, const char *fmt, ...)
{
  va_list argp;
  va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
  va_end(argp);
  lua_close(S);
  exit(EXIT_FAILURE);
}

Script script_new ()
{
  Script S = luaL_newstate();
  luaL_openlibs(S);
  return S;
}

void script_destroy (Script S)
{
  lua_close(S);
}

/* return 0 if no errors, 1 if errors */
int script_load (Script S, const char *filename)
{
  return (luaL_loadfile(S, filename) || lua_pcall(S, 0, 0, 0)) ? 1 : 0;
}

int script_call_function (Script S, const char *function)
{
  lua_getglobal(S, function);  /* function to be called */
  return (lua_pcall(S, 0, 0, 0) ? 1 : 0);
}

void script_set_cfunction (Script S, 
    int (*function)(lua_State*), 
    const char *name)
{
  lua_pushcfunction(S, function);
  lua_setglobal(S, name);  
}

void script_set_int(Script S, const char *name, int value)
{
  lua_pushnumber(S, value); 
  lua_setglobal(S, name);
}

void script_set_float(Script S, const char *name, float value)
{
  lua_pushnumber(S, value); 
  lua_setglobal(S, name);
}

void script_set_string(Script S, const char *name, const char *value)
{
  lua_pushstring(S, value); 
  lua_setglobal(S, name);
}

void script_set_boolean(Script S, const char *name, int value)
{
  lua_pushboolean(S, value); 
  lua_setglobal(S, name);
}

void script_set_null(Script S, const char *name)
{
  lua_setglobal(S, name);
}
