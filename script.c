#include "script.h"

int print_id(Script S)
{
  lua_getglobal(S, "object_id");

  if (!lua_isnumber(S, -1))
    script_bail (S, "not able to get object_id\n");

  printf("%d\n", (int) lua_tonumber(S, -1));

  lua_pop(S, 1);

  return 0;
}

Script load_script_into_memory(const char *filename)
{
  Script S = luaL_newstate();
  luaL_openlibs(S);

  lua_pushcfunction(S, print_id); // <-- the function pointer name
  lua_setglobal(S, "print_id");   // <-- can be different from this string

  if (luaL_loadfile(S, filename) || lua_pcall(S, 0, 0, 0))
    script_bail (S, "Can't load %s into memory", filename);

  return S;
}

void script_bail (Script S, const char *fmt, ...)
{
  va_list argp;
  va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
  va_end(argp);
  lua_close(S);
  exit(EXIT_FAILURE);
}

void script_call_function(Script S, const char * function)
{
  /* push functions */
  lua_getglobal(S, function);  /* function to be called */

  if (lua_pcall(S, 0, 0, 0) != 0)
    script_bail (S, "Error calling function: %s", function);
}

void script_set_table (Script S, const char *name)
{
  lua_getglobal(S, name);

  if (!lua_istable(S, -1))
    script_bail (S, "'%d' is not a table", name);
}

const char * script_get_table_field (Script S, const char *key)
{
  lua_pushstring(S, key);
  lua_gettable(S, -2);

  if (!lua_isstring(S, -1))
    script_bail (S, "invalid field in table for key: %s", key);

  const char *result = lua_tostring(S, -1);

  lua_pop(S, 1); // pop our key field so top of stack is table

  return result;
}

void script_destroy (Script S)
{
  lua_close (S);
}
