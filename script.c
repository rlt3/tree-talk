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

void script_load_classes (const char *filename)
{
  const char * name;
  const char * script_files[MAX_CLASSES];

  int element_index = 0;
  int script_index = 0;

  Script S = load_script_into_memory(filename);

  /* setup table */
  script_set_table(S, "classes");

  /* each element has no key */
  lua_pushnil(S);

  /* iterate through this table of tables */
  while (lua_next(S, -2)) {

    if (!lua_istable(S, -1))
      script_bail (S, 
          "Element %d of table `classes' is not a table!\n",
          element_index);

    name = script_get_table_field(S, "name");

    /* get the table where that's named `components' */
    lua_pushstring(S, "components");
    lua_gettable(S, -2);

    if (!lua_istable(S, -1))
      script_bail (S, 
          "Components of element %d is not a table!\n",
          element_index);

    /* each component has no key */
    lua_pushnil(S);

    /* go through each component */
    while (lua_next(S, -2)) {

      if (!lua_isstring(S, -1))
        script_bail (S, 
            "Element %d of components of class %d is not a string!\n",
            script_index, element_index);

      /* add the component file to our index and increment the index */
      script_files[script_index++] = lua_tostring(S, -1);

      /* pop current to move to next */
      lua_pop(S, 1);
    }

    if (class_create(name, script_index, script_files) != 0)
      script_bail(S, "Classes exceeds limit of %d\n", MAX_CLASSES);

    /* pop the components table */
    /* pop the current table to make room for the next */
    lua_pop(S, 2); 
  }

  script_destroy(S);
}

void script_load_tree (const char *filename)
{
  const char * name;
  const char * script_files[MAX_CLASSES];

  int element_index = 1;

  const char* key;
  int value;

  Script S = load_script_into_memory(filename);

  /* setup table */
  script_set_table(S, "tree");

  /* each element has no key */
  lua_pushnil(S);

  /* iterate through this table of tables */
  while (lua_next(S, -2)) {

    if (!lua_istable(S, -1))
      script_bail (S, 
          "Element %d of table `tree' is not a table!\n",
          element_index);

    /* each table has no key */
    lua_pushnil(S);

    /* go through each component */
    while (lua_next(S, -2)) {

      key   = lua_tostring(S, -2);
      value = (int)lua_tonumber(S, -1);

      printf("%s => %d\n", key, value);

      lua_pop(S, 1);
    }

    lua_pop(S, 1);
    element_index++;
  }
}
