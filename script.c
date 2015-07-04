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
    script_bail (S, "Can't load %s into memory\n", filename);

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
    script_bail (S, "'%s' is not a table", name);
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
  lua_close(S);
  S = NULL;
}

void script_load_classes (Script S)
{
  const char * name;
  const char * script_files[MAX_CLASSES];

  int element_index = 0;
  int script_index = 0;

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


    element_index++;
    script_index = 0;
  }
}

void script_load_tree (Script S)
{
  Script scripts[MAX_CLASSES];
  int env_index = 0;
  int class_id;

  struct Class *class;

  int element_index = 1;

  int x, y;

  /* setup table */
  script_set_table(S, "tree");

  /* each element has no key */
  lua_pushnil(S);

  /* iterate through this table of tables */
  while (lua_next(S, -2)) {

    if (!lua_istable(S, -1))
      script_bail (S, "Element %d of table `tree' is not a table!\n",
          element_index);

    lua_pushstring(S, "class");
    lua_gettable(S, -2);

    class_id = lua_tointeger(S, -1);

    lua_pop(S, 1);

    /* get the table where that's named `components' */
    lua_pushstring(S, "envs");
    lua_gettable(S, -2);

    if (!lua_istable(S, -1))
      script_bail (S, "envs is not a table in element %d!\n", element_index);

    /* each component has no key */
    lua_pushnil(S);

    /* go through each component */
    while (lua_next(S, -2)) {

      if (!lua_istable(S, -1))
        script_bail (S, 
          "Env %d of Element %d of table `tree' is not a table!\n",
          env_index, element_index);

      /* need some way to push this environment to be loaded when the script
       * is loaded into memory. need some sort of key => value map that can
       * work with multiple data types. maybe only int now for prototype
       */

      /* get our x and y values */
      lua_pushstring(S, "x");
      lua_gettable(S, -2);
      x = (int) lua_tonumber(S, -1);
      lua_pop(S, 1);

      lua_pushstring(S, "y");
      lua_gettable(S, -2);
      y = (int) lua_tonumber(S, -1);
      lua_pop(S, 1);

      /* get our class reference */
      class = class_get_by_id(class_id);

      /* create a new lua state and push our environment */
      Script N = luaL_newstate();
      luaL_openlibs(N);

      lua_pushnumber(N, x); 
      lua_setglobal(N, "x");

      lua_pushnumber(N, y); 
      lua_setglobal(N, "y");

      /* load the file from the specifc script in the class reference */
      if (luaL_loadfile(N, class->component_files[env_index]) || 
          lua_pcall(N, 0, 0, 0))
        script_bail (N, "Can't load %s into memory\n", 
            class->component_files[env_index]);

      /* test everything out and then destroy it, we're not saving now */
      lua_getglobal(N, "message");  /* function to be called */
      if (lua_pcall(N, 0, 0, 0) != 0)
        script_bail (N, "Error calling function: %s", "message");

      lua_close(N);

      //scripts[env_index] = N;

      // printf("Env %d (class %d) of element %d: { x => %d, y => %d }\n", 
      //     env_index, class_id, element_index, x, y);

      lua_pop(S, 1);
      env_index++;
    }

    element_index++;
    env_index = 0;
    lua_pop(S, 2);
  }
}
