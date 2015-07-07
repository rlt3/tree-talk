#include "script.h"
#include "instance.h"

void create_tree (Script S)
{
  int element = 0;
  int component = 0;

  lua_getglobal(S, "tree");
  if (!lua_istable(S, -1))
    script_bail (S, "`tree' is not a table\n");

  /* each element has no key */
  lua_pushnil(S);

  /* iterate through this table of tables */
  while (lua_next(S, -2)) {
    if (!lua_istable(S, -1))
      script_bail (S, "Element %d of table `tree' is not a table!\n", element);

    /* get the table where that's named `components' */
    lua_pushstring(S, "components");
    lua_gettable(S, -2);
    if (!lua_istable(S, -1))
      script_bail (S, "`components' is not a table in element %d!\n", 
          element);

    lua_pushnil(S);

    /* go through each component */
    while (lua_next(S, -2)) {

      if (!lua_istable(S, -1))
        script_bail (S, 
          "Component %d of Element %d of table `tree' is not a table!\n",
          component, element);

      lua_pushstring(S, "script");
      lua_gettable(S, -2);
      if (!lua_isstring(S, -1))
        script_bail (S, 
            "Script name is not valid for component %d of element %d in table `tree'\n", 
            component, element);

      printf("%s => {", lua_tostring(S, -1));
      lua_pop(S, 1);

      /* get the table where that's named `components' */
      lua_pushstring(S, "env");
      lua_gettable(S, -2);
      if (!lua_istable(S, -1))
        script_bail (S, 
            "`env' is not a table for component %d of element %d in table `tree'\n", 
            component, element);

      lua_pushnil(S);

      while (lua_next(S, -2)) {
        printf(" %s = %d ", lua_tostring(S, -2), (int)lua_tointeger(S, -1));
        lua_pop(S, 1);
      }

      printf("}\n");

      /* pop next key and the env table */
      lua_pop(S, 2);
    }

    /* pop next key and components table */
    lua_pop(S, 2);
  }

  /* pop our nil key */
  lua_pop(S, 1);
}

int main (void)
{
  Script S = script_new();
  script_load(S, "load.lua");
  create_tree(S);
  script_destroy(S);
  return 0;
}
