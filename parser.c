#include "parser.h"
#include <stdlib.h>

/* Forward declarations for `private' functions */
void parser_bail (Parser P, const char *fmt, ...);
void parser_set_key (Parser P, const char *key);
const char * parser_get_string (Parser P, int element, const char *varname);
int parser_get_integer (Parser P, int element, const char *varname);
void parser_push_table (Parser P, int element, const char *table);

void parser_bail (Parser P, const char *fmt, ...)
{
  va_list argp;
  va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
  va_end(argp);
  lua_close(P);
  exit(EXIT_FAILURE);
}

/* 
 * Take an array of Variable with the parser setup at a specific point. Parse
 * through and return a count of how many variables were found.
 */
int parser_get_vars(Parser P, int max_vars, Variable *vars)
{
  int count = 0;
  double n;

  while (lua_next(P, -2)) {
    if (count > max_vars - 1)
      return -1;

    vars[count].name = lua_tostring(P, -2);

    switch(lua_type(P, -1)) {
      case LUA_TNUMBER:
        n = lua_tonumber(P, -1);

        /* check to see if number is floating point or not by casting to int
         * and comparing. If n = 5.0, 5.0 == 5 because both get truncated and
         * equal each other. Otherwise it becomes 4.48 = 4 and this is wrong.
         *
         * In this way, 6.0, 7.0 and other floating points will be converted
         * to integers.
         */
        if (n == (int) n) {
          vars[count].type = t_int;
          vars[count].value.i = n;
        } else {
          vars[count].type = t_float;
          vars[count].value.f = n;
        }
        break;

      case LUA_TSTRING:
        vars[count].type = t_string;
        vars[count].value.s = lua_tostring(P, -1);
        break;

      case LUA_TBOOLEAN:
        vars[count].type = t_bool;
        vars[count].value.i = lua_toboolean(P, -1);
        break;

      case LUA_TNIL:
        vars[count].type = t_null;
        break;

      default:
        break;
    }

    count++;
    lua_pop(P, 1);
  }

  return count;
}

Parser parser_create(const char *filename)
{
  Script S = luaL_newstate();
  luaL_openlibs(S);

  return (luaL_loadfile(S, filename) || lua_pcall(S, 0, 0, 0)) ? NULL : S;
}

void parser_destroy(Parser P)
{
  lua_close(P);
}

void parser_set_key (Parser P, const char *key)
{
  lua_pushstring(P, key);
  lua_gettable(P, -2);
}

const char * parser_get_string (Parser P, int element, const char *varname)
{
  parser_set_key(P, varname);

  if (!lua_isstring(P, -1))
    parser_bail (P, "%s is not valid for element %d in table `tree'\n", 
        varname, element);

  const char *string = lua_tostring(P, -1);
  lua_pop(P, 1);

  return string;
}

int parser_get_integer (Parser P, int element, const char *varname)
{
  parser_set_key(P, varname);

  if (!lua_isnumber(P, -1))
    parser_bail (P, "%s is not valid for element %d in table `tree'\n", 
        varname, element);

  int number = (int) lua_tonumber(P, -1);
  lua_pop(P, 1);

  return number;
}

void parser_push_table (Parser P, int element, const char *table)
{
  parser_set_key(P, table);

  if (!lua_istable(P, -1))
    parser_bail (P, "`%s' is not a table in element %d!\n", table, element);

  lua_pushnil(P);
}

void parser_create_tree (Parser P)
{
  int element_index = 0;
  int component_index = 0;

  Variable vars[MAX_ENV_VARS];
  int var_count;

  int parent_id;
  const char *name;
  const char *script;

  Instance *instance;
  Component component;

  lua_getglobal(P, "tree");
  if (!lua_istable(P, -1))
    parser_bail (P, "`tree' is not a table\n");

  /* each element has no key */
  lua_pushnil(P);

  /* iterate through this table of tables */
  while (lua_next(P, -2)) {

    if (!lua_istable(P, -1))
      parser_bail (P, "Element %d of table `tree' is not a table!\n", element_index);

    /* get name and parent and then create our instance */
    name = parser_get_string(P, element_index, "name");
    parent_id = parser_get_integer(P, element_index, "parent");
    instance = instance_create(name, parent_id);

    if (instance == NULL)
      parser_bail (P, "Element %d is passed the max elements of %d\n", element_index,
          MAX_INSTANCES);

    /* get the table where that's named `components' */
    parser_push_table(P, element_index, "components");

    /* go through each component */
    while (lua_next(P, -2)) {

      if (!lua_istable(P, -1))
        parser_bail (P, 
          "Component %d of Element %d of table `tree' is not a table!\n",
          component_index, element_index);

      script = parser_get_string(P, element_index, "script");

      parser_push_table(P, element_index, "env");

      /* now that our table is setup, grab our env variables */
      if((var_count = parser_get_vars(P, MAX_ENV_VARS, vars)) < 0)
        parser_bail (P, 
            "Too many environment variables for component %d of element %d in table `tree'\n",
            component_index, element_index);

      /* attempt to create the component */
      if (component_create(&component, script, var_count, vars))
        parser_bail (P, "Component %d of element %d could not be loaded!\n", 
            component_index, element_index);

      /* attempt to add the component to our instance */
      if (instance_add_component(instance, component))
        parser_bail (P, "Too many components for element %d \n", element_index);

      /* pop next key and the env table */
      lua_pop(P, 2);
    }

    /* pop next key and components table */
    lua_pop(P, 2);
  }

  /* pop our nil key */
  lua_pop(P, 1);
}
