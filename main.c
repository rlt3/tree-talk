#include "instance.h"

void load_classes (const char *filename)
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
          "Element %d of table classes is not a table!\n",
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

int main (void)
{
  load_classes("load.lua");

  int id = 0;
  struct Class * class = class_get_by_id(id);

  while (class != NULL) {
    printf("%s: {", class->name);

    int i;
    for (i = 0; i < class->component_count; i++) {
      printf(" %s ", class->component_files[i]);
    }

    printf("}\n");

    id++;
    class = class_get_by_id(id);
  }

  return 0;
}
