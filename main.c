#include "instance.h"
#include "script.h"
#include "class.h"

void print_class() 
{
  int id = 0;
  struct Class * class = class_get_by_id(id);

  while (class != NULL) {
    printf("%p| %s: {", class, class->name);

    int i;
    for (i = 0; i < class->component_count; i++) {
      printf(" %s ", class->component_files[i]);
    }

    printf("}\n");

    id++;
    class = class_get_by_id(id);
  }
}

int main (void)
{
  Script config = load_script_into_memory("load.lua");

  script_load_classes(config);
  script_load_tree(config);

  print_class();

  script_destroy(config);

  return 0;
}
