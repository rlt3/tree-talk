#include "instance.h"
#include "script.h"
#include "class.h"

void print() 
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
  script_load_classes("load.lua");
  script_load_tree("load.lua");

  print();

  int id = 0;
  struct Class * class = class_get_by_id(id);

  while (class != NULL) {
    int i;
    for (i = 0; i < class->component_count; i++) {
      free(class->component_files[i]);
    }

    free(class->name);

    id++;
    class = class_get_by_id(id);
  }

  return 0;
}
