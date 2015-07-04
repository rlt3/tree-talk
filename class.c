#include "class.h"

#include <stdlib.h>

static struct Class classes[MAX_CLASSES];
static int c_index = 0;

struct Class * class_get_by_id (int id)
{
  if (id >= c_index)
    return NULL;

  return &classes[id];
}

int class_create (char *name, int count, char **files)
{
  if (c_index + 1 >= MAX_CLASSES)
    return 1;

  classes[c_index].name = name;
  classes[c_index].component_count = count;
  memcpy(classes[c_index].component_files, files, count * sizeof(char *));

  c_index++;

  return 0;
}
