#include "class.h"

static struct Class classes[MAX_CLASSES];
static int class_index = 0;

struct Class * class_get_by_id (int id)
{
  if (id >= class_index)
    return NULL;

  return &classes[id];
}

int class_create (const char* name, int count, const char **files)
{
  if (class_index + 1 >= MAX_CLASSES)
    return 1;

  classes[class_index].name = name;
  classes[class_index].component_count = count;

  memcpy(classes[class_index].component_files, files, count * sizeof files);

  class_index++;

  return 0;
}
