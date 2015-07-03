#include "instance.h"

static struct Instance instances[MAX_CLASSES];
static int instance_index = 0;

struct Instance * instance_get_by_id (int id)
{
  if (id >= instance_index)
    return NULL;

  return &instances[id];
}

int instance_create (int class_id, int count, Script *comps)
{
  if (instance_index + 1 >= MAX_CLASSES)
    return 1;

  instances[instance_index].class_id = class_id;
  instances[instance_index].component_count = count;

  memcpy(instances[instance_index].components, comps, count * sizeof comps);

  instance_index++;

  return 0;
}
