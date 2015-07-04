#include "instance.h"

static struct Instance head;
static struct Instance instances[MAX_CLASSES];
static int i_index = 0;

struct Instance * instance_get_by_id (int id)
{
  if (id >= i_index)
    return NULL;

  return &instances[id];
}

int instance_create (int parent_id, int class_id, int count, Script *comps)
{
  if (i_index + 1 >= MAX_CLASSES)
    return 1;

  instances[i_index].id = i_index;
  instances[i_index].class_id = class_id;
  instances[i_index].component_count = count;
  instances[i_index].child_count = 0;

  memcpy(instances[i_index].components, comps, count * sizeof comps);

  memset(instances[i_index].children, 0, 
      MAX_COMPONENTS * sizeof(struct Instance *));

  struct Instance * parent;

  if (parent_id == -1) {
    parent = &head;
  } else {
    parent = instance_get_by_id(parent_id);
  }

  parent->children[parent->child_count++] = &instances[i_index];

  i_index++;

  return 0;
}
