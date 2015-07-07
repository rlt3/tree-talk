#include "instance.h"
#include <string.h>

#define CREATE 1
#define QUERY 0

/*
 * Single entry point for our statically allocated instances.
 */
Instance * instances()
{
  static Instance instances[MAX_INSTANCES];
  return instances;
}

/*
 * Returns the current index. If QUERYing, leaves index alone. Increments the
 * index if CREATEing. Index starts at 1 because 0 is reserved for the head of
 * tree.
 */
int instance_index (int do_increment)
{
  static int index = 1;
  return (do_increment ? index++ : index);
}

/*
 * Get current index, which also serves as the count.
 */
int instance_count ()
{
  return instance_index(QUERY);
}

/*
 * Get a pointer to a particular instance, NULL if given invalid id.
 */
Instance * instance_by_id (int id)
{
  if (id >= instance_index(QUERY))
    return NULL;

  return &instances()[id];
}

/*
 * In a While != NULL loop, loop through each component of an instance.
 */
Component * instance_each_component(Instance *ins)
{
  static int index = 0;

  if (index >= ins->component_count) {
    index = 0;
    return NULL;
  }

  return &ins->components[index++];
}

/*
 * In a While != NULL loop, loop through each child of an instance.
 */
Instance * instance_each_child(Instance *ins)
{
  static int index = 0;

  if (index >= ins->child_count) {
    index = 0;
    return NULL;
  }

  return ins->children[index++];
}

/*
 * Create the instance. Returns 0 if no problems, 1 otherwise.
 */
int instance_create (const char *name, int pid, int count)
{
  if (instance_count() + 1 >= MAX_INSTANCES)
    return 1;

  Instance * ins = instance_by_id(instance_index(CREATE));

  ins->id              = instance_count();
  ins->name            = name;
  ins->parent_id       = pid;
  ins->component_count = count;
  ins->child_count     = 0;

  Instance * parent = instance_by_id(pid);

  parent->children[parent->child_count++] = ins;

  return 0;
}

/*
 * Add a component to a given instance.
 */
int instance_add_component (Instance *ins, Component c)
{
  if (ins->component_count + 1 >= MAX_COMPONENTS)
    return 1;

  ins->components[ins->component_count++] = c;

  return 0;
}
