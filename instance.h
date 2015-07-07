#ifndef TT_INSTANCE_H
#define TT_INSTANCE_H

#include "component.h"

#define MAX_COMPONENTS 20
#define MAX_CHILDREN 20
#define MAX_INSTANCES 20

struct Instance {
  int id;
  int parent_id;
  const char *name;

  int component_count;
  struct Component components[MAX_COMPONENTS];

  int child_count;
  struct Instance *children[MAX_CHILDREN];
};

typedef struct Instance Instance;

/*
 * Single entry point for our statically allocated instances.
 */
Instance * instances();

/*
 * Get current index, which also serves as the count.
 */
int instance_count ();

/*
 * Get a pointer to a particular instance, NULL if given invalid id.
 */
Instance * instance_by_id (int);

/*
 * In a While != NULL loop, loop through each component of an instance.
 */
Component * instance_each_component(Instance *ins);

/*
 * In a While != NULL loop, loop through each child of an instance.
 */
Instance * instance_each_child(Instance *ins);

/*
 * Create the instance. Returns 0 if no problems, 1 otherwise.
 */
int instance_create (const char *, int, int);

/*
 * Add a component to a given instance.
 */
int instance_add_component (Instance *ins, Component c);

#endif
