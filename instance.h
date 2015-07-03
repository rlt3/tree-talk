#ifndef INSTANCE_H
#define INSTANCE_H

#include "script.h"
#include "class.h"

/*
 * Our instance implementation of our classes. Attach the internal id so that
 * internal functions can message between objects (not broadcasting) at an O(1)
 * efficiency. 
 *
 * Class id serves as reference our component count serves as convenience so 
 * that we don't have to keep looking up our class just to see the component
 * count.
 */

struct Instance
{
  int id;
  int class_id;
  int component_count;
  Script components[MAX_COMPONENTS];
};

struct Instance * instance_get_by_id (int);

/* Attempt to create instance. 0 return is successful, 1 is error. */
int instance_create (int, int, Script *);

#endif
