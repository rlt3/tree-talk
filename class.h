#ifndef CLASS_H
#define CLASS_H

#include <string.h>

#define MAX_CLASSES    256
#define MAX_COMPONENTS MAX_CLASSES

/*
 * Define our base class from which to create instances. Keep a count of how
 * many components there are along with the component filenames. This serves as
 * a reference so we can load and reload the files over and over again in the
 * instance object.
 */

struct Class
{
  int component_count;
  char *name;
  char *component_files[MAX_COMPONENTS];
};

struct Class * class_get_by_id (int);

/* 
 * create class from char array, int, and files. 0 return is successful, 1 is
 * error.
 */
int class_create (char*, int, char **);

#endif
