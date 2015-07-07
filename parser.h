#ifndef TT_PARSER_H
#define TT_PARSER_H

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include "component.h"
#include "instance.h"

typedef lua_State* Parser;

/*
 * Create a parser
 */
Parser parser_create(const char *filename);

/* 
 * Parse the tree and create the necessary instances and components.
 */
void parser_create_tree (Parser P);

/*
 * free the parser
 */
void parser_destroy(Parser P);

#endif
