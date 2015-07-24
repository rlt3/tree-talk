#ifndef TT_COMPONENT_HPP
#define TT_COMPONENT_HPP

extern "C" {
  #include "script.h"
}

#include <vector>
#include <map>
#include <string>

union ScriptData
{
  int i;
  float f;
  const char *s;

  ScriptData (int v) : i(v) { }
  ScriptData (float v) : f(v) { }
  ScriptData (const char *v) : s(v) { }
};

enum ScriptTypes
{
  t_int, t_float, t_string, t_bool, t_null
};

struct Variable
{
  const char *name;
  enum ScriptTypes type;
  ScriptData value;

  Variable (const char* n, int v) : name(n), type(t_int), value(v) { }
  Variable (const char* n, float v) : name(n), type(t_float), value(v) { }
  Variable (const char* n, const char *v) : name(n), type(t_string), value(v) { }
};

class Component
{
protected:
  static std::map<std::string, CFunctionPtr> cfunctions;

  std::vector<Variable> variables;
  Script script;

public:
  std::string filename;

  Component(std::string f) : filename(f) { }

  /*
   * Register a C function in the form of CFunctionPtr that will be available
   * in all components.
   */
  static void register_cfunction (std::string name, CFunctionPtr f);

  /*
   * Set (or create) a variable for this component's script. 
   */
  void add_env_var (Variable var);

  /*
   * Remove all the environment variables.
   */
  void clear_env_vars ();

  /*
   * Attempt to load the component with the given environment and C functions.
   */
  void load ();

  /*
   * Set the sender's id and call a function with the name of `message'
   */
  void message (int sender_id, std::string message);

  /*
   * Set the sender id, usually just before sending a message.
   */
  void set_sender_id (int id);

  /*
   * Set the object id, which is the Instance each component is attached to.
   */
  void set_object_id (int id);

  /*
   * Instead of handling this in the destructor, which would cause the script
   * to get deallocated as a Component object was passed around or copied,
   * we let the Instance handle destroying each component in it's destructor.
   */
  void destroy ();
};

#endif
