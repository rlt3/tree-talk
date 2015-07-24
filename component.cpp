#include "component.hpp"

std::map<std::string, CFunctionPtr> Component::cfunctions;

void Component::add_env_var (Variable var)
{
  variables.push_back(var);
}

void Component::clear_env_vars ()
{
  variables.clear();
}

/*
 * Set the sender and send a message.
 */
void Component::message (int sender_id, std::string message)
{
  set_sender_id(sender_id);
  script_call_function(script, message.c_str());
}

/*
 * Attempt to load the script from the given file.
 */
void Component::load ()
{
  Script S = script_new();

  if (script_load(S, filename.c_str())) {
    script_destroy(S);
    return;
  }

  for (Variable const &var : variables) {
    switch(var.type) {
    case t_int:
      script_set_int(S, var.name, var.value.i);
      break;

    case t_float:
      script_set_float(S, var.name, var.value.f);
      break;

    case t_string:
      script_set_string(S, var.name, var.value.s);
      break;

    case t_bool:
      script_set_boolean(S, var.name, var.value.i);
      break;

    case t_null:
      script_set_null(S, var.name);
      break;

    default:
      break;
    }
  }

  /* pair.second -> c function pointer, pair.first -> function name */
  for (auto const &pair : cfunctions) {
    script_set_cfunction(S, pair.second, pair.first.c_str());
  }

  script = S;
}

void Component::destroy()
{
  script_destroy(script);
}

void Component::set_sender_id (int id)
{
  script_set_int (script, "sender_id", id);
}

void Component::set_object_id (int id)
{
  script_set_int (script, "object_id", id);
}

/* static */
void Component::register_cfunction (std::string name, CFunctionPtr f)
{
  Component::cfunctions.insert(std::pair<std::string, CFunctionPtr>(name, f));
}
