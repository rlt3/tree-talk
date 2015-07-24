#include "instance.hpp"

int Instance::count = 0;

Instance::Instance (std::string s) 
  : name(s)
  , id(Instance::count++)
{ }

Instance::~Instance ()
{
  destroy_components();
}

void Instance::destroy_components ()
{
  for (Component &c : components)
    c.destroy();
}

Instance& Instance::message (int sender_id, std::string message)
{
  for (Component &c : components)
    c.message(sender_id, message);

  return *this;
}

Instance& Instance::add_component (Component c)
{
  c.set_object_id(id);
  components.push_back(c);
  return *this;
}

Instance& Instance::add_child (Instance *ins)
{
  children.push_back(ins);
  return *this;
}

Instance& Instance::clear_components ()
{
  destroy_components();
  components.clear();
  return *this;
}

