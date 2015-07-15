#include "instance.hpp"

int Instance::count = 0;

Instance::Instance (std::string s) 
  : name(s)
  , id(Instance::count++)
{ }

Instance::~Instance ()
{
  for (Component &c : components)
    c.destroy();
}

void Instance::message (int sender_id, std::string message)
{
  for (Component &c : components)
    c.message(sender_id, message);
}

void Instance::add_component (Component c)
{
  c.set_object_id(id);
  components.push_back(c);
}

void Instance::add_child (Instance *ins)
{
  children.push_back(ins);
}
