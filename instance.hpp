#ifndef TT_INSTANCE_HPP
#define TT_INSTANCE_HPP

#include <vector>
#include "component.hpp"

class Instance 
{
  static int count;

public: 
  int id;
  int parent_id;
  std::string name;

  std::vector<Component> components;
  std::vector<Instance*> children;

  Instance (std::string s);
  ~Instance();

  void message (int sender_id, std::string message);
  void add_component (Component c);
  void add_child (Instance *ins);
};

#endif
