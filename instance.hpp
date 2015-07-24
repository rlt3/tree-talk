#ifndef TT_INSTANCE_HPP
#define TT_INSTANCE_HPP

#include <vector>
#include "component.hpp"

class Instance 
{
private:
  static int count;

protected:
  void destroy_components();

public: 
  int id;
  int parent_id;
  std::string name;

  std::vector<Component> components;
  std::vector<Instance*> children;

  Instance (std::string s);
  ~Instance();

  Instance& message (int sender_id, std::string message);
  Instance& add_component (Component c);
  Instance& add_child (Instance *ins);

  Instance& clear_components ();

//template<typename Lambda>
//void each_component (Lambda f)
//{
//  for (Component &c : components)
//    f(components);
//}
};

#endif
