#include <string>
#include <iostream>
#include <type_traits>
#include <algorithm>
#include <functional>

template<typename T>
void print(T arg)
{
  std::cout << arg << std::endl;
}

/* Create an aligned block of Class T with a size of N */
template<class T, std::size_t N>
class Block 
{
  typename std::aligned_storage<sizeof(T), alignof(T)>::type data[N];
  std::size_t m_size = 0;

public:
  /* create an object by passing params as if you were constructing normally */
  template<typename ...Args> 
  void add(Args&&... args) 
  {
    if (m_size >= N)
      throw std::bad_alloc { };

    new (data+m_size) T(std::forward<Args>(args)...);
    ++m_size;
  }

  /* pass each object to the lambda */
  template<typename Lambda>
  void each(Lambda f)
  {
    for (std::size_t m = 0; m < m_size; m++)
      f((*this)[m]);
  }

  /* get object in aligned storage by the subscript operator */
  T& operator[](std::size_t pos)
  {
    return *reinterpret_cast<T*>(data+pos);
  }

  ~Block() 
  {
    for(std::size_t pos = 0; pos < m_size; ++pos)
      reinterpret_cast<const T*>(data+pos)->~T();
  }
};

class Component
{
  static int count;

public:
  int id;
  std::string filename;
  Component (std::string s) : filename(s) , id(Component::count++) { }

  ~Component()
  {
    /* clear component scripts */
  }
};

class Instance 
{
  static int count;

public: 
  int id;
  std::string name;
  Block<Component, 10> components;

  Instance (std::string s) : name(s) , id(Instance::count++) { }
};

int Instance::count = 0;
int Component::count = 0;

int 
main (void)
{
  Block<Instance, 22> instances;

  instances.add("test");
  instances.add("four");
  instances.add("twoe");

  instances.each([] (Instance &ins) { std::cout << ins.id << " -> " << ins.name << std::endl; });

  instances.each([] (Instance &ins) { ins.name = "change"; });
  
  instances.each([] (Instance &ins) { std::cout << ins.id << " -> " << ins.name << std::endl; });

  return 0;
}
