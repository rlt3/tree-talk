#ifndef TT_ALIGNED_STORAGE_HPP
#define TT_ALIGNED_STORAGE_HPP

/* Create an aligned block of Class T with a size of N */
template<class T, std::size_t N>
class AlignedStorage 
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

  ~AlignedStorage() 
  {
    for(std::size_t pos = 0; pos < m_size; ++pos)
      reinterpret_cast<const T*>(data+pos)->~T();
  }
};

#endif
