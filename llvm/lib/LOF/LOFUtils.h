#ifndef LLVM_LOF_LOFUTILS_H
#define LLVM_LOF_LOFUTILS_H

#include "llvm/ADT/iterator.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/ArrayRef.h"

namespace llvm {


  template<typename T, typename C>
  Optional<ArrayRef<T>> make_optional_ArrayRef(const Optional<C > &Container) {
    if (!Container.hasValue())
      return None;
    ArrayRef<T>  ARef{ Container.getValue() };
    return  ARef;
  }


  template< typename T1, typename T2, typename IteratorCategoryT, typename T>
  class  iterator_union : public iterator_facade_base<iterator_union<T1,T2,IteratorCategoryT,T> , IteratorCategoryT, T>
  {
    // TODO: make a proper union like PointerUnion
    int Discriminator;
      T1 iter1;
      T2 iter2;

  public:
    iterator_union() = default;
    iterator_union(const T1 &Iter) : Discriminator(0), iter1(iter1) {}
    iterator_union(const T2 &Iter) :  Discriminator(1), iter2(iter2) {}
    iterator_union(const iterator_union& That) : Discriminator(That.Discriminator), iter1(That.iter1), iter2(That.iter2) {}

    iterator_union& operator=(const iterator_union& That) {
      Discriminator = That.Discriminator;
      iter1 = That.iter1;
      iter2 = That.iter2;
      return *this;
    }

    T operator*() const {
      switch (Discriminator) {
      case 0:
        return *iter1 ;
      case 1:
        return *iter2 ;
      }
    }

    bool operator==(const iterator_union& That) const {
      if (Discriminator != That.Discriminator)
        return false;
      switch (Discriminator) {
      case 0:
        return iter1 == That.iter1;
      case 1:
        return iter2 == That.iter2;
      }
    }

    iterator_union& operator+=(std::ptrdiff_t N) {
      switch (Discriminator) {
      case 0:
         iter1 += N;
         break;
      case 1:
        iter2 += N;
        break;
      }
      return *this;
    }

    iterator_union& operator-=(std::ptrdiff_t N) {
      switch (Discriminator) {
      case 0:
        iter1 -= N;
        break;
      case 1:
        iter2 -= N;
        break;
      }
      return *this;
    }

    std::ptrdiff_t operator-(const iterator_union& That) const {
      switch (Discriminator) {
      case 0:
        return iter1 - That.iter1;
      case 1:
        return iter2 - That.iter2;
      }
    }

    bool operator<(const iterator_union& That) const {
      switch (Discriminator) {
      case 0:
        return iter1  < That.iter1;
      case 1:
        return iter2 < That.iter2;
      }
    }
  }; // class iterator_union

} // namespace llvm
#endif /* LLVM_LOF_LOFUTILS_H */
