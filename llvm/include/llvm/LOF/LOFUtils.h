#ifndef LLVM_LOF_LOFUTILS_H
#define LLVM_LOF_LOFUTILS_H

#include "LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/iterator.h"

namespace lof {

template <typename T, typename C>
Optional<ArrayRef<T>> make_optional_ArrayRef(const Optional<C> &Container) {
  if (!Container.hasValue())
    return None;
  ArrayRef<T> ARef{Container.getValue()};
  return ARef;
}

template <typename DerivedT, typename ContainerT, typename T,
          typename DifferenceTypeT = std::ptrdiff_t, typename PointerT = T *,
          typename ReferenceT = T &>
class map_index_iterator
    : public llvm::iterator_facade_base<DerivedT,
                                        std::random_access_iterator_tag, T,
                                        DifferenceTypeT, PointerT, ReferenceT> {
protected:
  DerivedT &getDerived() { return *static_cast<DerivedT *>(this); }
  const DerivedT &getDerived() const {
    return *static_cast<const DerivedT *>(this);
  }

  ContainerT Container;
  size_t Idx;

  map_index_iterator(ContainerT Container, size_t Idx)
      : Container(Container), Idx(Idx) {
    //      static_assert(std::is_same<ReferenceT, decltype(
    //      ((DerivedT)nullptr)->operator*() )>::value, "operator*() must return
    //      a ReferenceT" );
    static_assert(
        std::is_same<ReferenceT, decltype(static_cast<DerivedT *>(this)->
                                          operator*())>::value,
        "operator*() must return ReferenceT");
    static_assert(
        std::is_same<ReferenceT, decltype(static_cast<const DerivedT *>(this)->
                                          operator*())>::value,
        "operator*() must return ReferenceT");
  }

#if 0
    map_index_iterator() {}
  

  public:
    map_index_iterator(const map_index_iterator& That) :  Container(That.Container),  Idx(That.Idx) {}
    map_index_iterator( map_index_iterator&& That) :  Container(That.Container),  Idx(That.Idx) {}

    red_child_iterator& operator=(const red_child_iterator& That) {
      this->Cur = That.Cur;
      return *this;
    }

    red_child_iterator& operator=( red_child_iterator&& That) {
      this->Cur = std::move(That.Cur);
      return *this;
    }
#endif

#if 0
    const  RedRef& operator*() const { return Cur; }
#endif

public:
  bool operator==(const DerivedT &That) const { return Idx == That.Idx; }

  DerivedT &operator+=(std::ptrdiff_t N) {
    Idx += N;
    return getDerived();
  }

  DerivedT &operator-=(std::ptrdiff_t N) {
    Idx -= N;
    return getDerived();
  }

  std::ptrdiff_t operator-(const DerivedT &That) const {
    return Idx - That.Idx;
  }

  bool operator<(const DerivedT &That) const { return Idx < That.Idx; }

}; // class map_index_iterator

template <typename T1, typename T2, typename IteratorCategoryT, typename T>
class iterator_union
    : public llvm::iterator_facade_base<
          iterator_union<T1, T2, IteratorCategoryT, T>, IteratorCategoryT, T> {
  // TODO: make a proper union like PointerUnion
  int Discriminator;
  T1 iter1;
  T2 iter2;

public:
  iterator_union() = default;
  iterator_union(const T1 &Iter) : Discriminator(0), iter1(iter1) {}
  iterator_union(const T2 &Iter) : Discriminator(1), iter2(iter2) {}
  iterator_union(const iterator_union &That)
      : Discriminator(That.Discriminator), iter1(That.iter1),
        iter2(That.iter2) {}

  iterator_union &operator=(const iterator_union &That) {
    Discriminator = That.Discriminator;
    iter1 = That.iter1;
    iter2 = That.iter2;
    return *this;
  }

  T operator*() const {
    switch (Discriminator) {
    case 0:
      return *iter1;
    case 1:
      return *iter2;
    }
  }

  bool operator==(const iterator_union &That) const {
    if (Discriminator != That.Discriminator)
      return false;
    switch (Discriminator) {
    case 0:
      return iter1 == That.iter1;
    case 1:
      return iter2 == That.iter2;
    }
  }

  iterator_union &operator+=(std::ptrdiff_t N) {
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

  iterator_union &operator-=(std::ptrdiff_t N) {
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

  std::ptrdiff_t operator-(const iterator_union &That) const {
    switch (Discriminator) {
    case 0:
      return iter1 - That.iter1;
    case 1:
      return iter2 - That.iter2;
    }
  }

  bool operator<(const iterator_union &That) const {
    switch (Discriminator) {
    case 0:
      return iter1 < That.iter1;
    case 1:
      return iter2 < That.iter2;
    }
  }
}; // class iterator_union

} // namespace lof
#endif /* LLVM_LOF_LOFUTILS_H */
