#ifndef LLVM_LOF_REDREF_H
#define LLVM_LOF_REDREF_H

#include "llvm/LOF/Green.h"

namespace lof {
class RedRef;

/// RedRef separated into a subclass to allow the children() iterator.
/// All fields must be declared in RedRefImpl.
class RedRefImpl {
public:
  friend class RedRef;

protected:
  /// Only for assert builds?
  mutable bool Fix = false;

  GCommon *G = nullptr;
  const RedRef *Parent = nullptr;
  size_t ParentIdx = -1;

  RedRefImpl(GCommon *G, const RedRef *Parent, size_t ParentIdx)
      : G(G), Parent(Parent), ParentIdx(ParentIdx) {}
}; // class RedRefImpl

/// A Red tree, but using stack objects.
/// Must ensure that the parent Red node does not go out of scope while still
/// using any of its children! Useful for recursive algorithm, but impractical
/// to attach additional analysis to it.
class RedRef : public RedRefImpl {
public:
  RedRef() : RedRefImpl(nullptr, nullptr, -1) {}

#if 0
      /// No copy ctor to prevent accidental invalidation of That.
      RedRef(const RedRef& That) = delete;
      void operator =(const RedRef& That) = delete;

      /// Move ctor; don't move unless That is not yet fixed 
      RedRef(RedRef&& That) :  RedRefImpl(That.G, That.Parent,That.ParentIdx)  {
        assert(!That.Fix && "Must not move already referenced RedRef");
        That.G = nullptr;
      }

      void operator =(RedRef&&That) {
        assert(!Fix && "Cannot overwrite already referenced Parent");
      }
#endif

private:
  /// Create a root without any parent.
  explicit RedRef(GCommon *Root) : RedRefImpl(Root, nullptr, -1) {
    assert(Root);
  }

public:
  static RedRef createRoot(GCommon *G) {
    assert(G);
    return RedRef(G);
  }

private:
  /// Create a child
  /// Non-public: use parent methods
  RedRef(GCommon *G, const RedRef *Parent, size_t ParentIdx)
      : RedRefImpl(G, Parent, ParentIdx) {
    /// We are referencing Parent so it must not be moved anymore.
    Parent->Fix = true;
  }

public:
  size_t getNumChildren() const { return G->getNumChildren(); }
  RedRef getChild(size_t i) const {
    assert(i < G->getNumChildren());

    RedRef Result{G->getChild(i), this, i};
    return Result;
  }

public:
  class red_child_iterator
      : public llvm::iterator_facade_base<
            red_child_iterator, std::random_access_iterator_tag, RedRef> {
  public:
    friend class RedRef;
    friend class RedRefImpl;

  private:
    RedRefImpl Cur;

    explicit red_child_iterator(RedRef &&Cur) : Cur(std::move(Cur)) {}

  public:
    red_child_iterator(const red_child_iterator &That) : Cur(That.Cur) {}
    red_child_iterator(red_child_iterator &&That) : Cur(std::move(That.Cur)) {}

    red_child_iterator &operator=(const red_child_iterator &That) {
      this->Cur = That.Cur;
      return *this;
    }

    red_child_iterator &operator=(red_child_iterator &&That) {
      this->Cur = std::move(That.Cur);
      return *this;
    }

    const RedRef &operator*() const {
      return *static_cast<RedRef *>(const_cast<RedRefImpl *>(&Cur));
    }

    bool operator==(const red_child_iterator &That) const {
      // Note that one of the iterators can also be the end() marker, with G
      // being nullptr.
      assert(this->Cur.Parent == That.Cur.Parent);
      return this->Cur.ParentIdx == That.Cur.ParentIdx;
    }

    red_child_iterator &operator+=(std::ptrdiff_t N) {
      Cur.ParentIdx += N;
      return *this;
    }

    red_child_iterator &operator-=(std::ptrdiff_t N) {
      Cur.ParentIdx -= N;
      return *this;
    }

    std::ptrdiff_t operator-(const red_child_iterator &That) const {
      return Cur.ParentIdx - That.Cur.ParentIdx;
    }

    bool operator<(const red_child_iterator &That) const {
      return Cur.ParentIdx < That.Cur.ParentIdx;
    }
  }; // class red_child_iterato

  llvm::iterator_range<red_child_iterator> children() const {
    auto Num = getNumChildren();
    if (Num == 0)
      return llvm::make_range(red_child_iterator(RedRef{nullptr, this, 0}),
                              red_child_iterator(RedRef{nullptr, this, 0}));

    return llvm::make_range(red_child_iterator(getChild(0)),
                            red_child_iterator(RedRef{nullptr, this, Num}));
  }

public:
  bool isRoot() const { return Parent == nullptr; }
  const RedRef *getParent() const { return Parent; }
  auto getParentIdx() const { return ParentIdx; }
  auto getGreen() const { return G; }

  auto getCondInParent() const {
    return cast<Green>(Parent->getGreen())->getSubCond(ParentIdx);
  }

  auto getKind() const { return G->getKind(); }

  bool isContainer() const { return G->isContainer(); }
  bool isInstruction() const { return G->isInstruction(); }
  bool isExpr() const { return G->isExpr(); }

  bool isStmt() const { return G->isStmt(); }
  bool isLoop() const { return G->isLoop(); }

private:
  template <typename ResultT = void>
  void dfs_internal(
      const std::function<ResultT(
          const RedRef &R, ResultT &ParentResult, bool &ContinueChildren,
          bool &ContinueSiblings, bool &ContinueTree)> &PreorderCallback,
      const std::function<void(const RedRef &R, ResultT &ParentResult,
                               ResultT &&Result, bool &ContinueSiblings,
                               bool &ContinueTree)> &PostorderCallback,
      ResultT &ParentResult, bool &ContinueSiblings, bool &ContinueTree) const {

    bool VisitChildren = true;
    ResultT MyResult =
        PreorderCallback(*this, ParentResult, MyResult, VisitChildren,
                         ContinueSiblings, ContinueTree);
    if (!ContinueTree)
      return;

    if (VisitChildren) {
      bool ContinueMySiblings = true;
      for (auto &Child : children()) {
        dfs_internal(PreorderCallback, PostorderCallback, MyResult,
                     ContinueMySiblings, ContinueTree);
        if (!ContinueTree)
          return;
        if (!ContinueMySiblings)
          break;
      }
    }

    PostorderCallback(*this, ParentResult, std::move(MyResult),
                      ContinueSiblings, ContinueTree);
  }

public:
  template <typename ResultT = void>
  ResultT
  dfs(const std::function<ResultT(
          const RedRef &R, ResultT &ParentResult, bool &ContinueChildren,
          bool &ContinueSiblings, bool &ContinueTree)> &PreorderCallback,
      const std::function<void(const RedRef &R, ResultT &ParentResult,
                               ResultT &&Result, bool &ContinueSiblings,
                               bool &ContinueTree)> &PostorderCallback) const {
    bool ContinueTree = true;
    bool ContinueSiblings =
        true; // Root nodes has no siblings, so without effect

    // Actually, the root's 'parent'
    ResultT RootResult;

    dfs_internal(PreorderCallback, PostorderCallback, RootResult,
                 ContinueSiblings, ContinueTree);

    return RootResult;
  }

}; // class RedRef

} // namespace lof
#endif /* LLVM_LOF_REDREF_H */
