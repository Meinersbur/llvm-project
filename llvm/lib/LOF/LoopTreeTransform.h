#ifndef LLVM_LOF_LOOPTREETRANFORM_H
#define LLVM_LOF_LOOPTREETRANFORM_H

#include "RedRef.h"
#include "llvm/LOF/Green.h"

namespace lof {

template <typename DerivedT, typename ResultT> class GreenRecursiveVisitor {
private:
  DerivedT &getDerived() { return *static_cast<DerivedT *>(this); }
  const DerivedT &getDerived() const {
    return *static_cast<const DerivedT *>(this);
  }

protected:
  ResultT Visit(GCommon *Node) { return Visit(RedRef::createRoot(Node)); }

  ResultT Visit(const RedRef &Node) {}
}; // class GreenRecursiveVisitor

#if 0
  template<typename ResultT>
  struct RedRefVisitorData {
    const RedRef  Node;
    ResultT Data;
    RedRefVisitorData<ResultT>* ParentData;
  }; // struct RedRefVisitorData

  template<typename ResultT>
  class RedRefRecursiveVisitor {
    private:
      using DerivedT = GreenRecursiveVisitor;
      DerivedT& getDerived() { return *static_cast<DerivedT*>(this); }
      const DerivedT& getDerived() const  { return *static_cast<const DerivedT*>(this); }

    protected:
      ResultT visit() {
        RedRefVisitorData<ResultT> RootData;

      }

      ResultT visit(const RedRef& Node, RedRefVisitorData &ParentData) {
      }

  };  // class RedRefRecursiveVisitor
#endif

// template<typename DerivedT, typename ResultT, typename ParentT>
template <typename ResultT, typename ParentT> class RedRecursiveVisitor {
private:
  using DerivedT = RedRecursiveVisitor;
  DerivedT &getDerived() { return *static_cast<DerivedT *>(this); }
  const DerivedT &getDerived() const {
    return *static_cast<const DerivedT *>(this);
  }

private:
  bool Aborted = false;

public:
  void abortTraversal() { Aborted = true; }

protected:
  virtual ParentT previsitRoot(Red *Node, ParentT &NodeParentData) {
    return ParentT();
  }
  virtual ResultT postvisitRoot(Red *Node, ParentT &ParentData) {
    return ResultT();
  }

public:
  ResultT visitRoot(GCommon *Node) { return visitRoot(Node->asRedRoot()); }

  virtual ResultT visitRoot(Red *Node) {
    ParentT RootParentData = previsitRoot(Node);
    getDerived().visit(Node, RootParentData);
    return postvisitRoot(Node, RootParentData);
  }

protected:
  virtual ResultT visit(Red *Node, ParentT &ParentData) {
    if (Node->isContainer())
      return getDerived().visitContainer(Node, ParentData);
    if (Node->isInstruction())
      return getDerived().visitInstruction(Node, ParentData);
    if (Node->isExpr())
      return getDerived().visitExpr(Node, ParentData);
    llvm_unreachable("unhandled case");
  }

#if 0
    virtual void recurseArguments(Red *Node, ParentT &ParentData) {
      auto Green = Node->getGreen();
      ArrayRef<GExpr*> Arguments = isa<GOpExpr>(Green) ? cast<GOpExpr>(Green)->getArguments() : cast<Green>(Node)->getArguments();
      for (auto Arg : Arguments) 
        getDerived().visit(Arg, ParentData);
    }
#endif

  virtual void recurseChildren(Red *Node, ParentT &ParentData) {
    for (auto Child : Node->children()) {
      if (Aborted)
        return;
      getDerived().visit(Child, ParentData);
    }
  }

  virtual ParentT previsit(Red *Node, ParentT &NodeParentData) {
    return ParentT();
  }
  virtual ResultT postvisit(Red *Node, ParentT &ParentData) {
    return ResultT();
  }

  virtual ParentT previsitContainer(Red *Node, ParentT &NodeParentData) {
    return previsit(Node, NodeParentData);
  }
  virtual ResultT postvisitContainer(Red *Node, ParentT &ParentData) {
    return postvisit(Node, ParentData);
  }

  virtual ResultT visitContainer(Red *Node, ParentT &ParentData) {
    ParentT MyData = previsitContainer(Node, ParentData);
    recurseChildren(Node, ParentData);
    return postvisitContainer(Node, MyData);
  }

  virtual ParentT previsitInstruction(Red *Node, ParentT &NodeParentData) {
    return previsit(Node, NodeParentData);
  }
  virtual ResultT postvisitInstruction(Red *Node, ParentT &ParentData) {
    return postvisit(Node, ParentData);
  }

  virtual ResultT visitInstruction(Red *Node, ParentT &ParentData) {
    ParentT MyData = previsitInstruction(Node, ParentData);
    recurseChildren(Node, MyData);
    return postvisitInstruction(Node, MyData);
  }

  virtual ParentT previsitExpr(Red *Node, ParentT &NodeParentData) {
    return previsit(Node, NodeParentData);
  }
  virtual ResultT postvisitExpr(Red *Node, ParentT &ParentData) {
    return postvisit(Node, ParentData);
  }

  virtual ResultT visitExpr(Red *Node, ParentT &ParentData) {
    ParentT MyData = previsitExpr(Node);
    recurseChildren(Node, MyData);
    return postvisitExpr(Node, MyData);
  }

}; // class RedRecursiveVisitor

/// Transformer for when the transformation does not depend on parents
// template<typename DerivedT>
class GreenTreeTransform {
protected:
  using DerivedT = GreenTreeTransform;
  DerivedT &getDerived() { return *static_cast<DerivedT *>(this); }
  const DerivedT &getDerived() const {
    return *static_cast<const DerivedT *>(this);
  }

protected:
  LoopContext &Ctx;
  DenseMap<GCommon *, GCommon *> Replacement;

public:
  GreenTreeTransform(LoopContext &Ctx) : Ctx(Ctx) {}

  GCommon *visit(GCommon *Node) {
    assert((!Replacement.count(Node) || Replacement.lookup(Node)) &&
           "Loop in tree??");
    auto &Result = Replacement[Node];
    if (!Result) {
      Result = getDerived().transform(Node);
    }
    return Result;
  }

  GCommon *transform(GCommon *Node) {
    if (Node->isLoop())
      return getDerived().transformLoop(cast<Green>(Node));
    if (Node->isInstruction())
      return getDerived().transformInstruction(cast<Green>(Node));
    if (Node->isStmt())
      return getDerived().transformStmt(cast<Green>(Node));
    if (isa<GSymbol>(Node))
      return getDerived().transformRefExpr(cast<GSymbol>(Node));
    if (Node->isExpr())
      return getDerived().transformExpr(cast<GOpExpr>(Node));
    llvm_unreachable("unhandled case");
  }

  virtual Green *transformStmt(Green *Node) {
    return getDerived().transfromStmtOrLoop(Node, false);
  }

  virtual Green *transformLoop(Green *Node) {
    return getDerived().transfromStmtOrLoop(Node, true);
  }

  Green *transfromStmtOrLoop(Green *Node, bool IsLoop);

  void transformArguments(GCommon *Node,
                          SmallVectorImpl<GExpr *> &RecreatedArgs,
                          bool &Changed) {
    ArrayRef<GExpr *> Arguments = isa<GOpExpr>(Node)
                                      ? cast<GOpExpr>(Node)->getArguments()
                                      : cast<Green>(Node)->getArguments();
    RecreatedArgs.clear();
    RecreatedArgs.reserve(Arguments.size());
    for (auto Arg : Arguments) {
      auto NewArg = getDerived().visit(Arg);
      RecreatedArgs.push_back(cast<GExpr>(NewArg));
      if (Arg != NewArg)
        Changed = true;
    }
  }

  virtual Green *transformInstruction(Green *Node) {
    assert(Node->isInstruction());
    bool Changed = false;

    SmallVector<GExpr *, 8> RecreatedArgs;
    transformArguments(Node, RecreatedArgs, Changed);

    auto Op = Node->getOperation();

    if (!Changed)
      return Node;
    return Green::createInstruction(Node->getName(), Op, RecreatedArgs,
                                    Node->getAssignments(),
                                    Node->getOrigRange().first, Node);
  }

  virtual GOpExpr *transformExpr(GOpExpr *Node) {
    bool Changed = false;
    SmallVector<GExpr *, 8> RecreatedArgs;
    transformArguments(Node, RecreatedArgs, Changed);

    if (!Changed)
      return Node;

    auto Op = Node->getOperation();
    return GOpExpr::create(Op, RecreatedArgs);
  }

  virtual GSymbol *transformRefExpr(GSymbol *Node) { return Node; }

}; // class GreenTreeTransform

/// Transformer for when the transformation result depends on what its parent
/// is.
template <typename DerivedT> class RedTreeTransform {
protected:
  DerivedT &getDerived() { return *static_cast<DerivedT *>(this); }
  const DerivedT &getDerived() const {
    return *static_cast<const DerivedT *>(this);
  }

protected:
  LoopContext &LoopCtx;

protected:
  RedTreeTransform(LoopContext &LoopCtx) : LoopCtx(LoopCtx) {}

  GCommon *Visit(GCommon *Node) { return Visit(RedRef::createRoot(Node)); }

  GCommon *Visit(const RedRef &Node) {
    if (Node.isLoop())
      return VisitLoop(Node);
    if (Node.isInstruction())
      return VisitInstruction(Node);
  }

  GCommon *VisitInstruction(const RedRef &Node) {}

  GCommon *VisitLoop(const RedRef &Node) {}

  GCommon *VisitStmt(GCommon *Node) {}

  GCommon *VisitExpr(GCommon *Node) {}

}; // class LoopTreeTransform

} // namespace lof
#endif /* LLVM_LOF_LOOPTREETRANFORM_H */
