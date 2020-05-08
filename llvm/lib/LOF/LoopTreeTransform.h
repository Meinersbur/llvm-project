#ifndef LLVM_LOF_LOOPTREETRANFORM_H
#define LLVM_LOF_LOOPTREETRANFORM_H

#include "Green.h"
#include "RedRef.h"


namespace lof {

  template<typename DerivedT, typename ResultT>
  class GreenRecursiveVisitor {
  private:
    DerivedT& getDerived() { return *static_cast<DerivedT*>(this); }
    const DerivedT& getDerived() const  { return *static_cast<const DerivedT*>(this); }

  protected:
    ResultT Visit(GCommon* Node) {
      return Visit( RedRef::createRoot(Node))
    }


    ResultT Visit(const RedRef& Node) {
      
    }
  }; // class GreenRecursiveVisitor



  /// Transformer for when the transformation does not depend on parents
  //template<typename DerivedT>
  class GreenTreeTransform {
  protected:
    using DerivedT = GreenTreeTransform;
    DerivedT& getDerived() { return *static_cast<DerivedT*>(this); }
    const DerivedT& getDerived() const { return *static_cast<const DerivedT*>(this); }

  protected:
    LoopContext& Ctx;
    DenseMap<GCommon*, GCommon*> Replacement;

  public:
    GreenTreeTransform(LoopContext& Ctx) : Ctx(Ctx) {}

    GCommon* visit(GCommon* Node) {
      assert((!Replacement.count(Node) || Replacement.lookup(Node))  && "Loop in tree??");
      auto& Result = Replacement[Node];
      if (!Result) {
        Result = getDerived().transform(Node);
      }
      return Result;
    }

     GCommon* transform(GCommon* Node) {
      if (Node->isLoop())
        return getDerived(). transformLoop(cast<Green>( Node));
      if (Node->isInstruction())
        return getDerived(). transformInstruction(cast<Green>(Node));
      if (Node->isStmt()) 
        return getDerived(). transformStmt(cast<Green>(Node));
      if (isa<GSymbol>(Node))
        return getDerived().transformRefExpr( cast<GSymbol>(Node));
      if (Node->isExpr()) 
        return getDerived(). transformExpr(cast<GOpExpr>(Node));
      llvm_unreachable("unhandled case");
    }

    virtual Green* transformStmt(Green* Node) {
      return getDerived().transfromStmtOrLoop(Node, false);
    }

     virtual Green* transformLoop(Green* Node) {
      return getDerived().transfromStmtOrLoop(Node, false);
    }

     Green* transfromStmtOrLoop(Green* Node, bool IsLoop);

    void transformArguments(GCommon *Node, SmallVectorImpl<GExpr*> &RecreatedArgs, bool &Changed) {
      ArrayRef<GExpr*> Arguments = isa<GOpExpr>(Node) ? cast<GOpExpr>(Node)->getArguments() : cast<Green>(Node)->getArguments();
      RecreatedArgs.clear();
      RecreatedArgs.reserve(Arguments.size());
      for (auto Arg : Arguments) {
        auto NewArg = getDerived().visit(Arg);
        RecreatedArgs.push_back(cast<GExpr>( NewArg));
        if (Arg != NewArg)
          Changed = true;
      }
    }

    virtual Green* transformInstruction(Green* Node) {
      assert(Node->isInstruction());
      bool Changed = false;

      SmallVector<GExpr*, 8> RecreatedArgs;
      transformArguments(Node,RecreatedArgs, Changed );

      auto Op = Node->getOperation();

      if (!Changed)
        return Node;
    return Green::createInstruction(Op, RecreatedArgs, Node->getAssignments(), Node->getOrigRange().first);
    }

    virtual    GOpExpr* transformExpr(GOpExpr* Node) {
      bool Changed = false;
      SmallVector<GExpr*, 8> RecreatedArgs;
      transformArguments(Node,RecreatedArgs, Changed );

      if (!Changed)
        return Node;
      
      auto Op = Node->getOperation();
      return GOpExpr::create(Op,  RecreatedArgs );
    }

    virtual  GSymbol* transformRefExpr(GSymbol* Node) {
      return Node;
    }

  }; // class GreenTreeTransform



     /// Transformer for when the transformation result depends on what its parent is.
  template<typename DerivedT>
  class RedTreeTransform  {
  protected:
    DerivedT& getDerived() { return *static_cast<DerivedT*>(this); }
    const DerivedT& getDerived() const  { return *static_cast<const DerivedT*>(this); }

  protected:
    LoopContext& LoopCtx;

  protected:
    RedTreeTransform(LoopContext& LoopCtx) : LoopCtx(LoopCtx) {}

    GCommon* Visit(GCommon* Node) {
      return Visit(RedRef::createRoot(Node))
    }


    GCommon* Visit(const RedRef& Node) {
      if (Node.isLoop()) 
        return VisitLoop(Node);
      if (Node.isInstruction())
        return VisitInstruction(Node);
    }


    GCommon* VisitLoop(const RedRef &Node) {

    }

    GCommon* VisitStmt(GCommon* Node) {

    }

    GCommon* VisitExpr(GCommon* Node) {

    }


  }; // class LoopTreeTransform

} // namespace lof
#endif /* LLVM_LOF_LOOPTREETRANFORM_H */
