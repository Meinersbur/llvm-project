#include "Dep.h"
#include "GreenBuilder.h"
#include "LoopTreeTransform.h"
#include "Red.h"
#include <llvm/Analysis/AliasSetTracker.h>
#include <llvm/Analysis/ScalarEvolution.h>

#define DEBUG_TYPE "lof-dep"

using namespace lof;

SymAccessRef lof::prevSymAccess(const RedRef &StartStmt, GSymbol *Sym,
                                SymAccessRef::AccessKind Kind, bool FindDefs,
                                bool FindUses) {
  RedRef CurStmt = StartStmt;
  auto CurGreen = cast<Green>(CurStmt.getGreen());
  const RedRef *CurParentStmt = CurStmt.getParent();
  auto CurParentIdx = CurStmt.getParentIdx();

  if (Kind == SymAccessRef::Use)
    goto CheckDef;
  if (Kind == SymAccessRef::Def)
    goto Next;

  while (true) {
  CheckDef:
    if (FindDefs && CurGreen->hasScalarWrite(Sym))
      return {CurStmt, Sym, SymAccessRef::Def};

  CheckUse:
    if (FindUses && CurGreen->hasScalarRead(Sym))
      return {CurStmt, Sym, SymAccessRef::Use};

  Next:
    CurParentIdx -= 1;

  CheckBeforeFirst:
    if (CurParentIdx < 0) {
      CurParentStmt = CurParentStmt->getParent();
      CurParentIdx = CurParentStmt->getGreen()->getNumChildren() - 1;
      goto CheckBeforeFirst;
    }

    CurStmt = CurParentStmt->getChild(CurParentIdx);
    CurGreen = cast<Green>(CurStmt.getGreen());
  }

  return SymAccessRef{RedRef(), Sym, SymAccessRef::NotApplicable};
}

SymAccessRef lof::nextSymAccess(const RedRef &StartStmt, GSymbol *Sym,
                                SymAccessRef::AccessKind Kind) {
  RedRef CurStmt = StartStmt;
  auto CurGreen = cast<Green>(CurStmt.getGreen());
  const RedRef *CurParentStmt = CurStmt.getParent();
  auto CurParentIdx = CurStmt.getParentIdx();

  if (Kind == SymAccessRef::Def)
    goto Next;
  if (Kind == SymAccessRef::Use)
    goto CheckDef;

  while (true) {
  CheckUse:
    if (CurGreen->hasScalarRead(Sym))
      return {CurStmt, Sym, SymAccessRef::Use};

  CheckDef:
    if (CurGreen->hasScalarWrite(Sym))
      return {CurStmt, Sym, SymAccessRef::Def};

  Next:
    CurParentIdx += 1;

  CheckEnd:
    if (CurParentIdx >= CurParentStmt->getNumChildren()) {
      CurParentStmt = CurParentStmt->getParent();
      CurParentIdx = 0;
      goto CheckEnd;
    }

    CurStmt = CurParentStmt->getChild(CurParentIdx);
    CurGreen = cast<Green>(CurStmt.getGreen());
  }

  return SymAccessRef{RedRef(), Sym, SymAccessRef::NotApplicable};
}

namespace {
class ArrayDetector : public GreenTreeTransform {
private:
  //  llvm:: ScalarEvolution& SE;
  DenseMap<GSymbol *, GSymbol *> BasePtrToArray;

  GSymbol *getArrayForBasePtr(GSymbol *BasePtr) {
    auto &Result = BasePtrToArray[BasePtr];
    if (!Result) {
      Result = GSymbol::createFromScratch("anarray", BasePtr->getType());
    }
    return Result;
  }

public:
  ArrayDetector(LoopContext &Ctx) : GreenTreeTransform(Ctx) {}

  Green *transformInstruction(Green *Node) override {
    if (Node->getOperation().getKind() != Operation::LLVMInst)
      return Node;

    auto LLVMInst =
        dyn_cast<llvm::Instruction>(Node->getOperation().getLLVMInst());
    if (!LLVMInst)
      return Node;
    if (!isa<llvm::LoadInst>(LLVMInst) && !isa<llvm::StoreInst>(LLVMInst))
      return Node;

    GExpr *PtrArg;
    GExpr *ValArg;

    bool IsStore;
    if (auto LI = dyn_cast<llvm::LoadInst>(LLVMInst)) {
      PtrArg = Node->getArguments()[LI->getPointerOperandIndex()];
      ValArg = Node->getAssignments()[0];
      IsStore = false;
    } else if (auto SI = dyn_cast<llvm::StoreInst>(LLVMInst)) {
      PtrArg = Node->getArguments()[SI->getPointerOperandIndex()];
      ValArg = Node->getArguments()[0];
      IsStore = true;
    } else {
      return Node;
    }

    // The array access "analysis"
    auto Gep = cast<GOpExpr>(PtrArg);
    assert(Gep->getOperation().getKind() == Operation::LLVMSpeculable);
    assert(isa<llvm::GetElementPtrInst>(Gep->getOperation().getLLVMInst()));
    assert(Gep->getNumArguments() == 2); // BasePtr and index
    auto BasePtr = Gep->getArguments()[0];
    auto ArrayIndex = Gep->getArguments()[1];

    // GreenBuilder Builder{ Ctx };
    Green *NewInst;
    if (IsStore) {
      NewInst = Green::createInstruction(
          Node->getName(), Operation(Operation::StoreArrayElt, nullptr),
          {BasePtr, ArrayIndex, ValArg}, {}, LLVMInst, Node);
    } else {
      NewInst = Green::createInstruction(
          Node->getName(), Operation(Operation::LoadArrayElt, nullptr),
          {BasePtr, ArrayIndex}, {cast<GSymbol>(ValArg)}, LLVMInst, Node);
    }

    return NewInst;
  }

}; // class ArrayDetector
}; // namespace

Green *lof::detectArrays(LoopContext &Ctx, Green *Root) {
  ArrayDetector Detector(Ctx);
  return cast<Green>(Detector.visit(Root));
}

namespace {
struct Reduction {
  GExpr *Init;
  GExpr *Summand;

  Green *Last;
  GSymbol *RedResult;
};

class ReductionDetector : public GreenTreeTransform {
public:
  ReductionDetector(LoopContext &Ctx) : GreenTreeTransform(Ctx) {}

  Green *transformLoop(Green *Node) override {
    if (Node->isUnit())
      return GreenTreeTransform::transformLoop(Node);

    auto NodeRoot = RedRef::createRoot(Node);
    DenseSet<GSymbol *> Recurrences{Node->getScalarRecurrences().begin(),
                                    Node->getScalarRecurrences().end()};
    SmallVector<Reduction, 4> FoundReductions;
    DenseSet<Green *> OldReduceLast;

    auto NumReds = Node->getNumEpilogueStmts();
    for (int i = Node->getNumSubStmt() - NumReds; i < Node->getNumSubStmt();
         i += 1) {
      auto Red = Node->getSubStmt(i);
      if (Red->getOperation().getKind() != Operation::ReduceLast)
        continue;

      auto RedResult = Red->getAssignment(0);
      auto RedSym = cast<GRefExpr>(Red->getArgument(
          0)); // TODO: Argument could have the Reduction operation as well
      if (!RedSym)
        continue;

      auto RedChild = NodeRoot.getChild(i);
      auto RedAcc = prevSymAccess(RedChild, RedSym, SymAccessRef::NotApplicable,
                                  true, false);
      auto RedPHI = dyn_cast_or_null<Green>(RedAcc.Stmt.getGreen());
      if (!RedPHI)
        continue;
      if (!RedPHI->isInstruction())
        continue;
      if (RedPHI->getOperation().getKind() != Operation::Select)
        continue;
      if (RedPHI->getNumArguments() != 3)
        continue;
      if (RedPHI->getArgument(0) != Node->getIsFirstIteration())
        continue;
      auto RedInit = RedPHI->getArgument(1);
      auto RedOp = dyn_cast<GOpExpr>(RedPHI->getArgument(2));

      if (!RedOp)
        continue;
      auto EltOp = RedOp->getOperation();
      if (!EltOp.isAssociative())
        continue;
      auto LHS = RedOp->getArgument(0);
      auto RHS = RedOp->getArgument(1);

      GRefExpr *RedRecurrence;
      GExpr *Summand;
      if (isa<GRefExpr>(LHS) && Recurrences.count(cast<GRefExpr>(LHS))) {
        RedRecurrence = cast<GRefExpr>(LHS);
        Summand = RHS;
      } else if (isa<GRefExpr>(RHS) && Recurrences.count(cast<GRefExpr>(RHS))) {
        RedRecurrence = cast<GRefExpr>(RHS);
        Summand = LHS;
      } else
        continue;

      Reduction R;
      R.Init = RedInit; // FIXME: Currently assumed to be 0
      R.Last = Red;
      R.RedResult = RedResult;
      R.Summand = Summand;
      FoundReductions.push_back(R);
      OldReduceLast.insert(Red);
    }

    if (FoundReductions.empty())
      return GreenTreeTransform::transformLoop(Node);

    // Rebuild node, replace ReduceLast with ReduceAdd
    // PHI might be unused after this, dead code elimination shoud remove it
    GreenBuilder Rebuilder(Ctx);

    auto NumChildren = Node->getNumChildren();
    for (int i = 0; i < NumChildren; i += 1) {
      auto Child = cast<Green>(Node->getChild(i));
      if (OldReduceLast.count(Child))
        continue;
      Child = cast<Green>(getDerived().visit(Child));
      Rebuilder.addStmt(Node->getSubCond(i), Child);
    }

    for (auto Red : FoundReductions) {
      auto Last = Red.Last;
      Rebuilder.addInstruction(
          (Twine(Last->getName()) + ".reduce").str(), nullptr,
          Operation(Operation::ReduceAdd, nullptr), {Red.Summand},
          {Red.RedResult}, Last->getOrigRange().first);
    }

    auto ExecCond = Node->getExecCond();
    GExpr *NewExecCond = cast<GExpr>(getDerived().visit(ExecCond));
    Rebuilder.setTransformationOf(Node);

    auto Result = Rebuilder.createLoop(
        Node->getName(), NewExecCond, Node->getOrigRange().first,
        Node->getOrigRange().second, Node->getIsFirstIteration(),
        Node->getCanonicalCounter());
    return GreenTreeTransform::transformLoop(Result);
  }

}; // class ReductionDetector
} // namespace

Green *lof::detectReductions(LoopContext &Ctx, Green *Root) {
  ReductionDetector Detector(Ctx);
  return cast<Green>(Detector.visit(Root));
}

namespace {

struct DataFlowAnalysisForReachableDefs {
  using DefMapTy = DenseMap<GSymbol *, SmallVector<std::pair<Red *, int>, 4>>;
  DefMapTy ValidDefs;

  // DenseSet<GSymbol*> KilledSinceTop;

  void visitRoot(Red *Node) { visit(Node); }

  // TODO: Honor kills
  // TODO: Honor computed scalars
  void visit(Red *Node) {
    if (Node->isRefExpr()) {
      auto Sym = cast<GRefExpr>(Node->getGreen());
      auto &ReachableDefs = ValidDefs[Sym];
      auto TopIsReachable = true; //! KilledSinceTop.count(Sym);
      // Node->dump();
      Node->internal_setReachableDefs(ReachableDefs, TopIsReachable);

      for (auto ReachDef : ReachableDefs) {
        ReachDef.first->internal_addPossibleUse(ReachDef.second, Node);
      }
    }

#if 0
      if (Node->isStmt() && cast<Green>(Node->getGreen())->hasComputedScalars()) {
        auto G = cast<Green>(Node->getGreen());
        for (auto Killed : G->getScalarKills()) {
          KilledSinceTop.insert(Killed);
        }
      }
#endif

    for (auto Child : Node->children()) {
      visit(Child);
    }

    if (Node->isInstruction()) {
      auto Inst = cast<Green>(Node->getGreen());
      for (auto p : llvm::enumerate(Inst->getAssignments())) {
        auto Sym = p.value();
        ValidDefs[Sym].push_back({Node, p.index()});
      }
    }
  }

}; // class DataFlowAnalysisForReachableDefs
} // namespace

void lof::computeReachableDefs(Red *Root) {
  DataFlowAnalysisForReachableDefs ReachDefAnalyzer;
  ReachDefAnalyzer.visit(Root);
}

namespace {
// TODO: Make part of ReachableDefs of the Red tree to not have instantiate it
class JohnnyDep : public Dep {
public:
  Kind getKind() const override { return Dep::JohnnyDep; }
  static bool classof(const Dep *Obj) {
    return Obj->getKind() == Dep::JohnnyDep;
  }
  static bool classof(const JohnnyDep *) { return true; }

private:
  bool IsScalar = true;

  Red *Src;
  Red *Dst;

public:
  JohnnyDep(Red *Src, Red *Dst) : Src(Src), Dst(Dst) {
    assert(Src->isInstruction());
    assert(Dst->isInstruction());
  }

  bool isScalar() const override { return IsScalar; }

  Red *getSrc() const { return Src; }
  Red *getDst() const { return Dst; }

}; // class JohnnyDep

void collectRedInstructions(Red *R, std::vector<Red *> &Result) {
  auto G = R->getGreen();
  if (G->isInstruction()) {
    Result.push_back(R);
    return;
  }

  for (auto C : R->children()) {
    collectRedInstructions(C, Result);
  }
}

/// Collect all red instructions
std::vector<Red *> getAllRedInstructions(GCommon *G) {
  std::vector<Red *> Result;
  collectRedInstructions(G->asRedRoot(), Result);
  return Result;
}
} // namespace

// TODO: algorithm should avoid O(n^2) be skipping over transitive dependencies
std::vector<Dep *> lof::getAllDependencies(Green *Root) {
  std::vector<Dep *> Result;
  std::vector<Red *> Reds = getAllRedInstructions(Root);
  auto N = Reds.size();

  // Make scalar dependencies
  for (auto R : Reds)
    for (auto Use : R->collectRedExprs())
      for (auto Def : Use->getReachablesDefs())
        Result.push_back(new JohnnyDep(Def.first, R));

  return Result;

  DenseMap<GSymbol *, SmallVector<Red *, 2>> Reads;
  DenseMap<GSymbol *, SmallVector<Red *, 2>> Writes;

  for (int i = 0; i < N; i++) {
    auto Src = Reds[i];
    auto GSrc = cast<Green>(Src->getGreen());
    for (int j = 0; j < i; j++) {
      auto Dst = Reds[j];
      auto GDst = cast<Green>(Dst->getGreen());
      // assert(GSrc->hasComputedScalars());
      // assert(GDst->hasComputedScalars());

      auto SrcWrites = GSrc->getScalarWrites();
      auto SrcReads = GSrc->getScalarReads();
      auto DstReads = GDst->getScalarReads();
      auto DstWrites = GDst->getScalarWrites();

      for (auto Def : SrcWrites) {
        Result.push_back(new JohnnyDep(Src, Dst));
      }
    }
  }

  return Result;
}

namespace {

// TODO: Checks only a singled dependence; must be much more efficient
class CheckDepVisitor final {
private:
  Red *Src;
  Red *Dst;

  bool EncounteredSrc = false;
  bool EncounteredDst = false;
  bool Passed = true;

private:
  bool isRepresenting(Red *Orig, const RedRef &Derived) {
    auto RepresentingG = cast<Green>(Orig->getGreen());
    assert(!RepresentingG->getTransformationOf() &&
           "Assuming the dependency is from the original tree without reused "
           "nodes (i.e. a tree, not a DAG)");

    auto DerivedG = cast<Green>(Derived.getGreen());
    while (DerivedG->getTransformationOf()) {
      DerivedG = DerivedG->getTransformationOf();
    }

    return RepresentingG == DerivedG;
  }

public:
  CheckDepVisitor(Red *Src, Red *Dst) : Src(Src), Dst(Dst) {
    assert(Src->isInstruction());
    assert(Dst->isInstruction());
  }

  bool isPassed() const { return Passed; }

  void visit(const RedRef &Node) {
    if (Node.isContainer())
      return visitContainer(Node);
    if (Node.isInstruction())
      return visitInstruction(Node);
    if (Node.isExpr())
      return visitExpr(Node);
    llvm_unreachable("unhandled case");
  }

  void visitContainer(const RedRef &Node) {
    for (auto &X : Node.children()) {
      visit(X);
    }
  }

  void visitInstruction(const RedRef &Node) {
    if (isRepresenting(Src, Node))
      EncounteredSrc = true;
    if (isRepresenting(Dst, Node)) {
      EncounteredDst = true;
      if (EncounteredSrc) {
        // Src before definition, i.e. dependency violation
        Passed = false;
      }
    }
  }

  void visitExpr(const RedRef &Node) { return; }

}; // class CheckDepVisitor

} // namespace

bool lof::checkDependencies(Green *NewRoot, ArrayRef<Dep *> Deps) {

  for (auto D : Deps) {
    auto Dep = cast<JohnnyDep>(D);
    CheckDepVisitor Checker(Dep->getSrc(), Dep->getDst());
    auto NewRedRoot = RedRef::createRoot(NewRoot);
    Checker.visit(NewRedRoot);
    if (!Checker.isPassed()) {
      LLVM_DEBUG(llvm::dbgs() << "Not preserved dependency\n");
      return false;
    }
  }

  return true;
}
