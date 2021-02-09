#include "llvm/LOF/LoopOpt.h"
#include "Dep.h"
#include "GreenBuilder.h"
#include "LoopTransform.h"
#include "LoopTreeCodegen.h"
#include "LoopTreeConverter.h"
#include "LoopTreeTransform.h"
#include "Red.h"
#include "RedRef.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/AliasAnalysisEvaluator.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/LOF/Green.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/LoopUtils.h"

using namespace llvm;
using namespace lof;

#define DEBUG_TYPE "lof-loopopt"

namespace {
class UnrollAllOutermostLoops final : public GreenTreeTransform {
private:
  int Factor;

public:
  UnrollAllOutermostLoops(LoopContext &Ctx, int Factor)
      : GreenTreeTransform(Ctx), Factor(Factor) {}

  static GCommon *run(LoopContext &Ctx, GCommon *Root, int Factor) {
    UnrollAllOutermostLoops Transformer(Ctx, Factor);
    return Transformer.visit(Root);
  }

  Green *transformLoop(Green *Loop) override {
    return applyUnrollAndJam(Ctx, Loop, 2);
  }
}; // class UnrollAllOutermostLoops

class LoopOptimizerImpl : public LoopOptimizer {
private:
  Function *Func;

  LoopContext *Ctx;
  LoopInfo *LI;
  ScalarEvolution *SE;

public:
  LoopOptimizerImpl(Function *Func, LoopInfo *LI, ScalarEvolution *SE)
      : Func(Func), LI(LI), SE(SE) {

    Ctx = new LoopContext(Func->getContext());
  }

  Green *OrigTree = nullptr;
  Green *getOriginalRoot() override {
    if (!OrigTree) {
      GreenConverter Converter(*Ctx, Func, LI);
      OrigTree = Converter.build();
    }
    return OrigTree;
  }

  Green *normalize(Green *Root) override {
    auto Result = detectArrays(*Ctx, OrigTree);
    auto DetRed = detectReductions(*Ctx, Result);
    return DetRed;
  }

  bool optimize() override {
    auto OrigTree = getOriginalRoot();
    OrigTree->dump();
    OrigTree->asRedRoot()->dump();

    OrigTree = normalize(OrigTree);
    OrigTree->dump();

    computeReachableDefs(OrigTree->asRedRoot());

    auto OrigDeps = getAllDependencies(OrigTree);

    if (!checkDependencies(OrigTree, OrigDeps)) {
      LLVM_DEBUG(
          dbgs() << "Unmodified tree does not preserve dependencies???\n");
      return false;
    }

    auto NewTree = cast<Green>(UnrollAllOutermostLoops::run(*Ctx, OrigTree, 2));
    NewTree->dump();

    auto NewTree2 = cast<Green>(UnrollAllOutermostLoops::run(*Ctx, NewTree, 2));
    NewTree2->dump();

#if 0
    // Try to unroll ever top-level loop
  auto  NewTreeBuilder = RedRef::createRoot(OrigTree).dfs<GreenBuilder>(
      [](const RedRef& R, GreenBuilder& ParentResult, bool& ContinueChildren, bool& ContinueSiblings, bool& ContinueTree) -> GreenBuilder {},
      [](const RedRef& R, GreenBuilder& ParentResult, GreenBuilder&& Result, bool& ContinueSiblings, bool& ContinueTree) {}
    );

 //   auto NewTree = applyUnrollAndJam(*Ctx, OrigTree, 4);
  auto NewTree = NewTreeBuilder.createStmt( OrigTree->getOrigRange().first, OrigTree->getOrigRange().second );
#endif

    // Confirm that all dependencies in OrigDeps are honored in NewTree.
    if (!checkDependencies(NewTree2, OrigDeps)) {
      LLVM_DEBUG(dbgs() << "Not passing dependency check\n");
      return false;
    }

    GreenCodeGen CG(NewTree2, Func->getParent(), Func->getContext());
    CG.replaceOrig(OrigTree);

    return true;
  }

  void view(Green *Root) {
    ViewGraph<GCommon *>(Root, "lof", false, "Loop Hierarchy Graph");
  }

  void print(raw_ostream &OS) override { OS << "Nothing to print yet\n"; }
};
} // namespace

LoopOptimizer *lof::createLoopOptimizer(Function *Func, LoopInfo *LI,
                                        ScalarEvolution *SE) {
  return new LoopOptimizerImpl(Func, LI, SE);
}
