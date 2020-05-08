

#include "LoopOpt.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "Green.h"
#include "GreenBuilder.h"
#include "LoopTreeConverter.h"
#include "LoopTreeCodegen.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/LoopUtils.h"
#include "llvm/Analysis/AliasAnalysisEvaluator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/IR/IRBuilder.h"
#include "LoopTransform.h"
#include "RedRef.h"
#include "LoopTreeTransform.h"

using namespace llvm;
using namespace lof;

namespace {


  class UnrollAllOutermostLoops final : public GreenTreeTransform  {
  public:
    UnrollAllOutermostLoops(LoopContext& Ctx) : GreenTreeTransform(Ctx) {}

    static GCommon * run(LoopContext& Ctx, GCommon *Root) {
      UnrollAllOutermostLoops Transformer(Ctx);
      return Transformer.visit(Root);
    }

    Green *transformLoop(Green* Loop) override {
      return applyUnrollAndJam(Ctx,Loop, 4 );
    }


  }; // class UnrollAllOutermostLoops


 
class LoopOptimizerImpl : public LoopOptimizer {
private:
  Function *Func;

  LoopContext* Ctx;
  LoopInfo *LI;
  ScalarEvolution *SE;

  Green *createInst(Value *I);

  

public:
  LoopOptimizerImpl(Function *Func, LoopInfo *LI, ScalarEvolution *SE)
      : Func(Func), LI(LI), SE(SE) {
  
    Ctx = new LoopContext( Func->getContext() );
  }



  Green* buildOriginalLoopTree() {
    GreenConverter Converter(*Ctx, Func, LI );
    return Converter.build();
  }


  bool optimize() override {
    auto OrigTree = buildOriginalLoopTree();
    OrigTree->dump();

   auto NewTree = cast<Green>( UnrollAllOutermostLoops::run(*Ctx, OrigTree));
   NewTree->dump();

#if 0
    // Try to unroll ever top-level loop

   
  auto  NewTreeBuilder = RedRef::createRoot(OrigTree).dfs<GreenBuilder>(
      [](const RedRef& R, GreenBuilder& ParentResult, bool& ContinueChildren, bool& ContinueSiblings, bool& ContinueTree) -> GreenBuilder {},
      [](const RedRef& R, GreenBuilder& ParentResult, GreenBuilder&& Result, bool& ContinueSiblings, bool& ContinueTree) {}
    );

 //   auto NewTree = applyUnrollAndJam(*Ctx, OrigTree, 4);
  auto NewTree = NewTreeBuilder.createStmt( OrigTree->getOrigRange().first, OrigTree->getOrigRange().second );
#endif

    GreenCodeGen CG(NewTree,Func->getParent(), Func->getContext());
    CG.replaceOrig(OrigTree);

    return true;
  }



  void view( Green* Root) {
    ViewGraph< Green *>(Root, "lof", false, "Loop Hierarchy Graph");
  }



  void print(raw_ostream &OS) override { OS << "Nothing to print yet\n"; }
};
} // namespace




LoopOptimizer *lof::createLoopOptimizer(Function *Func, LoopInfo *LI,  ScalarEvolution *SE) {
  return new LoopOptimizerImpl(Func, LI, SE);
}


