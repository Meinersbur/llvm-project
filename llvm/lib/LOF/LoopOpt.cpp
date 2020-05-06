

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

using namespace llvm;
using namespace lof;

namespace {


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

 

    GreenCodeGen CG(OrigTree,Func->getParent(), Func->getContext());
    CG.replaceOrig(OrigTree );

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


