#ifndef LLVM_LOF_LOOPTREEBUILDER_H
#define LLVM_LOF_LOOPTREEBUILDER_H

#include "Green.h"
#include "GreenBuilder.h"
#include "llvm/Analysis/LoopInfo.h"





namespace llvm {
  namespace lof {


    class GreenConverter {
      DenseMap <Value*, GSymbol*> InputsVals;
      Function* Func;
      LoopInfo* LI;
      LLVMContext& LLVMCtx;


    public:
      GreenConverter(Function *Func,LoopInfo* LI) : Func(Func),  LI(LI), LLVMCtx(Func->getContext()) {}
      Green* build() {

        for (auto &Arg : Func->args()) {
          InputsVals[&Arg] = GSymbol::createLLVM(&Arg);
        }
        return buildOriginalLoop(nullptr, &Func->getEntryBlock(), GOpExpr::createTrueExpr());
      }

    private:
      GSymbol* getOrCreateSym(Value* LLVMVal) {
        assert(!isa<Constant>(LLVMVal));
        auto &Sym = InputsVals[LLVMVal];
        if (!Sym) {
          Sym = GSymbol::createLLVM( LLVMVal );
        }
        return Sym;
      }

      GExpr* getOrCreateRefExpr(Value* LLVMVal) {
        // FIXME: constants that may trap must be instructions
        // TODO: Uniquefy constant expressions
        if (auto C = dyn_cast<Constant>(LLVMVal))
          return GOpExpr::createConstExpr(C);
        return getOrCreateSym(LLVMVal);
      }

      Green* buildOriginalLoop(Loop* L, BasicBlock* Entry, GExpr* Cond);


    }; // class GreenConverter
  } // namespace lof
} // namespace llvm

#endif /* LLVM_LOF_LOOPTREEBUILDER_H */
