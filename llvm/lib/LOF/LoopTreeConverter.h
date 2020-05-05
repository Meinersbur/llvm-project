#ifndef LLVM_LOF_LOOPTREECONVERTER_H
#define LLVM_LOF_LOOPTREECONVERTER_H

#include "Green.h"
#include "GreenBuilder.h"
#include "llvm/Analysis/LoopInfo.h"


  namespace lof {
    class GreenConverter {
      DenseMap <llvm::Value*, GSymbol*> InputsVals;
      llvm::  Function* Func;
      llvm:: LoopInfo* LI;
      llvm:: LLVMContext& LLVMCtx;


    public:
      GreenConverter(llvm::Function *Func,llvm::LoopInfo* LI) : Func(Func),  LI(LI), LLVMCtx(Func->getContext()) {}

      Green* build() {
        for (auto &Arg : Func->args()) {
          InputsVals[&Arg] = GSymbol::createLLVM(&Arg);
        }
        return buildOriginalLoop(nullptr, &Func->getEntryBlock(), GOpExpr::createTrueExpr());
      }

    private:
      GSymbol* getOrCreateSym(llvm::Value* LLVMVal) {
        assert(!isa<llvm::Constant>(LLVMVal));
        auto &Sym = InputsVals[LLVMVal];
        if (!Sym) {
          Sym = GSymbol::createLLVM( LLVMVal );
        }
        return Sym;
      }

      GExpr* getOrCreateRefExpr(llvm::Value* LLVMVal) {
        // FIXME: constants that may trap must be instructions
        // TODO: Uniquefy constant expressions
        if (auto C = dyn_cast<llvm::Constant>(LLVMVal))
          return GOpExpr::createConstExpr(C);
        return getOrCreateSym(LLVMVal);
      }

      Green* buildOriginalLoop(llvm::Loop* L, llvm::BasicBlock* Entry, GExpr* Cond);


    }; // class GreenConverter
  } // namespace lof


#endif /* LLVM_LOF_LOOPTREECONVERTER_H */
