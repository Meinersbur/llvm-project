#ifndef LLVM_LOF_LOOPTREECONVERTER_H
#define LLVM_LOF_LOOPTREECONVERTER_H

#include "GreenBuilder.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/LOF/Green.h"

namespace lof {

class GreenConverter {
  DenseMap<llvm::Value *, GSymbol *> InputsVals;
  DenseMap<llvm::Value *, GExpr *> InputsExprs;
  llvm::Function *Func;
  llvm::LoopInfo *LI;
  LoopContext &Ctx;

public:
  GreenConverter(LoopContext &Ctx, llvm::Function *Func, llvm::LoopInfo *LI)
      : Func(Func), LI(LI), Ctx(Ctx) {
    assert(&Ctx.getLLVMContext() == &Func->getContext());
  }

  Green *build();

private:
  GSymbol *getOrCreateSym(llvm::Value *LLVMVal) {
    assert(!isa<llvm::Constant>(LLVMVal));
    auto &Sym = InputsVals[LLVMVal];
    if (!Sym) {
      Sym = GSymbol::createLLVM(LLVMVal);
      InputsExprs[LLVMVal] = Sym;
    }
    return Sym;
  }

  GExpr *getOrCreateRefExpr(llvm::Value *LLVMVal) {
    // FIXME: constants that may trap must be instructions
    // TODO: Uniquefy constant expressions
    if (auto C = dyn_cast<llvm::Constant>(LLVMVal))
      return GOpExpr::createConstExpr(C);
    return getOrCreateSym(LLVMVal);
  }

  GExpr *getExpr(llvm::Value *LLVMVal) {
    if (auto C = dyn_cast<llvm::Constant>(LLVMVal))
      return GOpExpr::createConstExpr(C);
    auto Operand = InputsExprs.lookup(LLVMVal);
    assert(Operand);
    return Operand;
  }

  Green *buildOriginalLoop(llvm::Loop *L, llvm::BasicBlock *Entry, GExpr *Cond,
                           GreenBuilder &ParentBuilder);

}; // class GreenConverter
} // namespace lof
#endif /* LLVM_LOF_LOOPTREECONVERTER_H */
