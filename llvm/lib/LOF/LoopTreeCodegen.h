#ifndef LLVM_LOF_LOOPTREECODEGEN_H
#define LLVM_LOF_LOOPTREECODEGEN_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/LOF/Green.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

namespace lof {
class GreenCodeGen {
  using BuilderTy = llvm::IRBuilder<>;

private:
  Green *Root;
  llvm::Module *M;
  llvm::LLVMContext &LLVMCtx;

  llvm::Function *Func = nullptr;
  BuilderTy AllocaBuilder;
  DenseMap<GSymbol *, llvm::Value *> SymbolPtrs;

  void emitSequence(Green *G, BuilderTy &Builder);
  void emitLoop(Green *G, BuilderTy &Builder);
  llvm::Value *emitOperation(const Operation &Op, ArrayRef<GExpr *> Arguments,
                             BuilderTy &Builder, bool IsExpr);
  llvm::Value *emitExpr(GExpr *G, BuilderTy &Builder);
  void emitInstruction(Green *G, BuilderTy &Builder);
  void emitGreen(GCommon *G, BuilderTy &Builder);

  llvm::Value *getPtr(GSymbol *Sym);

public:
  GreenCodeGen(Green *Root, llvm::Module *M, llvm::LLVMContext &C)
      : Root(Root), M(M), AllocaBuilder(C), LLVMCtx(C) {}

  std::tuple<llvm::Function *, std::vector<GSymbol *>, std::vector<GSymbol *>>
  emitAsFunction();

  void replaceOrig(Green *Orig);
};

} // namespace lof
#endif /* LLVM_LOF_LOOPTREECODEGEN_H */
