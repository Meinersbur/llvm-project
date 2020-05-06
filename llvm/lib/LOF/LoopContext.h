#ifndef LLVM_LOF_LOOPCONTEXT_H
#define LLVM_LOF_LOOPCONTEXT_H

#include "Green.h"

namespace llvm {
  class LLVMContext;
} // namespace llvm

  namespace lof {
    class LoopContext {
    private:
     llvm:: LLVMContext& LLVMCtx;

      GOpExpr* FalseExpr;
      GOpExpr* TrueExpr;
    public:
      LoopContext() = delete;
      LoopContext(llvm::LLVMContext& LLVMCtx);

      llvm::LLVMContext& getLLVMContext() const { return LLVMCtx; }

      GOpExpr * getFalse() const { return FalseExpr; }
      GOpExpr *getTrue() const { return TrueExpr; }

      // Currently relies on LLVMContext
      // TODO: Canonicalize and intern expressions (like ScalarEvolution)
      GOpExpr* getConst(int Val) {
        auto C = llvm::ConstantInt::get( LLVMCtx ,llvm:: APInt(  Val, sizeof(Val)*CHAR_BIT)   );
        return GOpExpr::createOp (Operation(Operation::LLVMFloating, C), {  });
      }


      
    };

  }; // namespace lof
#endif /* LLVM_LOF_LOOPCONTEXT_H */
