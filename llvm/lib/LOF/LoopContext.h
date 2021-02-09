#ifndef LLVM_LOF_LOOPCONTEXT_H
#define LLVM_LOF_LOOPCONTEXT_H

#include "llvm/LOF/Green.h"

namespace llvm {
class LLVMContext;
} // namespace llvm

namespace lof {

#if 0
    struct ExprValInfo {
    static   bool isEqual(GExpr* LHS, GExpr* RHS);
    static unsigned getHashValue(GExpr *Val);
    static  GExpr* getEmptyKey() { return DenseMapInfo<GExpr*>::getEmptyKey(); }
    static GExpr* getTombstoneKey() { return DenseMapInfo<GExpr*>::getTombstoneKey(); }
    }; // ExprValInfo
#endif

class LoopContext {
private:
  llvm::LLVMContext &LLVMCtx;

  GOpExpr *FalseExpr;
  GOpExpr *TrueExpr;

#if 0
      llvm::DenseSet<GExpr*, ExprValInfo> Expressions;
      GExpr* uniqueExpr(GExpr* E) {
        return E; // Implement at some later point

        auto It =  Expressions.insert(E);
        return *It.first;
      }
#endif

public:
  LoopContext() = delete;
  LoopContext(llvm::LLVMContext &LLVMCtx);

  llvm::LLVMContext &getLLVMContext() const { return LLVMCtx; }
  llvm::Type *getBoolType() const { return llvm::Type::getInt1Ty(LLVMCtx); }
  llvm::Type *getGenericIntType() const { return nullptr; }

  GOpExpr *getFalse() const { return FalseExpr; }
  GOpExpr *getTrue() const { return TrueExpr; }

public:
  // Currently relies on LLVMContext
  // TODO: Canonicalize and intern expressions (like ScalarEvolution)
  GOpExpr *getConst(int Val);

  GSymbol *createSymbol(StringRef Name, llvm::Type *Ty);

}; // class LoopContext
} // namespace lof
#endif /* LLVM_LOF_LOOPCONTEXT_H */
