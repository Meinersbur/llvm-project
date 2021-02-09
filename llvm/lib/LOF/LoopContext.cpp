#include "LoopContext.h"

using namespace lof;

#if 0
 unsigned  ExprValInfo::getHashValue(GExpr* Val) {
   if (isa<GSymbol>(Val))
     return DenseMapInfo<GSymbol*>::getHashValue(cast<GSymbol>( Val));

   return 0;
}

bool ExprValInfo:: isEqual(GExpr* LHS, GExpr* RHS) {
  if (LHS == RHS)
    return true;
  if (isa<GRefExpr>(LHS) || isa<GRefExpr>(RHS))
    return false;

  

  return false;
}
#endif

LoopContext::LoopContext(llvm::LLVMContext &LLVMCtx) : LLVMCtx(LLVMCtx) {
  FalseExpr = GOpExpr::createFalseExpr();
  TrueExpr = GOpExpr::createTrueExpr();
}

GOpExpr *LoopContext::getConst(int Val) {
  auto C = llvm::ConstantInt::get(
      LLVMCtx, llvm::APInt(sizeof(Val) * CHAR_BIT, Val, true));
  return GOpExpr::createOp(Operation(Operation::LLVMSpeculable, C), {});
}

GSymbol *LoopContext::createSymbol(StringRef Name, llvm::Type *Ty) {
  return GSymbol::createFromScratch(Name, Ty);
}