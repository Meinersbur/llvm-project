#include "LoopContext.h"

using namespace llvm;
using namespace lof;

LoopContext::LoopContext(LLVMContext &LLVMCtx) : LLVMCtx(LLVMCtx) {
  FalseExpr = GOpExpr::createFalseExpr();
  TrueExpr = GOpExpr::createTrueExpr();
}

