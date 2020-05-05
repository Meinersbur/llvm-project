#include "LoopContext.h"

using namespace llvm;
using namespace lof;

LoopContext::LoopContext() {
  FalseExpr = GOpExpr::createFalseExpr();
  TrueExpr = GOpExpr::createTrueExpr();
}

