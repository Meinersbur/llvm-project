#ifndef LLVM_LOF_LOOPCONTEXT_H
#define LLVM_LOF_LOOPCONTEXT_H

#include "Green.h"

  namespace lof {
    class LoopContext {
    private:
      GOpExpr* FalseExpr;
      GOpExpr* TrueExpr;
    public:
      LoopContext();

      GOpExpr * getFalse() const { return FalseExpr; }
      GOpExpr *getTrue() const { return TrueExpr; }
    };

  }; // namespace lof

#endif /* LLVM_LOF_LOOPCONTEXT_H */
