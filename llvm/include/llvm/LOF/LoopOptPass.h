#ifndef LLVM_LOF_LOOPOPTPASS_H
#define LLVM_LOF_LOOPOPTPASS_H

#include "llvm/PassRegistry.h"

namespace llvm {
  class Pass;

  void initializeLoopOptimizationFrameworkPass(PassRegistry &);
  Pass* createLoopOptimizationFrameworkPass();
} // namespace llvm

#endif /* LLVM_LOF_LOOPOPTPASS_H */
