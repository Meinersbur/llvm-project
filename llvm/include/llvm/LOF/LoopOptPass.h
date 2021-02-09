#ifndef LLVM_LOF_LOOPOPTPASS_H
#define LLVM_LOF_LOOPOPTPASS_H

#include "llvm/PassRegistry.h"
#include <memory>
#include <vector>

namespace lof {
class LoopOptimizer;
}

namespace llvm {
class Pass;

void initializeLoopFrameworkOptimizerPass(PassRegistry &);
Pass *createLoopFrameworkOptimizerPass();

void initializeLoopFrameworkAnalyzerPass(PassRegistry &);
Pass *createLoopFrameworkAnalyzerPass(
    std::vector<std::unique_ptr<lof::LoopOptimizer>> &);
} // namespace llvm

#endif /* LLVM_LOF_LOOPOPTPASS_H */
