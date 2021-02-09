#include "llvm/LOF/LoopOptPass.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/iterator.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/Analysis/BlockFrequencyInfoImpl.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/PassManager.h"
#include "llvm/InitializePasses.h"
#include "llvm/LOF/LoopOpt.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <string>
#include <utility>

using namespace llvm;
using namespace lof;

namespace {
class LoopFrameworkOptimizer : public FunctionPass {
private:
  std::unique_ptr<LoopOptimizer> lo;

public:
  static char ID;

  // TODO: Make a SCC pass since it may outline functions
  LoopFrameworkOptimizer() : FunctionPass(ID) {
    initializeLoopFrameworkOptimizerPass(*PassRegistry::getPassRegistry());
  }

  /// @name FunctionPass interface
  //@{
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<LoopInfoWrapperPass>();
    AU.addRequired<ScalarEvolutionWrapperPass>();
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<OptimizationRemarkEmitterWrapperPass>();
    AU.addRequired<AAResultsWrapperPass>();

    // Required since transitive
    // AU.addPreserved<ScalarEvolutionWrapperPass>();
  }

  void releaseMemory() override { lo.reset(); }

  bool runOnFunction(Function &F) override {
    // Do not re-optimize our own output.
    if (F.hasFnAttribute("lof-generated"))
      return false;

    auto LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    auto SE = &getAnalysis<ScalarEvolutionWrapperPass>().getSE();
    lo.reset(createLoopOptimizer(&F, LI, SE));
    return lo->optimize();
  }

  void print(raw_ostream &OS, const Module *) const override {
    if (!lo) {
      OS << "Loop Hierachy Graph not built\n";
      return;
    }

    lo->print(OS);
  }
  //@}
};

} // namespace

char LoopFrameworkOptimizer::ID = 0;

INITIALIZE_PASS_BEGIN(LoopFrameworkOptimizer, "lof-opt",
                      "Loop Optimization Framework -- Optimizer", false, false)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_DEPENDENCY(OptimizationRemarkEmitterWrapperPass)
INITIALIZE_PASS_END(LoopFrameworkOptimizer, "lof-opt",
                    "Loop Optimization Framework -- Optimizer", false, false)

llvm::Pass *llvm::createLoopFrameworkOptimizerPass() {
  return new LoopFrameworkOptimizer();
}

namespace {
class LoopFrameworkAnalyzer : public FunctionPass {
private:
  std::vector<std::unique_ptr<lof::LoopOptimizer>> *List;

public:
  static char ID;
  LoopFrameworkAnalyzer()
      : FunctionPass(ID),
        List(new std::vector<std::unique_ptr<lof::LoopOptimizer>>()) {}

  // TODO: Make a SCC pass since it may outline functions
  LoopFrameworkAnalyzer(std::vector<std::unique_ptr<lof::LoopOptimizer>> &List)
      : FunctionPass(ID), List(&List) {
    initializeLoopFrameworkAnalyzerPass(*PassRegistry::getPassRegistry());
  }

  /// @name FunctionPass interface
  //@{
  void getAnalysisUsage(AnalysisUsage &AU) const override {
    AU.addRequired<LoopInfoWrapperPass>();
    AU.addRequired<ScalarEvolutionWrapperPass>();
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<OptimizationRemarkEmitterWrapperPass>();
    AU.addRequired<AAResultsWrapperPass>();

    // Required since transitive
    // AU.addPreserved<ScalarEvolutionWrapperPass>();
  }

  void releaseMemory() override {}

  bool runOnFunction(Function &F) override {
    auto LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    auto SE = &getAnalysis<ScalarEvolutionWrapperPass>().getSE();

    std::unique_ptr<LoopOptimizer> LOF{createLoopOptimizer(&F, LI, SE)};

    // Uses LI, might be deallocated when accessing List, so ensure the loop
    // graph is created now
    LOF->getOriginalRoot();

    List->push_back(std::move(LOF));
    return false;
  }

  void print(raw_ostream &OS, const Module *) const override {
    OS << "Generated graphs:\n";
    for (auto &L : *List) {
      if (!L)
        continue;
      L->print(OS);
    }
  }
  //@}
};

} // namespace

char LoopFrameworkAnalyzer::ID = 0;

INITIALIZE_PASS_BEGIN(LoopFrameworkAnalyzer, "lof-analyze",
                      "Loop Optimization Framework -- Analyzer", false, true)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_DEPENDENCY(OptimizationRemarkEmitterWrapperPass)
INITIALIZE_PASS_END(LoopFrameworkAnalyzer, "lof-analyze",
                    "Loop Optimization Framework -- Analyzer", false, true)

Pass *llvm::createLoopFrameworkAnalyzerPass(
    std::vector<std::unique_ptr<lof::LoopOptimizer>> &List) {
  return new LoopFrameworkAnalyzer(List);
}
