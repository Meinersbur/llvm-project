#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

#define DEBUG_TYPE "parallel-aa"

using namespace llvm;

namespace {
struct ParallelAliasAnalysis : PassInfoMixin<ParallelAliasAnalysis> {

  class ParallelAliasAnalysisResult {
  public:
    // TODO: This only stores one llvm::Value, but different Values from
    // different iterations may not-alias as well, even though they might be the
    // same value in the same iteration.
    DenseSet<std::pair<Loop *, Value *>> KnownIvdep;
  };

  PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {
    LLVM_DEBUG(dbgs() << "Executing Parallel Alias-Analysis on Func '"
                      << F.getName() << "'\n");
    LoopInfo &LI = FAM.getResult<LoopAnalysis>(F);

    SmallVector<Loop *> Loops;
    for (Loop *I : LI)
      for (Loop *L : depth_first(I))
        Loops.push_back(L);

    ParallelAliasAnalysisResult AnalysisResult;
    auto &KnownIvdep = AnalysisResult.KnownIvdep;

    for (auto *L : Loops) {
      MDNode *ParallelAccesses =
          findOptionMDForLoop(L, "llvm.loop.parallel_accesses");
      SmallPtrSet<MDNode *, 4> ParallelAccessGroups;
      if (ParallelAccesses) {
        for (const MDOperand &MD : drop_begin(ParallelAccesses->operands())) {
          MDNode *AccGroup = cast<MDNode>(MD.get());
          ParallelAccessGroups.insert(AccGroup);
        }
      }

      // FIXME: Is it sufficient that only one of the accesses is unconditional?
      // The other might never be executed, and make us conclude that a pointer
      // is non-alias but used by another, well-defined access. E.g.: for (int i
      // = 0; i < n; ++i) {
      //   idx = f(i);
      //   use(A[idx]); // unconditional read
      //   if (i % 2 == 2)
      //     A[idx] = 42; // never-executed write
      //   use(A[idx+1]); // another read that aliases with other iterations
      // }
      DenseSet<Value *> UnconditionalWriteAccesses;
      DenseSet<Value *> UnconditionalReadAccesses;
      SmallVector<Instruction *> Accesses;

      for (BasicBlock *BB : L->blocks()) {
        for (Instruction &I : *BB) {
          // Cannot argue this access is ivdep unless we know the instruction
          // can be executed in parallel to other instructions in another
          // iteration.
          MDNode *AccGroup = I.getMetadata(LLVMContext::MD_access_group);
          if (!ParallelAccessGroups.contains(AccGroup))
            continue;

          bool IsUnconditional = isGuaranteedToExecuteForEveryIteration(&I, L);

          if (StoreInst *SI = dyn_cast<StoreInst>(&I)) {
            if (!SI->isSimple())
              continue;

            if (IsUnconditional)
              UnconditionalWriteAccesses.insert(SI->getPointerOperand());
            Accesses.push_back(SI);
          } else if (LoadInst *LI = dyn_cast<LoadInst>(&I)) {
            if (!LI->isSimple())
              continue;

            if (IsUnconditional)
              UnconditionalReadAccesses.insert(LI->getPointerOperand());
            Accesses.push_back(LI);
          }
        }
      }

      for (Instruction *Acc : Accesses) {
        Value *Ptr = getLoadStorePointerOperand(Acc);

        // FIXME: This assumes that the ptr value is unique in a loop iteration,
        // which is not the case if it derived from a PHINode in a nested loop,
        // or it may not be relevant.
        bool HasConflictWithOtherThread =
            UnconditionalWriteAccesses.contains(Ptr);
        if (isa<StoreInst>(Acc) && !HasConflictWithOtherThread)
          HasConflictWithOtherThread = UnconditionalReadAccesses.contains(Ptr);

        // TODO: We also have to enusure that no synchronization event (e.g.
        // barrier) happens between the two accesses. See also: ThreadSanitizer

        if (!HasConflictWithOtherThread)
          continue;

        KnownIvdep.insert({L, Ptr});
        LLVM_DEBUG(dbgs() << "Cross-iteration non-alias:" << Ptr->getType()
                          << "\n");
      }
    }

    return PreservedAnalyses::none();
  }
};

} // namespace

llvm::PassPluginLibraryInfo getParallelAliasAnalysisPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "ParallelAliasAnalysis", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerScalarOptimizerLateEPCallback(
                [](llvm::FunctionPassManager &PM, OptimizationLevel Level) {
                  PM.addPass(ParallelAliasAnalysis());
                });
            PB.registerPipelineParsingCallback(
                [](StringRef Name, llvm::FunctionPassManager &PM,
                   ArrayRef<llvm::PassBuilder::PipelineElement>) {
                  if (Name == "parallel-aa") {
                    PM.addPass(ParallelAliasAnalysis());
                    return true;
                  }
                  return false;
                });
          }};
}

#ifndef LLVM_BYE_LINK_INTO_TOOLS
extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getParallelAliasAnalysisPluginInfo();
}
#endif
