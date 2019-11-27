
#include "llvm/LOF/LoopOptPass.h"
#include "llvm/Pass.h"
#include "LoopOpt.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"

using namespace llvm;

namespace {
	class LoopOptimizationFramework : public FunctionPass {
	private:
		std::unique_ptr<LoopOptimizer> lo;

	public :
		static char ID;

		LoopOptimizationFramework() : FunctionPass(ID){
			initializeLoopOptimizationFrameworkPass(*PassRegistry::getPassRegistry());
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
			//AU.addPreserved<ScalarEvolutionWrapperPass>();
		}

		void releaseMemory() override { lo.reset(); }

		bool runOnFunction(Function &F) override {
			// Do not re-optimize our own output.
			if (F.hasFnAttribute("lof-output"))
				return false;

			auto LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
			auto SE = &getAnalysis<ScalarEvolutionWrapperPass>().getSE();
			lo .reset(  createLoopOptimizer(&F, LI,SE));
			return lo->optimize();
		}

		void print(raw_ostream &OS, const Module *) const override {
			if (!lo) {
				OS<< "Loop Hierachy Graph not built\n";
				return;
			}

			lo->print(OS);
		}
		//@}
	};



}

char LoopOptimizationFramework::ID = 0;

INITIALIZE_PASS_BEGIN(LoopOptimizationFramework, "lof",	"Loop Optimization Framework", false,	false)
INITIALIZE_PASS_DEPENDENCY(AAResultsWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_DEPENDENCY(OptimizationRemarkEmitterWrapperPass)
INITIALIZE_PASS_END(LoopOptimizationFramework, "lof", "Loop Optimization Framework", false, false)
