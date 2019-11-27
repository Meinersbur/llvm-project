#ifndef LLVM_LOF_LOOPOPT_H
#define LLVM_LOF_LOOPOPT_H

namespace llvm {
	class Function;
	class LoopInfo;
	class ScalarEvolution;
	class raw_ostream;

	class LoopOptimizer {
	public:
		virtual ~LoopOptimizer()	{		}
		virtual bool optimize()=0;
		virtual void print(raw_ostream &OS) =0;
	};

	LoopOptimizer *createLoopOptimizer(Function*Func,LoopInfo*LI,ScalarEvolution *SE);
}


#endif /* LLVM_LOF_LOOPOPT_H */
