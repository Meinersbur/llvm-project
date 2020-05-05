#ifndef LLVM_LOF_LOOPOPT_H
#define LLVM_LOF_LOOPOPT_H

namespace llvm {
  class Function;
  class LoopInfo;
  class ScalarEvolution;
  class raw_ostream;
} // namespace llvm


namespace lof {
class LoopOptimizer {
public:
  virtual ~LoopOptimizer() {}
  virtual bool optimize() = 0;
  virtual void print(llvm::raw_ostream &OS) = 0;
};

LoopOptimizer *createLoopOptimizer(llvm::Function *Func,llvm:: LoopInfo *LI, llvm::ScalarEvolution *SE);
} // namespace lof

#endif /* LLVM_LOF_LOOPOPT_H */
