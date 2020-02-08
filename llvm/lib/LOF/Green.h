#ifndef LLVM_LOF_GREEN_H
#define LLVM_LOF_GREEN_H


#include "llvm/IR/Instruction.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
  namespace lof {
    class Green;

    class Operation {
    private:
      // Using this only to avoid duplicating the representation of an LLVM instruction. It's SSA value and operands have no meaning in the green DAG.
      llvm::Instruction* Inst = nullptr;
    public:

      bool isValid() const { return Inst != nullptr; }
    };


    class Input {
    private:
    public:
    };

    class Output {
    private:
    public:
    };


    class Dep {
    private:
      Green* OutputGreen;
      int OutputIdx;
      Green* InputGreen;
      int InputIdx;

    public:
      bool isScalar() const { return true; }
    };


    class Green {
    private:
      Operation Op;
      bool IsFloating;
      bool IsLoop;

      // Execution Predicate (for Stmt,Loop)
      Green* ExecPred;

      // Re-execution condition (for Loop)
      Green* BackedgePred;

      SmallVector<Green*, 8> Children;
      SmallVector<Dep*, 8> Connections;

      SmallVector<Input, 4> Inputs;
      SmallVector<Output, 4> Outputs;

      Green() {      }

    public:
      static Green* create() { return new Green(); }

      bool isExpr() const { return Op.isValid() && IsFloating; }
      bool isStmt() const { return !IsFloating; }
      bool isLoop() const { return IsLoop; }
    };
  }
}

#endif /* LLVM_LOF_GREEN_H */
