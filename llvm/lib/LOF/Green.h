#ifndef LLVM_LOF_GREEN_H
#define LLVM_LOF_GREEN_H


#include "llvm/IR/Instruction.h"
#include "llvm/IR/Operator.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/ValueTracking.h"


namespace llvm {
  namespace lof {
    class Green;

    class Operation {
    private:
      // Using this only to avoid duplicating the representation of an LLVM instruction. It's SSA value and operands have no meaning in the green DAG.
      llvm::Value* Inst = nullptr;
    public:
      Operation() {  }
      Operation(llvm::Value* Inst) : Inst(Inst) {  }

      bool isValid() const { return Inst != nullptr; }
      bool isFloating() const { 
        // TODO: must not depend on operators
        return isa<Constant>(Inst) ||llvm:: isSafeToSpeculativelyExecute (Inst);
      }

      int getNumInputs() const {
        return cast<llvm::Operator >(Inst)->getNumOperands();
      }
      int getNumOutputs() const { 
        if (isa<StoreInst>(Inst))
          return 0;
        return 1;
      }

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
      Green* ExecPred=nullptr;

      // Re-execution condition (for Loop)
      Green* BackedgePred=nullptr;

      SmallVector<Green*, 8> Children;
      SmallVector<Dep*, 8> Connections;

      SmallVector<Input, 4> Inputs;
      SmallVector<Output, 4> Outputs;

      Green(Operation Op): Op(Op) {      }

    public:
      static Green* createFloating(Operation Op) {
        assert(Op.isFloating());
        return new Green(Op); 
      }
      static Green* createStmt(Operation Op) {
        assert(Op.isFloating());
        return new Green(Op);
      }
      static Green* createSequence(ArrayRef <Green*> Insts) {
        return new Green(Operation());
      }
      static Green* createLoop(ArrayRef <Green*> Stmts) {
        return new Green(Operation());
      }
      static Green* createFunc(ArrayRef <Green*> Stmts) {
        return new Green(Operation());
      }

      bool isExpr() const { return Op.isValid() && IsFloating; }
      bool isStmt() const { return !IsFloating; }
      bool isLoop() const { return IsLoop; }
    };
  }
}

#endif /* LLVM_LOF_GREEN_H */
