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
    public:
      enum Kind {
        Unknown,
        LLVMInst,
        LLVMFloating,
        False,
        True,
        Negation,
        Conjuction,
        Disjunction,

        Last = Disjunction
      };

    private:
      Kind K=Unknown;
      // Using this only to avoid duplicating the representation of an LLVM instruction. It's SSA value and operands have no meaning in the green DAG.
      llvm::Value* Inst = nullptr;
    public:
      Operation() {  }
      Operation(Kind K,  llvm::Value* Inst) : K(K), Inst(Inst) {  }

      bool isValid() const {
        
        return K != Unknown;
      }
      bool isFloating() const { 
        assert(isValid());
        return K != LLVMInst;
      
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
      // TODO: Need connection between output, not direct reference
      Green* GreenExpr;
    public:
      /* implicit */ Input(Green* E) : GreenExpr(E) {}
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
      bool IsFloating = false;
      bool IsLoop = false;

      // Execution Predicate (for Stmt,Loop)
      Green* ExecPred=nullptr;

      // Re-execution condition (for Loop)
      Green* BackedgePred=nullptr;

      SmallVector<Green*, 8> Children; 
      SmallVector<Dep*, 8> Connections;

      SmallVector<Input, 4> Inputs;
      SmallVector<Output, 4> Outputs;

      Green(Operation Op,  ArrayRef<Green*> Ops): Op(Op), Inputs(Ops.begin(), Ops.end()) {      }

    public:
      // TODO: Singletons
      static Green* createTrueExpr() {
        return createExpr(Operation(Operation::True, nullptr), {});
      }
      static Green* createFalseExpr() {
        return createExpr(Operation(Operation::True, nullptr), {});
      }

      static Green* createNotExpr(Green *Ops) {
        return createExpr(Operation(Operation::Negation, nullptr), { Ops });
      }

      // TOOD: List of operands
      static Green* createConjunctionExpr(Green*LHS, Green*RHS ) {
        return createExpr(Operation(Operation::Conjuction, nullptr), {LHS, RHS});
      }
      static Green* createDisjunctionExpr(Green*LHS, Green*RHS ) {
        return createExpr(Operation(Operation::Disjunction, nullptr), {LHS, RHS});
      }


      static Green* createExpr(Operation Op, ArrayRef<Green*> Ops) {
        assert(Op.isFloating());
        return new Green(Op, Ops); 
      }
      static Green* createStmt(Operation Op) {
        assert(!Op.isFloating());
        return new Green(Op, {});
      }
      static Green* createSequence(ArrayRef <Green*> Insts) {
        return new Green(Operation(), {});
      }
      static Green* createLoop(ArrayRef <Green*> Stmts) {
        return new Green(Operation(), {});
      }
      static Green* createFunc(ArrayRef <Green*> Stmts) {
        return new Green(Operation(), {});
      }

      bool isExpr() const { return Op.isValid() && IsFloating; }
      bool isStmt() const { return !IsFloating; }
      bool isLoop() const { return IsLoop; }
    };
  }
}

#endif /* LLVM_LOF_GREEN_H */
