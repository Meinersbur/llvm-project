#include "Green.h"
//#include "GreenBuilder.h"

using namespace llvm::lof;

namespace llvm {
  namespace lof {



  }
}

#if 0
 Green* Green::createNotExpr(Green* Subexpr) {
  return GreenBuilder:: createUnaryOpExpr(Subexpr, Operation::Negation);
}

  Green* Green::createConjunctionExpr(Green* LHS, Green* RHS) {
   return GreenBuilder:: createBinaryOpExpr(LHS, RHS, Operation::Conjuction);
 }
  Green* Green::createDisjunctionExpr(Green* LHS, Green* RHS) {
   return GreenBuilder::  createBinaryOpExpr(LHS, RHS, Operation::Disjunction);
 }
#endif

#if 0
   Green* Green::createLoop(int NumInputs, ArrayRef<Dep*> InputConsumers, int NumIntermediate, int NumOutputs, ArrayRef<GreenMeaning> Children) {
    auto Result = new Green(NumInputs, InputConsumers, NumIntermediate, NumOutputs, Operation(), Children, false, false, true);
    assert(Result->isLoop());
    assert(Result->isStmt());
    return Result;
  }

   Green* Green::createFunc(int NumInputs, ArrayRef<Dep*> InputConsumers, int NumIntermediate, int NumOutputs, ArrayRef<GreenMeaning> Children) {
    auto Result = new Green(NumInputs, InputConsumers, NumIntermediate, NumOutputs, Operation(), Children, false, false,  false);
    assert(Result->isStmt());
    return Result;
  }
#endif


LLVM_DUMP_METHOD void Green::dump() const {
  print(errs());
}


void  Green::print(raw_ostream &OS) const  { 
  OS << "Green"; 
}




