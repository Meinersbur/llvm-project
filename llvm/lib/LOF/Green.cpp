#include "Green.h"
//#include "GreenBuilder.h"

using namespace llvm;
using namespace llvm::lof;

namespace llvm {
  namespace lof {



  }
}


GOpExpr* GExpr:: createOp(Operation Op, ArrayRef<GExpr*> Args) { 
  return GOpExpr::create(Op,Args);
}



static void visitDetermineScalar(GCommon* G, DenseSet<GSymbol*>& Reads, DenseSet<GSymbol*>& Kills, DenseSet<GSymbol*>& Writes,  DenseSet<GSymbol*>& AllReferences) {
  if (auto Ref = dyn_cast<GSymbol>(G)) {
    Reads.insert(Ref);
    AllReferences.insert(Ref);
  } else if (auto E = dyn_cast<GOpExpr>(G)) {
    for (auto A : E->args())
      visitDetermineScalar(A, Reads, Kills, Writes,AllReferences);
  } else if (G->isInstruction()) {
    auto Stmt = cast<Green>(G);
    auto Op = Stmt->getOperation();
    for (auto A : Stmt->getArguments()) {
      visitDetermineScalar(A, Reads, Kills, Writes,AllReferences);
    }
    for (auto A : Stmt->getAssignments()) {
      Writes.insert(A);
      // TODO: depending on conditions, also kills
      AllReferences.insert(A);
    }
  }  else if (G->isStmt()) {
    auto Stmt = cast<Green>(G);
    for (auto C : Stmt->children()) 
      visitDetermineScalar(C, Reads, Kills, Writes,AllReferences);
 
  }  else 
    llvm_unreachable("unhandled");
}

void GCommon:: determineScalars(DenseSet<GSymbol*>& Reads, DenseSet<GSymbol*>& Kills, DenseSet<GSymbol*>& Writes,  DenseSet<GSymbol*>& AllReferences) {
  visitDetermineScalar(this, Reads, Kills, Writes,  AllReferences);
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




