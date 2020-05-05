#include "Green.h"
//#include "GreenBuilder.h"
#include "Red.h"



using namespace lof;


GOpExpr* GExpr:: createOp(Operation Op, ArrayRef<GExpr*> Args) { 
  return GOpExpr::create(Op,Args);
}



static void visitDetermineScalar(GCommon* G, DenseSet<GSymbol*>& Reads, DenseSet<GSymbol*>& Kills, DenseSet<GSymbol*>& Writes,  DenseSet<GSymbol*>& AllReferences) {
  if (auto Ref = dyn_cast<GSymbol>(G)) {
    Reads.insert(Ref);
    AllReferences.insert(Ref);
    return;
  }
  if (auto E = dyn_cast<GOpExpr>(G)) {
    for (auto A : E->args())
      visitDetermineScalar(A, Reads, Kills, Writes, AllReferences);
    return;
  }

  auto Stmt = cast<Green>(G);

  if (Stmt->hasComputedScalars()) {
    auto &StmtReads = Stmt->getScalarReads();
    Reads.insert(StmtReads.begin(), StmtReads.end());

    auto &StmtKills = Stmt->getScalarKills();
    Kills.insert(StmtKills.begin(), StmtKills.end());

    auto &StmtWrites = Stmt->getScalarWrites();
    Writes.insert(StmtWrites.begin(), StmtWrites.end());

    // TODO: Still need AllReferences?
    return;
  }

if (Stmt->isInstruction()) {
    auto Op = Stmt->getOperation();
    for (auto A : Stmt->getArguments()) {
      visitDetermineScalar(A, Reads, Kills, Writes,AllReferences);
    }
    for (auto A : Stmt->getAssignments()) {
      Writes.insert(A);
      // TODO: depending on conditions, also kills
      AllReferences.insert(A);
    }
    return;
  }  

if (Stmt->isStmt()) {
    for (auto C : Stmt->children()) 
      visitDetermineScalar(C, Reads, Kills, Writes,AllReferences);
    return;
  }   

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
  print(llvm::errs());
}


void  Green::print(raw_ostream &OS) const  { 
  OS << "Green"; 
}




 GCommon*  lof:: green_child_iterator ::operator*() const {
  if (auto Stmt = dyn_cast<Green>(Parent)) {
    return Stmt->getChildren()[Idx];
  }
  if (auto Expr = dyn_cast<GOpExpr>(Parent)) {
    return Expr->getArguments()[Idx];
  }
  llvm_unreachable("unknown type or has no children");
}



 Red* GCommon:: asRedRoot()  {
  return Red::createRoot( this);
 }


 static void collectRedInstructions(Red* R, std::vector<Red*>&Result) {
   auto G = R->getGreen();
   if (G->isInstruction()) {
     Result.push_back(R);
     return;
   }

      
   for (auto C : R->getChildren()) {
     collectRedInstructions(C, Result);
   }
 }


 /// Collect all red instructions
 static std::vector<Red*> getAllRedInstructions(GCommon* G) {
   std::vector<Red*> Result;
   collectRedInstructions(G->asRedRoot(), Result);
  return Result;
}





 std::vector<Dep*> Green::getAllDependencies()  {
   std::vector<Dep*> Result;
   std::vector<Red*> Reds = getAllRedInstructions(this);
   auto N = Reds.size();

   for (int i = 0; i < N; i++) {
     auto Src = Reds[i];
     auto GSrc =  cast<Green>( Src->getGreen());
     for (int j = 0; j < i; j++) {
       auto Dst = Reds[j];
       auto GDst = cast<Green>( Dst->getGreen());

       auto SrcWrites = GSrc->getScalarWrites();
       auto DstReads = GDst->getScalarReads();
       auto DstWrites = GDst->getScalarWrites();

       for (auto Def : SrcWrites) {
         
       }


     }
   }

   return Result;
 }


