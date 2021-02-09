#include "llvm/LOF/Green.h"
#include "LoopContext.h"
#include "Red.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/IR/instructions.h"

using namespace lof;

void Operation::assertOK() const {
  switch (K) {
  case Operation::Unknown:
    assert(Inst == nullptr);
    return;
  case Operation::Nop:
    assert(Inst == nullptr); // There is no "nop" LLVM instruction
    break;
  case Operation::Select:
    assert(NArgs >= 1); // with just a fallback-value, it's effectively like nop
                        // (maybe we don't need the Nop kind)
    assert(NArgs % 2 == 1);
    break;

  case Operation::ReduceReturn:
  case Operation::ReduceLast:
  case Operation::ReduceAdd:
    assert(Inst == nullptr);
    break;

  case Operation::LLVMSpeculable:
  case Operation::LLVMInst:
    assert(Inst);
    if (!Inst->getType()->isVoidTy()) {
      assert(llvm::PointerType::isValidElementType(Inst->getType()) &&
             "Must be allocatable");
    }
    assert(!isa<llvm::PHINode>(Inst) && "PHIs are not regular operation");
    if (auto I = dyn_cast<llvm::Instruction>(Inst)) {
      assert(!cast<llvm::Instruction>(I)->isTerminator() &&
             "Operations don't cover control-flow");
    }
    break;
  case Operation::LoadArrayElt:
  case Operation::StoreArrayElt:
    break;
  case Operation::True:
    if (Inst) {
      auto C = cast<llvm::ConstantInt>(Inst);
      assert(C->getType()->isIntegerTy(1));
      assert(C->isOne());
    }
    break;
  case Operation::False:
    if (Inst) {
      auto C = cast<llvm::ConstantInt>(Inst);
      assert(C->getType()->isIntegerTy(1));
      assert(C->isZero());
    }
    break;
  case Operation::Negation:
    assert(!Inst);
    break;
  case Operation::Conjuction:
    if (Inst) {
      auto O = cast<llvm::BinaryOperator>(Inst);
      assert(O->getType()->isIntegerTy(1));
      assert(O->getOpcode() == llvm::Instruction::And);
    }
    break;
  case Operation::Disjunction:
    if (Inst) {
      auto O = cast<llvm::BinaryOperator>(Inst);
      assert(O->getType()->isIntegerTy(1));
      assert(O->getOpcode() == llvm::Instruction::Or);
    }
    break;
  case Operation::Add:
    break;
  default:
    llvm_unreachable("Not supported kind?!?");
  }
}

bool Operation::isAssociative() const {
  if (K != LLVMSpeculable)
    return false;
  auto LLVMOp = llvm::dyn_cast<llvm::BinaryOperator>(Inst);
  if (!LLVMOp)
    return false;
  return LLVMOp->getOpcode() == llvm::BinaryOperator::Add;
}

#ifndef NDEBUG
LLVM_DUMP_METHOD void Operation::dump() const {
  printLine(llvm::errs());
  llvm::errs() << '\n';
}
#endif

GOpExpr *GExpr::createOp(Operation Op, ArrayRef<GExpr *> Args) {
  return GOpExpr::create(Op, Args);
}

static void visitDetermineScalar(GCommon *G, DenseSet<GSymbol *> &Reads,
                                 DenseSet<GSymbol *> &Kills,
                                 DenseSet<GSymbol *> &Writes,
                                 DenseSet<GSymbol *> &AllReferences) {
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

  // if ( Stmt->hasComputedScalars()) {
  auto StmtReads = Stmt->getScalarReads();
  Reads.insert(StmtReads.begin(), StmtReads.end());

  auto StmtKills = Stmt->getScalarKills();
  Kills.insert(StmtKills.begin(), StmtKills.end());

  auto StmtWrites = Stmt->getScalarWrites();
  Writes.insert(StmtWrites.begin(), StmtWrites.end());

  // TODO: Still need AllReferences?
  return;
  // }

  if (Stmt->isInstruction()) {
    auto Op = Stmt->getOperation();
    for (auto A : Stmt->getArguments()) {
      visitDetermineScalar(A, Reads, Kills, Writes, AllReferences);
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
      visitDetermineScalar(C, Reads, Kills, Writes, AllReferences);
    return;
  }

  llvm_unreachable("unhandled");
}

void lof::determineScalars(GCommon *Node, DenseSet<GSymbol *> &Reads,
                           DenseSet<GSymbol *> &Kills,
                           DenseSet<GSymbol *> &Writes,
                           DenseSet<GSymbol *> &AllReferences) {
  visitDetermineScalar(Node, Reads, Kills, Writes, AllReferences);
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

#if 0
LLVM_DUMP_METHOD void Green::dump() const {
  print(llvm::errs());
}


void  Green::print(raw_ostream &OS) const  { 
  OS << "Green"; 
}
#endif

GCommon *lof::green_child_iterator ::operator*() const {
  if (auto Stmt = dyn_cast<Green>(Container)) {
    return Stmt->getChildren()[Idx];
  }
  if (auto Expr = dyn_cast<GOpExpr>(Container)) {
    return Expr->getArguments()[Idx];
  }
  llvm_unreachable("unknown type or has no children");
}

Red *GCommon::asRedRoot() {
  if (!RedRoot)
    RedRoot = Red::createRoot(this);
  return RedRoot;
}

Green *Green::findNode(StringRef Name, bool Recursive) {
  if (Name == getName())
    return this;

  if (!Recursive) {
    for (auto Sub : this->SubStmts) {
      if (Sub->getName() == Name)
        return Sub;
    }
    return nullptr;
  }

  for (auto N : llvm::breadth_first<GCommon *>(this)) {
    if (auto G = dyn_cast<Green>(N)) {
      if (G->getName() == Name)
        return G;
    }
  }
  return nullptr;
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
void GCommon::dump() const {
  GreenDumper Dumper(llvm::errs());
  Dumper.dump(const_cast<GCommon *>(this));
}
#endif
