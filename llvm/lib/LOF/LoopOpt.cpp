

#include "LoopOpt.h"
#include "GreenTree.h"
#include "RedTree.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "Green.h"
#include "GreenBuilder.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/LoopUtils.h"
#include "llvm/Analysis/AliasAnalysisEvaluator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Analysis/AliasAnalysis.h"

using namespace llvm;
using namespace llvm::lof;

namespace {
class StagedSequence;

class StagedLoop {
public:
  Loop *L;
  StagedSequence *Body;
  Value *Iterations;
  AtomDisjointSet *Atoms;

  explicit StagedLoop(Loop *LoopInfoLoop, Value *Iterations,
                      AtomDisjointSet *Atoms);
};

class StagedSequence {
public:
  SmallVector<llvm::PointerUnion<GreenStmt *, StagedLoop *>, 16> Stmts;
  SmallVector<const GreenExpr *, 16> Predicates;
  SmallVector<AtomDisjointSet *, 16> Atoms;

  void appendStmt(GreenStmt *Stmt, const GreenExpr *Predicate,
                  AtomDisjointSet *AtomSet) {
    Stmts.push_back(Stmt);
    Predicates.push_back(Predicate);
    Atoms.push_back(AtomSet);
  }
  void appendLoop(StagedLoop *Loop, const GreenExpr *Predicate,
                  AtomDisjointSet *AtomSet) {
    Stmts.push_back(Loop);
    Predicates.push_back(Predicate);
    Atoms.push_back(AtomSet);
  }
};

StagedLoop::StagedLoop(Loop *LoopInfoLoop, Value *Iterations,
                       AtomDisjointSet *Atoms)
    : L(LoopInfoLoop), Body(new StagedSequence()), Iterations(Iterations),
      Atoms(Atoms) {}



class GreenCodeGen {
  using BuilderTy = IRBuilder<>;
private:
  Green* Root;
  BuilderTy AllocaBuilder;
  LLVMContext& C;
 
  void emitGreen(Green*G, BuilderTy& Builder) {
    if (G->isExpr() || G->isStmt()) {
      auto& Op = G->getOperation();
      return;
    }

    if (G->isLoop()) {
      for (auto C : G->children()) {
        emitGreen(G, Builder);
      }

    }

    llvm_unreachable("Not yet implemented");
  }

public:
  GreenCodeGen(Green *Root, LLVMContext &C) :Root(Root), AllocaBuilder(C), C(C) {}


  Function* emitAsFunction(Module *M) {
    auto Func = Function::Create(FunctionType::get( Type::getVoidTy(C), false ), GlobalValue::InternalLinkage  , "", M);
    auto AllocaBB = BasicBlock::Create(C, "func.alloca", Func);
    auto EntryBB = BasicBlock::Create(C, "func.entry", Func);
   

    AllocaBuilder.SetInsertPoint(AllocaBB);
    AllocaBuilder.CreateBr(EntryBB);
    AllocaBuilder.SetInsertPoint(AllocaBB, AllocaBB->getFirstInsertionPt());

    BuilderTy Builder(EntryBB);
    Builder.SetInsertPoint(EntryBB);
    

    emitGreen(Root, Builder);


    auto ExitBB = BasicBlock::Create(C, "func.exit", Func);
    Builder.CreateBr(ExitBB);
    Builder.SetInsertPoint(ExitBB);
    Builder.CreateRetVoid();

    return Func;
  }
};


class LoopOptimizerImpl : public LoopOptimizer {
private:
  Function *Func;

  LoopInfo *LI;
  ScalarEvolution *SE;

  GreenRoot *OriginalRoot = nullptr;

  GreenLoop *createHierarchy(Function *F) const;
  GreenLoop *createHierarchy(Loop *L) const;
  GreenStmt *createHierarchy(BasicBlock *BB) const;

  GreenExpr *createExpr(Value *I);
  Green *createInst(Value *I);

  DenseMap<Value *, GreenExpr *> ExprCache;
  GreenExpr *getGreenExpr(Value *C);

  const GreenExpr *GreenTrue = GreenTrueLiteral::create();
  const GreenExpr *GreenFalse = GreenFalseLiteral::create();
  const GreenExpr *getGreenBool(bool Val) {
    return Val ? GreenTrue : GreenFalse;
  }

  DenseMap<CtrlAtom *, GreenCtrl *> CtrlExprCache;
  const GreenCtrl *getGreenCtrl(CtrlAtom *Atom) {
    auto Result = CtrlExprCache[Atom];
    if (!Result) {
      Result = GreenCtrl::create(Atom);
    }
    return Result;
  }

  const GreenExpr *getGreenLogicOr(const GreenExpr *LHS, const GreenExpr *RHS) {
    SmallVector<const GreenExpr *, 4> Operands;

    if (auto LOr = dyn_cast<GreenLogicOr>(LHS)) {
      auto LHChildren = LHS->getExprChildren();
      for (auto Arg : LHChildren) {
        if (!isa<GreenFalseLiteral>(Arg))
          Operands.push_back(Arg);
      }
    } else if (isa<GreenFalseLiteral>(LHS)) {
      // No effect
    } else if (isa<GreenTrueLiteral>(LHS)) {
      return getGreenBool(true);
    } else {
      Operands.push_back(LHS);
    }

    if (auto ROr = dyn_cast<GreenLogicOr>(RHS)) {
      auto RHChildren = RHS->getExprChildren();
      for (auto Arg : RHChildren) {
        if (!isa<GreenFalseLiteral>(Arg))
          Operands.push_back(Arg);
      }
    } else if (isa<GreenFalseLiteral>(RHS)) {
      // No effect
    } else if (isa<GreenTrueLiteral>(RHS)) {
      return getGreenBool(true);
    } else {
      Operands.push_back(RHS);
    }

    switch (Operands.size()) {
    case 0:
      return getGreenBool(false);
    case 1:
      return Operands[0];
    default:
      return GreenLogicOr::create(Operands);
    }
  }

  const GreenExpr *getGreenLogicAnd(const GreenExpr *LHS,
                                    const GreenExpr *RHS) {
    SmallVector<const GreenExpr *, 4> Operands;

    if (auto LAnd = dyn_cast<GreenLogicAnd>(LHS)) {
      auto LHChildren = LHS->getExprChildren();
      for (auto Arg : LHChildren) {
        if (!isa<GreenTrueLiteral>(Arg))
          Operands.push_back(Arg);
      }
    } else if (isa<GreenTrueLiteral>(LHS)) {
      // No effect
    } else if (isa<GreenFalseLiteral>(LHS)) {
      return getGreenBool(false);
    } else {
      Operands.push_back(LHS);
    }

    if (auto RAnd = dyn_cast<GreenLogicAnd>(RHS)) {
      auto RHChildren = RHS->getExprChildren();
      for (auto Arg : RHChildren) {
        if (!isa<GreenTrueLiteral>(Arg))
          Operands.push_back(Arg);
      }
    } else if (isa<GreenTrueLiteral>(RHS)) {
      // No effect
    } else if (isa<GreenFalseLiteral>(RHS)) {
      return getGreenBool(true);
    } else {
      Operands.push_back(RHS);
    }

    switch (Operands.size()) {
    case 0:
      return getGreenBool(true);
    case 1:
      return Operands[0];
    default:
      return GreenLogicAnd::create(Operands);
    }
  }

  DenseMap<Instruction *, GreenInst *> InstCache; // FIXME: Instructions may not be re-usable, so do not cache.
  Green *getGreenInst(Instruction *I);

  GreenStmt *createGreenStmt(const GreenExpr *MustExecutePredicate,
                             const GreenExpr *MayExecutePredicate,
                             ArrayRef<GreenInst *> Insts,
                             AtomDisjointSet *Atoms);

  GreenLoop *createGreenLoop(const GreenExpr *Predicate, StagedLoop *Staged);
  GreenSequence *createGreenSequence(StagedSequence *Sequence);
  GreenRoot *createGreenRoot(StagedSequence *TopLoop);

  const GreenSequence *parallelizeSequence(const GreenSequence *Seq) {
    bool Changed = false;
    SmallVector<const GreenBlock *, 32> NewBlocks;
    for (auto Block : Seq->blocks()) {
      const GreenBlock *NewBlock;
      if (auto Stmt = dyn_cast<GreenStmt>(Block)) {
        NewBlock = Stmt;
      } else if (auto Loop = dyn_cast<GreenLoop>(Block)) {
        NewBlock = parallelizeLoop(Loop);
      } else
        llvm_unreachable("unexpected");

      if (Block != NewBlock)
        Changed = true;

      NewBlocks.push_back(NewBlock);
    }

    if (!Changed)
      return Seq;

    return GreenSequence::create(NewBlocks);
  }

  const GreenLoop *parallelizeLoop(const GreenLoop *Loop) {
    auto L = Loop->getLoopInfoLoop();
    if (!L)
      return Loop;

    auto HasPragmaParallelize =
        getBooleanLoopAttribute(L, "llvm.loop.parallelize_thread.enable");
    if (!HasPragmaParallelize && !L->isAnnotatedParallel())
      return Loop;
    if (Loop->isExecutedInParallel())
      return Loop;

    auto Cloned = Loop->clone();
    Cloned->setExecuteInParallel();
    return Cloned;
  }

public:
  LoopOptimizerImpl(Function *Func, LoopInfo *LI, ScalarEvolution *SE)
      : Func(Func), LI(LI), SE(SE) {}

  Green *buildOriginalLoop(Loop *L, BasicBlock *Entry, Green *Cond);
  Green* buildOriginalLoopTree() {
      return buildOriginalLoop(nullptr, &Func->getEntryBlock(),  Green::createTrueExpr() );
  }

  void codegen(const GreenRoot *Root);

  bool optimize() override {
    auto OrigTree = buildOriginalLoopTree();

    emitAsFunc(OrigTree);

    return false;
  }


  

  Function* emitAsFunc(Green* Root) {
    GreenCodeGen CG(Root, Func->getContext());
    return CG.emitAsFunction(Func->getParent());
  }

  void view( Green* Root) {
    ViewGraph< Green *>(Root, "lof", false, "Loop Hierarchy Graph");
  }



  void print(raw_ostream &OS) override { OS << "Nothing to print yet\n"; }
};
} // namespace

GreenLoop *LoopOptimizerImpl::createHierarchy(Function *F) const {
  llvm_unreachable("unimplemented");
}

GreenLoop *LoopOptimizerImpl::createHierarchy(Loop *L) const {
  llvm_unreachable("unimplemented");
}

GreenStmt *LoopOptimizerImpl::createHierarchy(BasicBlock *BB) const {
  for (auto &I : *BB) {
    if (I.mayThrow())
      return nullptr;
  }
  llvm_unreachable("unimplemented");
}

GreenExpr *LoopOptimizerImpl::getGreenExpr(Value *V) {
  auto &Result = ExprCache[V];
  if (!Result) {
    Result = createExpr(V);
  }
  return Result;
}

GreenExpr *LoopOptimizerImpl::createExpr(Value *I) {
  if (auto P = dyn_cast<Argument>(I)) {
    auto Green = GreenArg::create(P);
    return Green;
  }

  if (auto C = dyn_cast<Constant>(I)) {
    auto Green = GreenConst::create(C);
    return Green;
  }

  if (auto PHI = dyn_cast<PHINode>(I)) {
    // FIXME: This is not really a register use; replace by case instruction or
    // other closed-form (AddRecExpr) expression
    auto Green = GreenReg::create(PHI);
    return Green;
  }

  if (auto GEP = dyn_cast<GetElementPtrInst>(I)) {
    SmallVector<GreenExpr *, 4> Exprs;
    for (auto Op : GEP->operand_values()) {
      auto GreenOp = getGreenExpr(Op);
      Exprs.push_back(GreenOp);
    }
    auto Green = GreenGEP::create(Exprs);
    return Green;
  }

  if (auto E = dyn_cast<ICmpInst>(I)) {
    assert(E->getNumOperands() == 2);
    auto LHS = getGreenExpr(E->getOperand(0));
    auto RHS = getGreenExpr(E->getOperand(1));
    return GreenICmp::create(E->getPredicate(), LHS, RHS);
  }

  llvm_unreachable("unimplemented");
}

#if 0
Green* LoopOptimizerImpl::createInst(Instruction* I) {
  return Green::createStmt(Operation(Operation::LLVMInst, I ) );
}
#endif

#if 0
GreenInst *LoopOptimizerImpl::getGreenInst(Instruction *I) {
  auto &Result = InstCache[I];
  if (!Result) {
    if (auto S = dyn_cast<StoreInst>(I)) {
      auto Val = getGreenExpr(S->getValueOperand());
      auto Ptr = getGreenExpr(S->getPointerOperand());
      Result = GreenStore::create(Val, Ptr);
    } else if (auto C = dyn_cast<CallInst>(I)) {
      auto Callee = getGreenExpr(C->getCalledOperand());
      SmallVector<const GreenExpr *, 8> Ops;
      for (auto &Op : C->args()) {
        auto Val = getGreenExpr(Op.get());
        Ops.push_back(Val);
      }
      Result = GreenCall::create(Callee, Ops);
    } else {
      // Register definition
      assert(!I->mayHaveSideEffects());
      auto Expr = getGreenExpr(I);
      Result = GreenSet::create(I, Expr);
    }
  }
  return Result;
}
#endif

GreenStmt *LoopOptimizerImpl::createGreenStmt(
    const GreenExpr *MustExecutePredicate, const GreenExpr *MayExecutePredicate,
    ArrayRef<GreenInst *> Insts, AtomDisjointSet *Atoms) {
  return GreenStmt::create(MustExecutePredicate, MayExecutePredicate, Insts,
                           Atoms);
}

GreenLoop *LoopOptimizerImpl::createGreenLoop(const GreenExpr *Predicate,
                                              StagedLoop *Staged) {
  auto Seq = createGreenSequence(Staged->Body);
  auto Iters = getGreenExpr(Staged->Iterations);
  return GreenLoop::create(Predicate, Predicate, Iters,
                           Staged->L->getCanonicalInductionVariable(), Seq,
                           Staged->L, Staged->Atoms);
}

GreenSequence *
LoopOptimizerImpl::createGreenSequence(StagedSequence *Sequence) {
  SmallVector<GreenBlock *, 32> Blocks;

  for (auto Pair : zip(Sequence->Stmts, Sequence->Predicates)) {
    auto Block = std::get<0>(Pair);
    auto Predicate = std::get<1>(Pair);
    if (auto Stmt = Block.dyn_cast<GreenStmt *>()) {
      Blocks.push_back(Stmt);
    } else if (auto Loop = Block.dyn_cast<StagedLoop *>()) {
      auto GLoop = createGreenLoop(Predicate, Loop);
      Blocks.push_back(GLoop);
    } else
      llvm_unreachable("Something is wrong");
  }

  auto Green = GreenSequence::create(Blocks);
  return Green;
}

GreenRoot *LoopOptimizerImpl::createGreenRoot(StagedSequence *TopLoop) {
  auto GSeq = createGreenSequence(TopLoop);
  auto Green = GreenRoot::create(GSeq);
  return Green;
}




Green* LoopOptimizerImpl::buildOriginalLoop(Loop* L, BasicBlock *Entry, Green *Cond) {
  std::deque < std::pair< BasicBlock*, Green*>> Worklist; 
  Worklist.push_back({ Entry, Green::createTrueExpr() });

 // SmallVector<GreenMeaning, 32> LvlInsts;
  SmallVector<Dep*, 16> InputDeps;
  int NumIntermediates = 0;
  int NumOutputs = 0;

  //DenseMap<Instruction*,  GVal*> DefVals;
  DenseMap<Value*, GVal*> InputsVals;
  GreenBuilder Builder;

  auto QueueSuccessor = [&,this](BasicBlock* BB, Green *BBCond) {
    // Backedge
    if (L && BB == L->getHeader())
      return;

    // Exiting edge
    if (L && !L->contains(BB))
      return;

    auto InLoop = LI->getLoopFor(BB);
    if (L == InLoop) {
      Worklist.push_back({ BB , Green::createTrueExpr()  });
      return;
    }

    assert(InLoop->getHeader()==BB);
    auto InnerLoop = buildOriginalLoop(InLoop, BB, BBCond);
    //GreenMeaning GMeaning(InnerLoop, {}, {});
    //LvlInsts.push_back(GMeaning);
    Builder.addStmt(InnerLoop, {});
  };


  while (!Worklist.empty()) {
    auto p = Worklist.front();
    auto BB = p.first;
    auto BBCond = p.second;
    Worklist.pop_front();

    for (auto& Inst: *BB) {
      if (Inst.isTerminator()) {
        // TODO: Detect rejoining branches
        if (auto BrInst = dyn_cast<BranchInst>(&Inst)) {
          if (BrInst->isUnconditional()) {
            QueueSuccessor(BrInst->getSuccessor(0), BBCond);
            break;
          }
          if (BrInst->isConditional()) {
            auto BrCond = nullptr;//TODO
              QueueSuccessor(BrInst->getSuccessor(0),  GreenBuilder::buildConjunctionExpr(BBCond, BrCond ) );
              QueueSuccessor(BrInst->getSuccessor(1),  GreenBuilder::buildConjunctionExpr(BBCond, GreenBuilder ::buildNotExpr( BrCond )) );
              break;
          }
        }
        if (auto RetInst = dyn_cast<ReturnInst>(&Inst)) {
          assert(!L && "Dont support ret inside loops yet");
          break;
        }

        llvm_unreachable("Unsupported branch");
      }

     // auto GInst = Green::createOperation();
    //  auto GSlots = Builder.addOperation(Operation(Operation::LLVMInst, &Inst), {});

      SmallVector<GVal*, 8> OperandVals;
      OperandVals.set_size(Inst.getNumOperands());
      for (auto &Operand : Inst.operands()) {
        assert(Operand.getUser() == &Inst);
        auto OpIdx = Operand.getOperandNo();
        auto Def = Operand.get();
       //  OutputSlot* OperandSlot;

        if (InputsVals.count(Def)) {
          OperandVals[OpIdx] = InputsVals[Def];
        } else if (auto DefInst = dyn_cast<Instruction>(Def)) {
            auto OperandSlot =Builder.addArgument ( DefInst);
            InputsVals[DefInst] = OperandSlot;
            OperandVals[OpIdx] = OperandSlot;
        }else         if (auto Arg = dyn_cast<Argument>(Def)) {
          auto OperandSlot =Builder.addArgument( Arg );
          InputsVals[Arg] = OperandSlot;
          OperandVals[OpIdx] = OperandSlot;
          //Builder.connect(OperandSlot,GSlots.Inputs[Operand.getOperandNo()]);
        }else if (auto C = dyn_cast<Constant>(Def)) {
            auto GConst =    Green:: createConstExpr(C);
            auto Slots = Builder.addStmt(GConst, {});
            OperandVals[OpIdx] = Slots.front();
        } else 
          llvm_unreachable("unhandled def");
      }

      //GreenMeaning GMeaning(GInst, {}, {});
      //LvlInsts.push_back(GMeaning);
      auto GSlots = Builder.addOperation(Operation(Operation::LLVMInst, &Inst), OperandVals);
      assert(GSlots.size()==1);
      InputsVals[&Inst] = GSlots.front();
    }
  }

  if (L) {
    //return Green::createLoop(InputDeps.size(), InputDeps, NumIntermediates, NumOutputs, LvlInsts);
    return Builder.createLoop();
  }
//  return Green::createFunc(InputDeps.size(), InputDeps, NumIntermediates, NumOutputs,  LvlInsts);
  return Builder.createStmt();
}

#if 0
const GreenRoot *parallelize(const GreenRoot *Root) {
  auto NewSeq = parallelizeSequence(Root->getSequence());
  if (NewSeq == Root->getSequence())
    return Root;

  auto NewRoot = Root->clone();
  NewRoot->setSequence(NewSeq);
  return NewRoot;
}
#endif

void LoopOptimizerImpl::codegen(const GreenRoot *Root) {
  auto M = Func->getParent();
  auto &Context = M->getContext();

  // Make a clone of the original function.
  FunctionType *FT = Func->getFunctionType();
  std::string FuncName = Func->getName().str();
  Function *NewFunc = Function::Create(FT, Func->getLinkage(), Twine(), M);
  NewFunc->addFnAttr("lof-output");
  // TODO: Carry-over function attributes

  BasicBlock *EntryBB = BasicBlock::Create(Context, "entry", NewFunc);

  IRBuilder<> Builder(EntryBB);
  CodegenContext CodegenCtx;
  auto &ActiveRegs = CodegenCtx.ActiveRegs;
  for (auto P : zip(Func->args(), NewFunc->args())) {
    ActiveRegs.insert({&std::get<0>(P), &std::get<1>(P)});
  }

  Root->getSequence()->codegen(Builder, CodegenCtx);

  if (FT->getReturnType()->isVoidTy())
    Builder.CreateRetVoid();
  else
    Builder.CreateRet(Builder.getInt32(0));

  // Remove old function
  // FIXME: Cannot remove function while being processed, so we just make it
  // 'unused' here at rely on some cleanup pass to actually remove it.
  // FIXME: FunctionPassManager has not been written for removing/adding
  // functions during passes. It will ignore added functions and continue to
  // process the currently processed function even if it was removed. We may
  // need to switch to be a CGSCC pass, which supports adding/removing
  // functions, bu will compute a call graph that we do not need. Howver, when
  // we want to process OpenMP frontend-outlined subfunctions, we will need to
  // become an CGSCC pass.
  Func->replaceAllUsesWith(NewFunc); // This might be evil in a FunctionPass
  Func->setLinkage(GlobalValue::PrivateLinkage);
  Func->setName(Twine(".") + FuncName + Twine("."));

  // Assign the name after removing the previous to avoid name disambiguation.
  NewFunc->setName(FuncName);
}





template <> struct llvm::GraphTraits<const llvm::GreenNode *> {
  using GraphRef = const GreenNode *;
  using NodeRef = const GreenNode *;

  static NodeRef getEntryNode(GraphRef L) { return L; }

  using ChildIteratorType = ArrayRef<NodeRef>::iterator;
  static ChildIteratorType child_begin(NodeRef N) {
    return N->getChildren().begin();
  }
  static ChildIteratorType child_end(NodeRef N) {
    return N->getChildren().end();
  }

  using nodes_iterator = df_iterator<NodeRef>;
  static nodes_iterator nodes_begin(GraphRef RI) {
    return nodes_iterator::begin(getEntryNode(RI));
  }
  static nodes_iterator nodes_end(GraphRef RI) {
    return nodes_iterator::end(getEntryNode(RI));
  }
};

template <>
struct llvm::DOTGraphTraits<const Green *> : public DefaultDOTGraphTraits {
  using GraphRef = const Green *;
  using NodeRef = const Green *;

  DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

  std::string getNodeLabel(NodeRef Node, GraphRef Graph) {
    SmallString<256> Result;
    raw_svector_ostream OS(Result);
    if (isSimple())
      Node->print(OS);
    else
      Node->print(OS);
    return OS.str().str();
  }
};



LoopOptimizer *llvm::createLoopOptimizer(Function *Func, LoopInfo *LI,  ScalarEvolution *SE) {
  return new LoopOptimizerImpl(Func, LI, SE);
}
