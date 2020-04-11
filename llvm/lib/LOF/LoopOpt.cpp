

#include "LoopOpt.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "Green.h"
#include "GreenBuilder.h"
#include "LoopTreeBuilder.h"
#include "LoopTreeCodegen.h"
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
#include "llvm/IR/IRBuilder.h"

using namespace llvm;
using namespace llvm::lof;

namespace {


class GreenConverter {
  DenseMap <Value*, GSymbol*> InputsVals;
  Function* Func;
  LoopInfo* LI;



public:
  GreenConverter(Function *Func,LoopInfo* LI) : Func(Func),  LI(LI) {}
  Green* build() {

    for (auto &Arg : Func->args()) {
      InputsVals[&Arg] = GSymbol::create(&Arg);
    }
    return buildOriginalLoop(nullptr, &Func->getEntryBlock(), GOpExpr::createTrueExpr());
  }

private:
  GSymbol* getOrCreateSym(Value* LLVMVal) {
    assert(!isa<Constant>(LLVMVal));
   auto &Sym = InputsVals[LLVMVal];
   if (!Sym) {
     Sym = GSymbol::create( LLVMVal );
   }
   return Sym;
  }

  GExpr* getOrCreateRefExpr(Value* LLVMVal) {
    // FIXME: constants that may trap must be instructions
    // TODO: Uniquefy constant expressions
    if (auto C = dyn_cast<Constant>(LLVMVal))
      return GOpExpr::createConstExpr(C);
    return getOrCreateSym(LLVMVal);
  }

  Green* buildOriginalLoop(Loop* L, BasicBlock *Entry, GExpr *Cond) {
    std::deque < std::pair< BasicBlock*, GExpr*>> Worklist; 
    Worklist.push_back({ Entry, GOpExpr::createTrueExpr() });

    // SmallVector<GreenMeaning, 32> LvlInsts;
    //SmallVector<Dep*, 16> InputDeps;
    //int NumIntermediates = 0;
    //int NumOutputs = 0;

    //DenseMap<Instruction*,  GVal*> DefVals;
    //DenseMap<Value*, GVal*> InputsVals;
    GreenBuilder Builder;

    auto QueueSuccessor = [&,this](BasicBlock* BB, GExpr *BBCond) {
      // Backedge
      if (L && BB == L->getHeader())
        return;

      // Exiting edge
      if (L && !L->contains(BB))
        return;

      auto InLoop = LI->getLoopFor(BB);
      if (L == InLoop) {
        Worklist.push_back({ BB , GOpExpr::createTrueExpr()  });
        return;
      }

      assert(InLoop->getHeader()==BB);
      auto InnerLoop = buildOriginalLoop(InLoop, BB, BBCond);
      //GreenMeaning GMeaning(InnerLoop, {}, {});
      //LvlInsts.push_back(GMeaning);

      Builder.addStmt( BBCond,  InnerLoop);
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
              QueueSuccessor(BrInst->getSuccessor(0),  GOpExpr::createConjunctionExpr(BBCond, BrCond ) );
              QueueSuccessor(BrInst->getSuccessor(1),  GOpExpr::createConjunctionExpr(BBCond, GOpExpr::createNotExpr( BrCond )) );
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

        SmallVector<GExpr*, 8> OperandVals;
        OperandVals.set_size(Inst.getNumOperands());
        for (auto &Operand : Inst.operands()) {
          assert(Operand.getUser() == &Inst);
          auto OpIdx = Operand.getOperandNo();
          auto Def = Operand.get();
          //  OutputSlot* OperandSlot;

          OperandVals[OpIdx] = getOrCreateRefExpr(Def);
        }


        auto ResultVal = GSymbol::create(&Inst);
         Builder.addInstruction(BBCond,  Operation(Operation::LLVMInst, &Inst), OperandVals , {ResultVal});
        InputsVals[&Inst] = ResultVal ;
      }
    }

    if (L) {
      //return Green::createLoop(InputDeps.size(), InputDeps, NumIntermediates, NumOutputs, LvlInsts);
      return Builder.createLoop();
    }
    //  return Green::createFunc(InputDeps.size(), InputDeps, NumIntermediates, NumOutputs,  LvlInsts);
    return Builder.createStmt();
  }



};


class GreenCodeGen {
  using BuilderTy = IRBuilder<>;
private:
  Green* Root;
  BuilderTy AllocaBuilder;
  Module* M;
  LLVMContext& C;
 
  DenseMap<GSymbol*, Value*> SymbolPtrs;


  void emitSequence(Green*G, BuilderTy& Builder) {
    assert(G->isContainer());
    for (auto C :  G->children()) {
      emitGreen(C, Builder);
    }
    llvm_unreachable("unimplemented");
  }

  void emitLoop(Green*G, BuilderTy& Builder) {
    // TODO: create loop frame
    emitSequence(G, Builder);
    llvm_unreachable("unimplemented");
  }

  Value* emitOperation(const  Operation& Op, ArrayRef<GExpr*> Arguments, BuilderTy& Builder, bool IsExpr) {
    switch (Op.getKind()) {
   
    case Operation::LLVMInst:
      assert(!IsExpr);
      LLVM_FALLTHROUGH
    case Operation::LLVMFloating:{
      auto I = Op.getLLVMInst();
      if (auto C = dyn_cast<Constant>(I)) {
        return C;
      } else if (auto Inst = dyn_cast<Instruction>(I)) {
        auto Copy = Inst->clone();
        for (auto P :  enumerate( Arguments)) {
          auto i = P.index();
          auto GArg = P.value();
          auto NewVal = emitExpr(GArg, Builder);
          Copy->setOperand(i,NewVal );
        }
        Builder.Insert(Copy);
        return Copy;
      }
      else
        llvm_unreachable("unimplemented");
    } break;
    default:
      llvm_unreachable("unimplemented");
    }
  }

  Value* emitExpr(GExpr* G, BuilderTy& Builder) {
    if (auto Reg = dyn_cast<GRefExpr>(G)) {
      assert(SymbolPtrs.count(Reg));
      auto Ptr= SymbolPtrs.lookup(Reg);
     return Builder.CreateLoad(Ptr);
    } else if ( auto OpE = dyn_cast< GOpExpr >(G) ) {
      auto Op = OpE->getOperation();
      auto Result = emitOperation( Op, OpE->getArguments() , Builder, true);
      return Result;
    } else
     llvm_unreachable("unimplemented");
  }

  void emitInstruction(Green*G, BuilderTy& Builder) {
    assert(G->isInstruction());

    auto Op = G->getOperation();
    auto NewInst=  emitOperation(Op, G->getArguments(), Builder, false);

    assert(Op.getNumOutputs() <= 1);
    assert(Op.getNumOutputs() == G->getAssignments().size());
    if (Op.getNumOutputs()) {
      auto DstSym = G->getAssignments()[0];
      auto DstPtr = SymbolPtrs.lookup(DstSym);
      assert(DstPtr);
      Builder.CreateStore( NewInst, DstPtr );
    }
  }

  void emitGreen(GCommon*G, BuilderTy& Builder) {
    if (G->isInstruction()) {
      emitInstruction(cast<Green>(G), Builder);
    } else if (G->isLoop()) {
      emitLoop(cast<Green>(G), Builder);
    }    else if (G->isContainer()) {
      emitLoop(cast<Green>(G), Builder);
    } else 
    llvm_unreachable("Not yet implemented");
  }

public:
  GreenCodeGen(Green *Root,Module *M, LLVMContext &C) :Root(Root), M(M), AllocaBuilder(C), C(C) {}


  Function* emitAsFunction() {
    DenseSet<GSymbol*> Reads;
    DenseSet<GSymbol*> Kills;
    DenseSet<GSymbol*> Writes;
    DenseSet<GSymbol*> AllReferences;
    Root->determineScalars(Reads, Kills, Writes, AllReferences);

    DenseSet<GSymbol*> Outside;
    Outside.insert(Reads.begin(), Reads.end() );
    Outside.insert(Writes.begin(),Writes.end() );

    SmallVector<Type*, 8 > ParamsTypes;
    SmallVector<GSymbol*,8> OutsideVec(Outside.begin(), Outside.end());
    for (auto V : OutsideVec) {
      ParamsTypes.push_back(PointerType::get(V->getType(), 0));
    }
    
    auto Func = Function::Create(FunctionType::get( Type::getVoidTy(C), ParamsTypes, false ), GlobalValue::InternalLinkage  , "", M);

    for (auto A : llvm::zip( OutsideVec,  Func->args() )) {
      auto Sym =std:: get<0>(A);
      auto Ptr = &std:: get<1>(A);
      assert(Sym->getType() == Ptr->getType()->getPointerElementType());
      SymbolPtrs[Sym] = Ptr;
    }

    auto AllocaBB = BasicBlock::Create(C, "func.alloca", Func);
    auto EntryBB = BasicBlock::Create(C, "func.entry", Func);
   

    AllocaBuilder.SetInsertPoint(AllocaBB);
    for (auto R : AllReferences) {
      auto& Ptr = SymbolPtrs[R];
      if (!Ptr) {
        Ptr = AllocaBuilder.CreateAlloca(R->getType());
      }
    }

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

  Green *createInst(Value *I);

  

public:
  LoopOptimizerImpl(Function *Func, LoopInfo *LI, ScalarEvolution *SE)
      : Func(Func), LI(LI), SE(SE) {}





  Green* buildOriginalLoopTree() {
    GreenConverter Converter(Func, LI );
    return Converter.build();
  }




  bool optimize() override {
    auto OrigTree = buildOriginalLoopTree();

    emitAsFunc(OrigTree);

    return false;
  }


  

  Function* emitAsFunc(Green* Root) {
    GreenCodeGen CG(Root,Func->getParent(), Func->getContext());
    return CG.emitAsFunction();
  }

  void view( Green* Root) {
    ViewGraph< Green *>(Root, "lof", false, "Loop Hierarchy Graph");
  }



  void print(raw_ostream &OS) override { OS << "Nothing to print yet\n"; }
};
} // namespace




LoopOptimizer *llvm::createLoopOptimizer(Function *Func, LoopInfo *LI,  ScalarEvolution *SE) {
  return new LoopOptimizerImpl(Func, LI, SE);
}


