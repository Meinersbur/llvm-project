#include "LoopTreeConverter.h"
#include "llvm/Analysis/ValueTracking.h"

using namespace llvm;
using namespace lof;



Green* GreenConverter:: buildOriginalLoop(Loop* L, BasicBlock *Entry, GExpr *Cond, GreenBuilder &ParentBuilder) {
  std::deque < std::pair< BasicBlock*, GExpr*>> Worklist; 
  Worklist.push_back({ Entry, GOpExpr::createTrueExpr() });
  GreenBuilder Builder(Ctx);

  GSymbol* FirstSym = nullptr;
  GExpr* LoopCond;
  if (L) {
    FirstSym = GSymbol::createFromScratch("loop.isfirst", Type::getInt1Ty( Ctx.getLLVMContext() ) );
    LoopCond = GExpr::createRef(FirstSym);
  }



  llvm:: Instruction* OrigEnd = nullptr;
  DenseSet<GSymbol*> Inputs;
  DenseSet<GSymbol*> Outputs;

  auto ReadsKillsWrites = [&, this](Green* Stmt) {
    DenseSet<GSymbol*> Reads; DenseSet<GSymbol*> Kills; DenseSet<GSymbol*> Writes; DenseSet<GSymbol*> AllReferences;
    Stmt->determineScalars(Reads, Kills, Writes, AllReferences);

    for (auto Read : Reads) {
      bool DefinedOutside;
      if (isa<Argument>(Read->getLLVMValue())) {
        DefinedOutside = true;
      } else if (auto LLVMVal = dyn_cast<Instruction>(Read->getLLVMValue())) {
        if (L) {
          DefinedOutside = !L->contains(LLVMVal);
        } else {
          // TODO: Use DominatorTree to find whether defined before Entry. Currently this assumes that it's the entire function:
          DefinedOutside = false;
        }
      } else {
        // A constant or global
        continue;
      }

      if (DefinedOutside) {
        Inputs.insert(Read);
      }
    }

    for (auto Write : Writes) {
      auto LLVMVal = Write->getLLVMValue();
      auto LLVMInst = dyn_cast<Instruction>(LLVMVal);

      for (auto& U : LLVMInst->uses()) {
        auto UserInst = cast<Instruction>(U.getUser());
        bool UsedOutside;
        if (L) {
          UsedOutside = !L->contains(UserInst);
        } else {
          // TODO: Currently assuming this is an entire function
          UsedOutside = isa<ReturnInst>(UserInst);
        }

        if (UsedOutside)
          Outputs.insert(Write);
      }
    }
  };

  std::function<void(BasicBlock* BB, GExpr *BBCond)>  QueueSuccessor = [&,this](BasicBlock* BB, GExpr *BBCond) {
    // Backedge
    if (L && BB == L->getHeader()) 
      return;

    // Exiting edge
    if (L && !L->contains(BB)) {
      assert(!OrigEnd && "Do not support multi-exits yet");
      OrigEnd=&*BB->begin();
      return;
    }

    auto InLoop = LI->getLoopFor(BB);
    if (L == InLoop) {
      Worklist.push_back({ BB , GOpExpr::createTrueExpr()  });
      return;
    }

    
    assert(InLoop->getHeader()==BB);

#if 0
    BasicBlock* Latch = nullptr;
    BasicBlock* Entering = nullptr;
    for (auto Pred : predecessors(BB)) {
      if (InLoop->contains(Pred)) {
        assert(!Latch && "Requiring simplified loops for now");
        Latch = Pred;
      } else {
        assert(!Entering && "requiring a preheader for now");
        Entering = Pred;
      }
    }
    assert(Latch);
    assert(Entering);

    for (auto &PHI :  BB->phis()) {
      auto IncomingVal = getOrCreateRefExpr(PHI.getIncomingValueForBlock( Entering ));
      auto PHISum = getOrCreateSym(&PHI); 
      Builder.addInstruction(BBCond, Operation(Operation::Nop, nullptr), { IncomingVal }, { PHISum }, &PHI );
    }
#endif


    auto InnerLoop = buildOriginalLoop(InLoop, BB, BBCond, Builder);
    Builder.addStmt( BBCond,  InnerLoop);
    ReadsKillsWrites(InnerLoop);


    SmallVector<BasicBlock*, 4> ExitBlocks;
    InLoop->getExitBlocks(ExitBlocks);
    for (auto ExitBlock : ExitBlocks) {
      // TODO: If there are multiple exit blocks, they have conditions of leaving
      QueueSuccessor( ExitBlock,GOpExpr::createTrueExpr()    );
     // Worklist.push_back({ ExitBlock , GOpExpr::createTrueExpr()  });
    }
  };

  SmallVector<std::pair< GSymbol*,Value* >, 4> PHINextVals;
  if (L) {
    auto Header = L->getHeader();
    for (auto& EntryPHI : Header->phis()) {
      auto NumPredecessors = EntryPHI.getNumIncomingValues();
      Value* InitialVal = nullptr;
      Value* NextVal = nullptr;
      for (int i = 0; i < NumPredecessors; i += 1) {
        auto IncomingBlock = EntryPHI.getIncomingBlock(i);
        auto IncomingValue = EntryPHI.getIncomingValue(i);
        if (L->contains(IncomingBlock)) {
          assert(!NextVal);
          NextVal = IncomingValue;
        } else {
          assert(!InitialVal);
          InitialVal = IncomingValue;
        }
      }
      assert(InitialVal);
      assert(NextVal);

      auto PHISymbol = getOrCreateSym(&EntryPHI);
      ParentBuilder.addAssignment( Ctx.getTrue(), PHISymbol, getExpr(InitialVal) );

      PHINextVals.emplace_back(  PHISymbol, NextVal );
    }
  }

  while (!Worklist.empty()) {
    auto p = Worklist.front();
    auto BB = p.first;
    auto BBCond = p.second;
    Worklist.pop_front();

    for (auto& Inst: *BB) {
      if (auto PHI = dyn_cast<PHINode>(&Inst)) {
        if (L && L->getHeader() == BB) {
           // Handled separately (TODO)
          continue;
        }
      }

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
          assert(!OrigEnd && "Don't support multiple returns yet");
          OrigEnd = RetInst;
          break;
        }

        llvm_unreachable("Unsupported branch");
      }





      SmallVector<GExpr*, 8> OperandVals;
      OperandVals.set_size(Inst.getNumOperands());
      for (auto &Operand : Inst.operands()) {
        assert(Operand.getUser() == &Inst);
        auto OpIdx = Operand.getOperandNo();
        auto Def = Operand.get();

 
       // OperandVals[OpIdx] = getOrCreateRefExpr(Def);
        OperandVals[OpIdx] = getExpr(Def);
      }


      if (llvm::isSafeToSpeculativelyExecute(&Inst)) {
        InputsExprs[&Inst] = GOpExpr::create(Operation(Operation::LLVMSpeculable, &Inst), OperandVals);
        continue;
      }

      Green* Stmt;
      if (Inst.getType()->isVoidTy()) {
        Stmt = Builder.addInstruction(BBCond, Operation(Operation::LLVMInst, &Inst), OperandVals, {}, &Inst);
      } else {
        auto ResultVal = GSymbol::createLLVM(&Inst);
        Stmt = Builder.addInstruction(BBCond, Operation(Operation::LLVMInst, &Inst), OperandVals, { ResultVal }, &Inst);
        InputsVals[&Inst] = ResultVal;
        InputsExprs[&Inst] = GExpr::createRef(ResultVal);
      }
     
      ReadsKillsWrites(Stmt);
    }

    // FIXME: this is not correct. Should collect the conditions to leave the loop and negate.
    if (L && L->isLoopExiting(BB)) {
      auto T = cast<BranchInst>( BB->getTerminator());
      for (int i = 0; i < 2; i += 1) {
        auto SuccBB = T->getSuccessor(i);
        if (L->contains(SuccBB)) {
          auto C =  getOrCreateRefExpr( T->getOperand(0) );
          if (i == 1)
            C = GOpExpr::createNotExpr(C);

          LoopCond = GOpExpr::createDisjunctionExpr(LoopCond, C);
        }
      }
    }
  }

  for (auto P : PHINextVals ) {
      auto ContinueVal = getExpr(P.second);
      auto PHISym = P.first;
      auto Stmt = Builder.addInstruction(GOpExpr::createTrueExpr(), Operation(Operation::Nop, nullptr), { ContinueVal }, { PHISym }, cast<Instruction>( PHISym->getLLVMValue()));
    
      ReadsKillsWrites(Stmt);
  }

  
  SmallVector<GSymbol*, 8> ScalarReads(Inputs.begin(), Inputs.end()  );
  Builder.setScalarReads(ScalarReads);
    
  SmallVector<GSymbol*, 8> ScalarKillWrites(Outputs.begin(), Outputs.end()  );
  Builder.setScalarKills(ScalarKillWrites);
  Builder.setScalarWrites(ScalarKillWrites);


    if (L) {
    //return Green::createLoop(InputDeps.size(), InputDeps, NumIntermediates, NumOutputs, LvlInsts);
    return Builder.createLoop(LoopCond, &*Entry->begin(), OrigEnd, nullptr);
  }
  //  return Green::createFunc(InputDeps.size(), InputDeps, NumIntermediates, NumOutputs,  LvlInsts);
  return Builder.createStmt(&*Entry->begin(), OrigEnd);
}
