#include "LoopTreeConverter.h"
#include "llvm/Analysis/AliasSetTracker.h"
#include "llvm/Analysis/ValueTracking.h"

using namespace llvm;
using namespace lof;

Green *GreenConverter::buildOriginalLoop(Loop *L, BasicBlock *Entry,
                                         GExpr *Cond,
                                         GreenBuilder &ParentBuilder) {
  auto F = Entry->getParent();

  std::deque<std::pair<BasicBlock *, GExpr *>> Worklist;
  Worklist.push_back({Entry, GOpExpr::createTrueExpr()});
  GreenBuilder Builder(Ctx);

  GSymbol *FirstSym = nullptr;
  GExpr *LoopCond;
  BasicBlock *Header = nullptr;
  if (L) {
    Header = L->getHeader();
    FirstSym = GSymbol::createFromScratch(
        "loop.isfirst", Type::getInt1Ty(Ctx.getLLVMContext()));
    LoopCond = GExpr::createRef(FirstSym);
  }

  // llvm::Value* ReturnVal = nullptr;

  llvm::Instruction *OrigEnd = nullptr;

  auto AddReadAccesses = [](GExpr *E, SetVector<GSymbol *> &ReadList) {
    DenseSet<GSymbol *> EntryReads;
    DenseSet<GSymbol *> EntryKills;
    DenseSet<GSymbol *> EntryWrites;
    DenseSet<GSymbol *> EntryAllRefs;
    determineScalars(E, EntryReads, EntryKills, EntryWrites, EntryAllRefs);
    ReadList.insert(EntryReads.begin(), EntryReads.end());
  };

#if 1
  SetVector<GSymbol *> Inputs;
  SetVector<GSymbol *> Outputs;
  SetVector<GSymbol *> Recurrences;

  auto ReadsKillsWrites = [&, this](Green *Stmt) {
    // DenseSet<GSymbol*> Reads; DenseSet<GSymbol*> Kills; DenseSet<GSymbol*>
    // Writes; DenseSet<GSymbol*> AllReferences;
    // Stmt->determineScalars(Reads, Kills, Writes, AllReferences);
    auto Reads = Stmt->getScalarReads();
    auto Writes = Stmt->getScalarWrites();

    for (auto Read : Reads) {
      bool DefinedOutside;
      if (!Read->getLLVMValue())
        continue;
      if (isa<Argument>(Read->getLLVMValue())) {
        DefinedOutside = true;
      } else if (auto LLVMVal = dyn_cast<Instruction>(Read->getLLVMValue())) {
        if (L) {
          DefinedOutside = !L->contains(LLVMVal);
        } else {
          // TODO: Use DominatorTree to find whether defined before Entry.
          // Currently this assumes that it's the entire function:
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

      for (auto &U : LLVMInst->uses()) {
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
#endif
  SmallVector<llvm::Value *, 8> Escaping;
  SmallVector<llvm::Value *, 8> MaybeEscaping;
  SmallVector<llvm::ReturnInst *, 2> FuncReturns;

  std::function<void(BasicBlock * BB, GExpr * BBCond)> QueueSuccessor =
      [&, this](BasicBlock *BB, GExpr *BBCond) {
        // Backedge
        if (L && BB == L->getHeader())
          return;

        // Exiting edge
        if (L && !L->contains(BB)) {
          assert(!OrigEnd && "Do not support multi-exits yet");
          OrigEnd = &*BB->begin();
          return;
        }

        auto InLoop = LI->getLoopFor(BB);
        if (L == InLoop) {
          Worklist.push_back({BB, GOpExpr::createTrueExpr()});
          return;
        }

        assert(InLoop->getHeader() == BB);

        auto InnerLoop = buildOriginalLoop(InLoop, BB, BBCond, Builder);
        Builder.addStmt(BBCond, InnerLoop);
        ReadsKillsWrites(InnerLoop);
        for (auto W : InnerLoop->getScalarReads()) {
          MaybeEscaping.push_back(W->getLLVMValue());
        }

        SmallVector<BasicBlock *, 4> ExitBlocks;
        InLoop->getExitBlocks(ExitBlocks);
        for (auto ExitBlock : ExitBlocks) {
          // TODO: If there are multiple exit blocks, they have conditions of
          // leaving
          QueueSuccessor(ExitBlock, GOpExpr::createTrueExpr());
          // Worklist.push_back({ ExitBlock , GOpExpr::createTrueExpr()  });
        }
      };

  SmallVector<std::pair<GSymbol *, Value *>, 4> PHINextVals;
  if (L) {
    auto Header = L->getHeader();
    for (auto &EntryPHI : Header->phis()) {
      auto NumPredecessors = EntryPHI.getNumIncomingValues();
      Value *InitialVal = nullptr;
      Value *NextVal = nullptr;
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
      // ParentBuilder.addAssignment( Ctx.getTrue(), PHISymbol,
      // getExpr(InitialVal) );

      PHINextVals.emplace_back(PHISymbol, NextVal);
    }
  }

  DenseSet<BasicBlock *> AlreadyVisited;
  SmallVector<Green *, 8> PendingPHIs;

  while (!Worklist.empty()) {
    auto p = Worklist.front();
    auto BB = p.first;
    auto BBCond = p.second;
    Worklist.pop_front();

    auto It = AlreadyVisited.insert(BB);
    if (!It.second) {
      continue;
    }

    for (auto &Inst : *BB) {
      if (Inst.isTerminator()) {
        // TODO: Detect rejoining branches
        if (auto BrInst = dyn_cast<BranchInst>(&Inst)) {
          if (BrInst->isUnconditional()) {
            QueueSuccessor(BrInst->getSuccessor(0), BBCond);
            break;
          }
          if (BrInst->isConditional()) {
            auto BrCond = nullptr; // TODO
            QueueSuccessor(BrInst->getSuccessor(0),
                           GOpExpr::createConjunctionExpr(BBCond, BrCond));
            QueueSuccessor(BrInst->getSuccessor(1),
                           GOpExpr::createConjunctionExpr(
                               BBCond, GOpExpr::createNotExpr(BrCond)));
            break;
          }
        }

        if (!L) {
          if (auto RetInst = dyn_cast<ReturnInst>(&Inst)) {
            FuncReturns.push_back(RetInst);
            break;
          }
        }

#if 0
        if (auto RetInst = dyn_cast<ReturnInst>(&Inst)) {
          assert(!ReturnVal);
          if (RetInst->getNumOperands() >= 1) {
            ReturnVal = RetInst->getOperand(0);
          }
          assert(!L && "Dont support ret inside loops yet");
          assert(!OrigEnd && "Don't support multiple returns yet");
          OrigEnd = RetInst;
          break;
        }
#endif

        llvm_unreachable("Unsupported branch");
      }

      Green *Stmt;
      GSymbol *ResultVal = nullptr;
      if (auto PHIInst = dyn_cast<llvm::PHINode>(&Inst)) {
        MaybeEscaping.push_back(PHIInst);

        if (L && L->getHeader() == BB) {
          // Create A PHI that we can reference later
          // Creation nodes writing to the PHI will be created at the end
          getOrCreateRefExpr(PHIInst);
          continue;
        }

        ResultVal = GSymbol::createLLVM(&Inst);
        Stmt = Builder.addInstruction(
            Inst.getName(), BBCond, Operation(Operation::Nop, nullptr),
            {getExpr(Inst.getOperand(0))}, {ResultVal}, &Inst);
      } else {
        SmallVector<GExpr *, 8> OperandVals;
        OperandVals.set_size(Inst.getNumOperands());
        for (auto &Operand : Inst.operands()) {
          assert(Operand.getUser() == &Inst);
          auto OpIdx = Operand.getOperandNo();
          auto Def = Operand.get();

          // OperandVals[OpIdx] = getOrCreateRefExpr(Def);
          OperandVals[OpIdx] = getExpr(Def);
        }

        if (llvm::isSafeToSpeculativelyExecute(&Inst)) {
          auto Op = GOpExpr::create(Operation(Operation::LLVMSpeculable, &Inst),
                                    OperandVals);
          InputsExprs[&Inst] = Op;
          MaybeEscaping.push_back(&Inst);
          continue;
        }

        if (Inst.getType()->isVoidTy()) {
          Stmt = Builder.addInstruction(Inst.getName(), BBCond,
                                        Operation(Operation::LLVMInst, &Inst),
                                        OperandVals, {}, &Inst);
        } else {
          ResultVal = GSymbol::createLLVM(&Inst);
          Stmt = Builder.addInstruction(Inst.getName(), BBCond,
                                        Operation(Operation::LLVMInst, &Inst),
                                        OperandVals, {ResultVal}, &Inst);
          MaybeEscaping.push_back(&Inst);
        }
      }

      if (ResultVal) {
        InputsVals[&Inst] = ResultVal;
        InputsExprs[&Inst] = GExpr::createRef(ResultVal);
      }

      ReadsKillsWrites(Stmt);
    }

    // FIXME: this is not correct. Should collect the conditions to leave the
    // loop and negate.
    if (L && L->isLoopExiting(BB)) {
      auto T = cast<BranchInst>(BB->getTerminator());
      for (int i = 0; i < 2; i += 1) {
        auto SuccBB = T->getSuccessor(i);
        if (L->contains(SuccBB)) {
          auto C = getOrCreateRefExpr(T->getOperand(0));
          if (i == 1)
            C = GOpExpr::createNotExpr(C);

          LoopCond = GOpExpr::createDisjunctionExpr(LoopCond, C);
        }
      }
    }
  }

  if (L) {
    for (auto &EntryPHI : Header->phis()) {
      auto ResultVal = cast<GRefExpr>(getOrCreateRefExpr(&EntryPHI));
      auto NumPredecessors = EntryPHI.getNumIncomingValues();
      GExpr *EntryVal = nullptr;
      GExpr *RecurrenceVal = nullptr;
      for (int i = 0; i < NumPredecessors; i += 1) {
        auto WhenBlock = EntryPHI.getIncomingBlock(i);
        auto ThenVal = EntryPHI.getIncomingValue(i);
        if (L->contains(WhenBlock)) {
          assert(!RecurrenceVal && "Expecting single latch");
          RecurrenceVal = getExpr(ThenVal);
        } else {
          assert(!EntryVal && "Expecting single preheader");
          EntryVal = getExpr(ThenVal);
        }
      }
      assert(RecurrenceVal);
      assert(EntryVal);

      AddReadAccesses(EntryVal, Inputs);
      AddReadAccesses(RecurrenceVal, Recurrences);

      auto Stmt = Green::createInstruction(
          EntryPHI.getName(), Operation(Operation::Select, nullptr, 3),
          {FirstSym, EntryVal, RecurrenceVal}, {ResultVal}, &EntryPHI, nullptr);

      // FIXME: this reverses the order of PHIs
      Builder.prepend(Stmt);
    }
  }

#if 0
  for (auto P : PHINextVals) {
      auto ContinueVal = getExpr(P.second);
      auto PHISym = P.first;
      //auto Stmt = Builder.addInstruction(GOpExpr::createTrueExpr(), Operation(Operation::Nop, nullptr), { ContinueVal }, { PHISym }, cast<Instruction>( PHISym->getLLVMValue()));
    // ReadsKillsWrites(Stmt);
  }
#endif

  if (L) {
    assert(FuncReturns.empty());
    SmallVector<llvm::Value *, 8> ReductionRequired;
    for (auto Val : MaybeEscaping) {
      for (auto *U : Val->users()) {
        auto UI = cast<Instruction>(U);
        if (!L->contains(UI->getParent())) {
          ReductionRequired.push_back(Val);
          break;
        }
      }
    }

    for (auto Val : ReductionRequired) {
      auto RedVal = Ctx.createSymbol((Twine(Val->getName()) + ".lcssa").str(),
                                     Val->getType());
      auto RedInst = Builder.addInstruction(
          (Twine(Val->getName()) + ".last").str(), nullptr,
          Operation(Operation::ReduceLast, nullptr), {getExpr(Val)}, {RedVal},
          nullptr);
      InputsExprs[Val] = RedVal;
      InputsVals[Val] = RedVal;
    }
  }

  if (L) {
#if 1
    SmallVector<GSymbol *, 8> ScalarReads(Inputs.begin(), Inputs.end());
    // Builder.setScalarReads(ScalarReads);

    SmallVector<GSymbol *, 8> ScalarKillWrites(Outputs.begin(), Outputs.end());
    // Builder.setScalarKills(ScalarKillWrites);
    // Builder.setScalarWrites(ScalarKillWrites);
    Builder.setScalarReadKillsWrites(ScalarReads, ScalarKillWrites,
                                     ScalarKillWrites,
                                     Recurrences.getArrayRef());
#endif

    // return Green::createLoop(InputDeps.size(), InputDeps, NumIntermediates,
    // NumOutputs, LvlInsts);
    return Builder.createLoop(Header->getName(), LoopCond, &*Entry->begin(),
                              OrigEnd, FirstSym, nullptr);
  }

  // FIXME: If not a loop, we currently assume it is a function

  SetVector<GSymbol *> ScalarReads;
  for (auto &Arg : F->args()) {
    ScalarReads.insert(InputsVals[&Arg]);
  }

  SmallVector<GSymbol *, 8> ScalarWrites;
#if 0
  if (ReturnVal) {
    auto ReturnSym = InputsVals[ReturnVal];
    assert(ReturnSym);
    ScalarWrites.push_back(ReturnSym);
  }
#endif
  // assert(MaybeEscaping.empty()  );
  for (auto Ret : FuncReturns) {
    if (Ret->getNumOperands() == 0)
      continue;

    auto ReturnedExpr = getExpr(Ret->getReturnValue());
    AddReadAccesses(ReturnedExpr, ScalarReads);
    auto ReturnSym =
        Ctx.createSymbol(".retval", Ret->getReturnValue()->getType());
    Builder.addInstruction(".return", nullptr,
                           Operation(Operation::ReduceReturn, nullptr),
                           {ReturnedExpr}, {ReturnSym}, Ret);
    // ScalarWrites.push_back(InputsVals[ Ret->getReturnValue()]);
  }

  Builder.setScalarReadKillsWrites(ScalarReads.getArrayRef(), {}, ScalarWrites,
                                   {});

  //  return Green::createFunc(InputDeps.size(), InputDeps, NumIntermediates,
  //  NumOutputs,  LvlInsts);
  return Builder.createStmt(F->getName(), &*Entry->begin(), OrigEnd);
}

Green *GreenConverter::build() {
  for (auto &Arg : Func->args()) {
    auto ArgExpr = GSymbol::createLLVM(&Arg);
    InputsVals[&Arg] = ArgExpr;
    InputsExprs[&Arg] = ArgExpr;
  }

  GreenBuilder DummyBuilder(Ctx);
  return buildOriginalLoop(nullptr, &Func->getEntryBlock(),
                           GOpExpr::createTrueExpr(), DummyBuilder);
}
