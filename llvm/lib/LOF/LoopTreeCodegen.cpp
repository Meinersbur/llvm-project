#include "LoopTreeCodegen.h"
#include "llvm/IR/Verifier.h"

using namespace llvm;
using namespace lof;

void GreenCodeGen::replaceOrig(Green *Orig) {
  auto P = emitAsFunction();
  auto NewFunc = std::get<0>(P);
  auto UseArgs = std::get<1>(P);
  auto DefArgs = std::get<2>(P);

  Function *OldFunc = nullptr;
  BasicBlock *ReplBB;
  BasicBlock *ContBB;
  if (Orig->isLoop()) {
    auto OrigLoop = Orig->getOrigLoop();
    assert(OrigLoop);

    llvm_unreachable("unimplemented");
  } else {
    auto OrigRange = Orig->getOrigRange();
    assert(OrigRange.first);
    assert(OrigRange.second);
    OldFunc = OrigRange.first->getFunction();
    auto PrevBB = OrigRange.first->getParent();
    auto SecondBB = SplitBlock(PrevBB, OrigRange.first);

    ReplBB = BasicBlock::Create(LLVMCtx, "lof.replacement", OldFunc, SecondBB);
    cast<BranchInst>(PrevBB->getTerminator())->setSuccessor(0, ReplBB);

    ContBB = SplitBlock(OrigRange.second->getParent(), OrigRange.second);
  }

  BuilderTy AllocaBuilder(&OldFunc->getEntryBlock(),
                          OldFunc->getEntryBlock().begin());
  BuilderTy Builder(ReplBB);

  SmallVector<Value *, 8> Args;
  SmallVector<Value *, 8> Ptrs;
  for (auto P : llvm::enumerate(UseArgs)) {
    auto i = P.index();
    auto Sym = P.value();
    auto Val = Sym->getLLVMValue();

    assert(Val);
    Args.push_back(Val);
  }
  for (auto P : llvm::enumerate(DefArgs)) {
    auto i = P.index() + UseArgs.size();
    auto Sym = P.value();

    auto Ptr = AllocaBuilder.CreateAlloca(Sym->getType(), nullptr,
                                          Twine(Sym->getName()) + ".ptr");
    Args.push_back(Ptr);
  }
#if 0
  for (auto P : llvm::enumerate(Mapping)) {
    auto i = P.index();
    auto Sym = P.value();
    auto Val = Sym->getLLVMValue();

    auto Ptr = AllocaBuilder.CreateAlloca(Sym->getType(), nullptr, Twine(Sym->getName()) + ".ptr" );
    Builder.CreateStore( Val, Ptr );
    Args.push_back( Ptr );
  }
#endif

  Builder.CreateCall(NewFunc, Args);

  for (auto P : llvm::enumerate(DefArgs)) {
    auto i = P.index() + UseArgs.size();
    auto Sym = P.value();
    auto Val = Sym->getLLVMValue();

    auto NewVal = Builder.CreateLoad(Args[i], Sym->getName());
    Val->replaceAllUsesWith(NewVal);
  }
#if 0
  for (auto P : llvm::enumerate(Mapping)) {
    auto i = P.index();
    auto Sym = P.value();
    auto Val = Sym->getLLVMValue();
    auto NewVal = Builder.CreateLoad( Args[i], Sym->getName()  );
    Val->replaceAllUsesWith(NewVal);
  }
#endif

  Builder.CreateBr(ContBB);

  if (verifyFunction(*OldFunc, &errs())) {
    llvm_unreachable("Something's wrong!");
  }
}

std::tuple<Function *, std::vector<GSymbol *>, std::vector<GSymbol *>>
GreenCodeGen::emitAsFunction() {
  auto Reads = Root->getScalarReads();
  auto Kills = Root->getScalarKills();
  auto Writes = Root->getScalarWrites();
  // DenseSet<GSymbol*> AllReferences;
  // Root->determineScalars(Reads, Kills, Writes, AllReferences);
  // for (auto R : Reads) {
  //  assert(!Writes.count(R) && "Requiring SSA");
  // }

  DenseSet<GSymbol *> Outside;
  Outside.insert(Reads.begin(), Reads.end());
  Outside.insert(Writes.begin(), Writes.end());

  std::vector<GSymbol *> Mapping;
  SmallVector<Type *, 8> ParamsTypes;
  SmallVector<GSymbol *, 8> OutsideVec(Outside.begin(), Outside.end());

  std::vector<GSymbol *> UseArgs;
  for (auto V : Reads) {
    auto LLVMVal = V->getLLVMValue();
    if (!isa<Argument>(LLVMVal) && !isa<Instruction>(LLVMVal)) {
      // It's a global, accessible without parameters.
      continue;
    }

    ParamsTypes.push_back(V->getType());
    UseArgs.push_back(V);
  }

  std::vector<GSymbol *> DefArgs;
  for (auto V : Writes) {
    if (isa<GlobalValue>(V->getLLVMValue())) {
      // It's a global, accessible without parameters.
      continue;
    }

    ParamsTypes.push_back(PointerType::get(V->getType(), 0));
    DefArgs.push_back(V);
  }

  assert(DefArgs.size() <= 1 &&
         "We currently do only whole function replacement, there is just a "
         "single output value: the return value");

  Func = Function::Create(
      FunctionType::get(Type::getVoidTy(LLVMCtx), ParamsTypes, false),
      GlobalValue::InternalLinkage, "", M);
  Func->addFnAttr("lof-generated");

  for (auto A :
       llvm::zip_first(DefArgs, drop_begin(Func->args(), UseArgs.size()))) {
    auto Sym = std::get<0>(A);
    auto Ptr = &std::get<1>(A);
    Ptr->setName(Twine(Sym->getName()) + ".ptr");
    assert(Sym->getType() == Ptr->getType()->getPointerElementType());
    SymbolPtrs[Sym] = Ptr;
  }

  auto AllocaBB = BasicBlock::Create(LLVMCtx, "func.alloca", Func);
  auto EntryBB = BasicBlock::Create(LLVMCtx, "func.entry", Func);

  AllocaBuilder.SetInsertPoint(AllocaBB);
  AllocaBuilder.CreateBr(EntryBB);
  AllocaBuilder.SetInsertPoint(AllocaBB, AllocaBB->getFirstInsertionPt());

  for (auto A : llvm::zip_first(UseArgs, Func->args())) {
    auto Sym = std::get<0>(A);
    auto Arg = &std::get<1>(A);
    assert(Sym->getType() == Arg->getType());
    auto Ptr = AllocaBuilder.CreateAlloca(Sym->getType());
    AllocaBuilder.CreateStore(Arg, Ptr);
    SymbolPtrs[Sym] = Ptr;
  }

#if 0
  for (auto R : AllReferences) {
    auto& Ptr = SymbolPtrs[R];
    if (!Ptr) {
      Ptr = AllocaBuilder.CreateAlloca(R->getType());
    }
  }
#endif

  BuilderTy Builder(EntryBB);
  Builder.SetInsertPoint(EntryBB);

  emitGreen(Root, Builder);

  auto ExitBB = BasicBlock::Create(LLVMCtx, "func.exit", Func);
  Builder.CreateBr(ExitBB);
  Builder.SetInsertPoint(ExitBB);
  Builder.CreateRetVoid();

  if (verifyFunction(*Func, &errs())) {
    llvm_unreachable("Something's wrong!");
  }

  return {Func, UseArgs, DefArgs};
}

void GreenCodeGen::emitLoop(Green *G, BuilderTy &Builder) {
  auto PreheaderBB = BasicBlock::Create(LLVMCtx, "loop.preheader", Func);
  Builder.CreateBr(PreheaderBB);
  Builder.SetInsertPoint(PreheaderBB);

  auto HeaderBB = BasicBlock::Create(LLVMCtx, "loop.header", Func);
  Builder.CreateBr(HeaderBB);
  Builder.SetInsertPoint(HeaderBB);
  auto Cond = emitExpr(G->getExecCond(), Builder);

  // Creation order of BBs to have a nice block order in the function
  auto BodyBB = BasicBlock::Create(LLVMCtx, "loop.body", Func);
  auto ExitingTerm = Builder.CreateCondBr(Cond, BodyBB, nullptr);
  Builder.SetInsertPoint(BodyBB);
  emitSequence(G, Builder);

  auto LatchBB = BasicBlock::Create(LLVMCtx, "loop.latch", Func);
  Builder.CreateBr(LatchBB);
  Builder.SetInsertPoint(LatchBB);

  Builder.CreateBr(HeaderBB);

  auto ExitBB = BasicBlock::Create(LLVMCtx, "loop.exit", Func);
  ExitingTerm->setSuccessor(1, ExitBB);
  Builder.SetInsertPoint(ExitBB);
}

void GreenCodeGen::emitGreen(GCommon *G, BuilderTy &Builder) {
  if (G->isInstruction()) {
    emitInstruction(cast<Green>(G), Builder);
  } else if (G->isLoop()) {
    emitLoop(cast<Green>(G), Builder);
  } else if (G->isContainer()) {
    emitSequence(cast<Green>(G), Builder);
  } else
    llvm_unreachable("Not yet implemented");
}

void GreenCodeGen::emitSequence(Green *G, BuilderTy &Builder) {
  assert(G->isContainer());
  for (auto C : G->children()) {
    // TODO: Conditional execution
    emitGreen(C, Builder);
  }
}

Value *GreenCodeGen::getPtr(GSymbol *Sym) {
  auto &Ptr = SymbolPtrs[Sym];
  if (!Ptr) {
    auto Ty = Sym->getType();
    if (!Ty) {
      // Generic integer type
      // TODO: need a more systematic approach to integer sizes
      Ty = IntegerType::get(LLVMCtx, 64);
    }
    Ptr =
        AllocaBuilder.CreateAlloca(Ty, nullptr, Twine(Sym->getName()) + ".ptr");
  }
  return Ptr;
}

void GreenCodeGen::emitInstruction(Green *G, BuilderTy &Builder) {
  assert(G->isInstruction());

  auto Op = G->getOperation();
  auto NewInst = emitOperation(Op, G->getArguments(), Builder, false);

  assert(Op.getNumOutputs() <= 1);
  assert(Op.getNumOutputs() == G->getAssignments().size());
  if (Op.getNumOutputs()) {
    auto DstSym = G->getAssignments()[0];
    auto DstPtr = getPtr(DstSym);
    assert(DstPtr);
    Builder.CreateStore(NewInst, DstPtr);
  }
}

Value *GreenCodeGen::emitExpr(GExpr *G, BuilderTy &Builder) {
  if (auto Reg = dyn_cast<GRefExpr>(G)) {
    auto Ptr = getPtr(Reg);
    return Builder.CreateLoad(Ptr, Reg->getName());
  } else if (auto OpE = dyn_cast<GOpExpr>(G)) {
    auto Op = OpE->getOperation();
    auto Result = emitOperation(Op, OpE->getArguments(), Builder, true);
    return Result;
  } else
    llvm_unreachable("unimplemented");
}

Value *GreenCodeGen::emitOperation(const Operation &Op,
                                   ArrayRef<GExpr *> Arguments,
                                   BuilderTy &Builder, bool IsExpr) {
  switch (Op.getKind()) {
  case Operation::LLVMInst:
    assert(!IsExpr);
    LLVM_FALLTHROUGH;
  case Operation::LLVMSpeculable: {
    auto I = Op.getLLVMInst();
    if (auto C = dyn_cast<Constant>(I)) {
      return C;
    } else if (auto Inst = dyn_cast<Instruction>(I)) {
      auto Copy = Inst->clone();
      for (auto P : enumerate(Arguments)) {
        auto i = P.index();
        auto GArg = P.value();
        auto NewVal = emitExpr(GArg, Builder);
        Copy->setOperand(i, NewVal);
      }
      Builder.Insert(Copy);
      Copy->setName(Inst->getName());
      return Copy;
    } else
      llvm_unreachable("unimplemented");
  } break;
  case Operation::True:
    return ConstantInt::getTrue(LLVMCtx);
  case Operation::False:
    return ConstantInt::getFalse(LLVMCtx);
  case Operation::Negation:
    assert(Arguments.size() == 1);
    return Builder.CreateNot(emitExpr(Arguments[0], Builder));
  case Operation::Conjuction: {
    auto Result = emitExpr(Arguments[0], Builder);
    for (auto V : drop_begin(Arguments, 1))
      Result = Builder.CreateAnd(Result, emitExpr(V, Builder));
    return Result;
  }
  case Operation::Disjunction: {
    auto Result = emitExpr(Arguments[0], Builder);
    for (auto V : drop_begin(Arguments, 1))
      Result = Builder.CreateOr(Result, emitExpr(V, Builder));
    return Result;
  }
  case Operation::Add: {
    auto Result = emitExpr(Arguments[0], Builder);
    for (auto V : drop_begin(Arguments, 1)) {
      auto Last = Result;
      auto LastTy = Result->getType();
      auto New = emitExpr(V, Builder);
      auto NewTy = New->getType();
      if (LastTy != NewTy) {
        auto LastWidth = cast<IntegerType>(LastTy)->getBitWidth();
        auto NewWidth = cast<IntegerType>(NewTy)->getBitWidth();
        auto CommonTy = IntegerType::get(
            LLVMCtx,
            std::max(LastWidth, NewWidth)); // TODO: One more than larger size
                                            // in case of overflow?
        Last = Builder.CreateSExt(Last, CommonTy);
        New = Builder.CreateSExt(New, CommonTy);
      }
      Result = Builder.CreateAdd(Last, New);
    }
    return Result;
  }
  case Operation::Nop:
    assert(Arguments.size() == 1);
    return emitExpr(Arguments[0], Builder);
  default:
    llvm_unreachable("unimplemented");
  }
}
