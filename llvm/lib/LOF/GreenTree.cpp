#include "GreenTree.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;


ArrayRef <const  GreenNode * >GreenSequence:: getChildren() const  {
	return ArrayRef<GreenNode*>((GreenNode**)&Blocks[0],Blocks.size());
}


ArrayRef <const  GreenNode * >  GreenRoot::getChildren() const  {
	auto Result =  ArrayRef <const  GreenNode * >((const  GreenNode **)&this->Sequence, 1);
return Result;
}


void GreenBlock:: printLineCond(raw_ostream &OS) const  {
  auto Cond = getMustExecutePredicate();
  if (!isa<GreenTrueLiteral>(getMustExecutePredicate())) {
    // TODO: Print as child?
    OS << "[";
    Cond->printLine(OS);
    OS << "] ";
  }
}


void GreenBlock::codegen(IRBuilder<> &Builder, CodegenContext &CodegenCtx )const {
  auto Func = Builder.GetInsertBlock()->getParent();
  auto &Ctx = Builder.getContext();

  auto EntryBB = BasicBlock::Create(Ctx, "block", Func);
  Builder.CreateBr(EntryBB);
  Builder.SetInsertPoint(EntryBB);

  auto Cond = MustExecutePredicate;

  auto ExitBB = EntryBB;
  SmallVector<BasicBlock*,2> SuccBBs; //atoms
  
  if (!isa<GreenTrueLiteral>(Cond)) {
    auto BodyBB = BasicBlock::Create(Ctx, "block.then", Func);
    auto ElseBB = BasicBlock::Create(Ctx, "block.else", Func);
    ExitBB = BasicBlock::Create(Ctx, "block.exit", Func);
    SuccBBs.push_back(BodyBB);
    SuccBBs.push_back(ElseBB);

    auto CondV = Cond->codegen(Builder, CodegenCtx);
    Builder.CreateCondBr(CondV, BodyBB, ElseBB);

    Builder.SetInsertPoint(ElseBB);
    Builder.CreateBr(ExitBB);

    Builder.SetInsertPoint(BodyBB);
  } 


  codegenBody(Builder, CodegenCtx);


  SmallVector<Value*, 2> AtomVals;
  if (isa<GreenTrueLiteral>(Cond)) {
	  auto Atom = Builder.getInt1(true);
	  AtomVals.push_back(Atom);
  } else { 
	  Builder.SetInsertPoint(ExitBB);
	  auto TrueAtom = Builder.CreatePHI( Builder.getInt1Ty(), 2, "atom" );
	  TrueAtom->addIncoming(Builder.getInt1(true), SuccBBs[0] );
	  TrueAtom->addIncoming(Builder.getInt1(false), SuccBBs[1] );
	  AtomVals.push_back(TrueAtom);

	  auto FalseAtom = Builder.CreateNot(TrueAtom, "not_atom");
	  AtomVals.push_back(FalseAtom);
  }

  for (int i = 0; i < AtomSet->getNumAtoms(); i += 1)
	  CodegenCtx.AtomVals[AtomSet->getAtom(i)] = AtomVals[i];
}


#if 0
void GreenBlock:: codegenPredicate(IRBuilder<> &Builder, CodegenContext &CodegenCtx,const std::function <void(IRBuilder<> &, CodegenContext &)> &CodegenBody ) const {
  auto Cond = MustExecutePredicate;
  if (isa<GreenTrueLiteral>(Cond))
    return;

  auto Func = Builder.GetInsertBlock()->getParent();
  auto &Ctx = Builder.getContext();
  auto CondV = Cond->codegen(Builder, CodegenCtx);
  
  SmallVector<BasicBlock*,2> SuccBB;

  auto BodyBB = BasicBlock::Create(Ctx, "block.then", Func);
  auto ElseBB = BasicBlock::Create(Ctx, "block.else", Func);

  Builder.CreateCondBr(CondV, BodyBB, ElseBB);

  Builder.SetInsertPoint(BodyBB);
  CodegenBody(Builder,CodegenCtx);

  Builder.Set

  Builder.SetInsertPoint();

}
#endif

 ArrayRef <const GreenNode * >GreenLoop:: getChildren() const   {
	 return ArrayRef <const  GreenNode * >((const  GreenNode **)&Sequence,1);		
 }


GreenStmt:: GreenStmt(const GreenExpr *MustExecutePredicate,const GreenExpr *MayExecutePredicate, ArrayRef<GreenInst*> Insts, AtomDisjointSet *AtomSet):  
   GreenBlock(MustExecutePredicate, MayExecutePredicate, AtomSet ), // FIXME: If  MustExecute and MayExecute are not the same, the result of this atom depends on speculative execution, hence undefined; split into separate predicate that defines the atom?
   Insts(Insts) {}


void GreenStmt::printLine(raw_ostream &OS) const  { 
printLineCond(OS);
  OS << "Stmt"; 
}

  ArrayRef <const GreenNode * > GreenStmt:: getChildren() const  {
	  return ArrayRef<GreenNode*>((GreenNode**)&Insts[0],Insts.size());
  }


ArrayRef <const GreenNode * > GreenStore:: getChildren() const { 
	return  ArrayRef<GreenNode*>((GreenNode**)&Operands[0],(size_t)2); 
}


StructType *GreenLoop:: getIdentTy(Module *M) const {
	auto &Context = M->getContext();
	auto Int32Ty = Type::getInt32Ty(Context);
	auto Int8PtrTy = Type::getInt8PtrTy(Context);

	StructType *IdentTy = M->getTypeByName("struct.ident_t");
	if (!IdentTy) {
		Type *LocMembers[] = {Int32Ty, Int32Ty,	Int32Ty,Int32Ty,Int8PtrTy};
		IdentTy =			StructType::create(Context, LocMembers, "struct.ident_t", false);
	}
	return IdentTy;
}


void GreenSequence:: codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const{
	for (auto Block : Blocks) 
		Block->codegen(Builder,ActiveRegs);
}



Function * GreenLoop::codegenSubfunc(Module *M, SmallVectorImpl<Value*>& UseRegArgs)const {
	auto &Context = M->getContext();
	auto& DL = M->getDataLayout();

	auto Int32Ty = Type::getInt32Ty(Context);
	auto Int32PtrTy = Type::getInt32PtrTy(Context);
	auto Int8PtrTy = Type::getInt8PtrTy(Context);
	auto LongTy =  Type::getInt64Ty(Context); // Depends on the bitness of the loop counter
	auto LongPtrTy = LongTy->getPointerTo();
	auto VoidTy=  Type::getVoidTy(Context);


	SetVector<Instruction*> DefRegs;
	Sequence->findRegDefs(DefRegs);

	SetVector<Value *>UseRegs;
	Sequence->findRegUses(UseRegs);

	UseRegs.set_subtract(DefRegs);
	UseRegs.remove( IndVar );


	SmallVector<Type*, 8> ArgTypes  = {/* global_tid */ Int32PtrTy, /* bound_tid */ Int32PtrTy, /* Iterations */ LongPtrTy}; 
	for (auto X : UseRegs) {
		UseRegArgs.push_back(X);
		ArgTypes.push_back(X->getType()->getPointerTo() );
	}


	FunctionType *FT = FunctionType::get(VoidTy, ArgTypes, false);
	Function *SubFn = Function::Create(FT, Function::InternalLinkage, ".parloop.subfun.", M);
	(SubFn->arg_begin() + 0)->setName(".global_tid.");
	(SubFn->arg_begin() + 1)->setName(".bound_tid.");
	(SubFn->arg_begin() + 2)->setName("parloop.iterations.addr");
	SubFn->addFnAttr("lof-output");

	BasicBlock *EntryBB = BasicBlock::Create(Context, "parloop.entry", SubFn);
	BasicBlock *PrecondThenBB = BasicBlock::Create(Context, "parloop.precond.then", SubFn);
	BasicBlock *InnerForCondBB = BasicBlock::Create(Context, "parloop.inner.for.cond", SubFn);
	BasicBlock *InnerForBodyBB = BasicBlock::Create(Context, "parloop.inner.for.body", SubFn);
	BasicBlock *ExitBB = BasicBlock::Create(Context, "parloop.loop.exit", SubFn);
	BasicBlock *PrecondEndBB = BasicBlock::Create(Context, "parloop.precond.end", SubFn);

	IRBuilder<> Builder(EntryBB);
	Value *IsLastPtr = Builder.CreateAlloca(Int32Ty, nullptr,"parloop.is_last.addr");
	Value *LBPtr = Builder.CreateAlloca(LongTy, nullptr, "parloop.lb.addr");
	Value *UBPtr = Builder.CreateAlloca(LongTy, nullptr, "parloop.ub.addr");
	Value *StridePtr = Builder.CreateAlloca(LongTy, nullptr, "parloop.stride.addr");
	

	auto IterationsPtr = SubFn->arg_begin()+2;
	auto IterationsV = Builder.CreateLoad(IterationsPtr, "parloop.iterations");

	auto Cmp=Builder.CreateICmpULT( Builder.getInt64(0), IterationsV );
	Builder.CreateCondBr(Cmp, PrecondThenBB, PrecondEndBB);


	Builder.SetInsertPoint(PrecondThenBB);
	auto GlobalTidPtr = SubFn->arg_begin();
	auto GlobalTidV = Builder.CreateLoad(GlobalTidPtr, "parloop.global_id");

	auto IdentTy = getIdentTy(M);
	auto IdentPtrTy = IdentTy->getPointerTo();
	auto Ident = ConstantPointerNull::get(IdentPtrTy);

	Function *KmpcForStaticInit = M->getFunction("__kmpc_for_static_init_8u");
	if (!KmpcForStaticInit) {
		Type *Params[] = {IdentPtrTy, Int32Ty, Int32Ty,  
			Int32PtrTy,LongPtrTy, LongPtrTy, LongPtrTy,  LongTy, LongTy};
		FunctionType *Ty = FunctionType::get(VoidTy, Params, false);
		KmpcForStaticInit = Function::Create(Ty, Function::ExternalLinkage, "__kmpc_for_static_init_8u", M);
	}

	Builder.CreateStore( Builder.getInt32(0), IsLastPtr );
	Builder.CreateStore( Builder.getInt64(0),LBPtr  );
	Builder.CreateStore( IterationsV,UBPtr );
	Builder.CreateStore( Builder.getInt64(1),StridePtr );
	Value *KmpcForStaticInitArgs[] = {Ident ,GlobalTidV, /* kmp_sch_static */ Builder.getInt32(34) , 
		IsLastPtr,  LBPtr,UBPtr,StridePtr,Builder.getInt64(1), /* ChunkSize */ Builder.getInt64(1)};
	Builder.CreateCall(KmpcForStaticInit, KmpcForStaticInitArgs);


	Value* UB = Builder.CreateLoad(UBPtr, "parloop.ub");
	auto UBSmall= Builder.CreateICmpULT(  IterationsV, UB);
	UB = Builder.CreateSelect(UBSmall,IterationsV,UB );

	auto LB = Builder.CreateLoad(LBPtr, "parloop.lb");
	Builder.CreateBr(InnerForCondBB);


	Builder.SetInsertPoint(InnerForCondBB);
	auto IV = Builder.CreatePHI( LongTy, 2, "parloop.iv" );
	IV->addIncoming(LB, PrecondThenBB );
	auto Cont = Builder.CreateICmpULT(IV, UB, "parloop.cont");
	Builder.CreateCondBr(Cont, InnerForBodyBB,ExitBB );



	Builder.SetInsertPoint(InnerForBodyBB);


  CodegenContext SubContext;
	auto & SubFnActiveRegs = SubContext.ActiveRegs; 
	SubFnActiveRegs.insert({IndVar, IV} );
	for (auto Y : zip( UseRegArgs,drop_begin( SubFn->args(),3 )  ) ) {
		auto Loaded = Builder.CreateLoad(&std::get<1>(Y)  );
		SubFnActiveRegs.insert({ std::get<0>(Y) , Loaded});
	}

	Sequence->codegen(Builder, SubContext);

	auto NextIV = Builder.CreateAdd(IV, Builder.getInt64(1), "parloop.iv.next" );
	IV->addIncoming( NextIV, InnerForBodyBB );
	Builder.CreateBr(InnerForCondBB);


	Builder.SetInsertPoint(ExitBB);

	Function *KmpcForStaticFini = M->getFunction("__kmpc_for_static_fini");
	if (!KmpcForStaticFini) {
		Type *Params[] = {IdentPtrTy, Int32Ty};
		FunctionType *Ty = FunctionType::get(VoidTy, Params, false);
		KmpcForStaticFini = Function::Create(Ty, Function::ExternalLinkage, "__kmpc_for_static_fini", M);
	};
	Value *KmpcForStaticFiniArgs[] = {Ident, GlobalTidV};
	Builder.CreateCall(KmpcForStaticFini, KmpcForStaticFiniArgs);
	Builder.CreateBr(PrecondEndBB);


	Builder.SetInsertPoint(PrecondEndBB);
	Builder.CreateRetVoid();
	return SubFn;
}


void GreenLoop:: codegenBody(IRBuilder<> &Builder, CodegenContext &ActiveRegs) const {
	auto ItersV = Iterations->codegen(Builder, ActiveRegs);
	Function *F = Builder.GetInsertBlock()->getParent();
	LLVMContext &Context = F->getContext();
	auto M = F->getParent();

	auto Int32Ty = Type::getInt32Ty(Context);
	auto Int32PtrTy = Type::getInt32PtrTy(Context);
	auto Int8PtrTy = Type::getInt8PtrTy(Context);
	auto LongTy =  Type::getInt64Ty(Context); // Depends on the bitness of the loop counter
	auto LongPtrTy = LongTy->getPointerTo();
	auto VoidTy =  Type::getVoidTy(Context);

	if (ExecuteInParallel) {
		SmallVector<Value*,8> Params;
		auto SubFunc = codegenSubfunc(M,Params  );

		IRBuilder<> AllocaBuilder(&F->getEntryBlock());
		auto IterationsPtr =AllocaBuilder.CreateAlloca(LongTy, nullptr, "iterations.ptr");
		Builder.CreateStore(ItersV, IterationsPtr);

		auto IdentTy = getIdentTy(M);
		auto IdentPtrTy = IdentTy->getPointerTo();
		auto Ident = ConstantPointerNull::get(IdentPtrTy);

		FunctionType *Kmpc_MicroTy=nullptr;
		if (!Kmpc_MicroTy) {
			// Build void (*kmpc_micro)(kmp_int32 *global_tid, kmp_int32 *bound_tid,...)
			llvm::Type *MicroParams[] = {Int32PtrTy, Int32PtrTy};
			Kmpc_MicroTy = llvm::FunctionType::get(VoidTy, MicroParams, true);
		}

		Function *KmpcForkCall = M->getFunction("__kmpc_fork_call");
		if (!KmpcForkCall) {
			Type *Params[] = {IdentTy->getPointerTo(), Int32Ty,	Kmpc_MicroTy->getPointerTo()};
			FunctionType *Ty = FunctionType::get(VoidTy, Params, true);
			KmpcForkCall = Function::Create(Ty, Function::ExternalLinkage, "__kmpc_fork_call", M);
		}

	

		Value *Task = Builder.CreatePointerBitCastOrAddrSpaceCast(SubFunc, Kmpc_MicroTy->getPointerTo());

	

		SmallVector<Value*,8> Args =  {Ident,	/* Number of subfn varargs */ Builder.getInt32(1 +Params.size() ), Task, IterationsPtr  };
		for (auto Z : Params) {
			auto NewVal =ActiveRegs. ActiveRegs.lookup(Z);
			auto ValAlloca = AllocaBuilder.CreateAlloca(Z->getType());
			Builder.CreateStore(NewVal, ValAlloca);
			Args.push_back(ValAlloca);
		}
		Builder.CreateCall(KmpcForkCall, Args);
	} else {
		Value *ValueLB, *ValueUB, *ValueInc;
		Type *MaxType;
		BasicBlock *ExitBlock;
		CmpInst::Predicate Predicate;

		BasicBlock *BeforeBB = Builder.GetInsertBlock();
		auto AfterBB = SplitBlock(BeforeBB,&* Builder.GetInsertPoint());

		BasicBlock *CondBB = BasicBlock::Create(Context, "loop.cond", F);
		BasicBlock *BodyBB = BasicBlock::Create(Context, "loop.body", F);

		BeforeBB->getTerminator()->setSuccessor(0, CondBB);
	

		Builder.SetInsertPoint(CondBB);
		PHINode *IV = Builder.CreatePHI(Builder.getInt32Ty() , 2, "loop.indvar");
		IV->addIncoming(Builder.getInt32(0), BeforeBB);
		Value *LoopCondition = Builder.CreateICmp(CmpInst::ICMP_SLT , IV, ItersV, "loop.cond");
		BranchInst *B = Builder.CreateCondBr(LoopCondition, BodyBB, AfterBB);

		Builder.SetInsertPoint(BodyBB);
		Sequence->codegen(Builder, ActiveRegs);
		Value *IncrementedIV = Builder.CreateNSWAdd(IV,  Builder.getInt32(1), "loop.indvar.next");
		IV->addIncoming(IncrementedIV, BodyBB);

		Builder.SetInsertPoint(AfterBB);
	}
}



void GreenStmt:: codegenBody(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const {
	for (auto Inst : Insts) 
		Inst->codegen(Builder, ActiveRegs);
}


ArrayRef <const GreenNode * > GreenSet:: getChildren() const  { return ArrayRef<GreenNode*>(Val); }


void GreenSet::codegen(IRBuilder<> &Builder, CodegenContext &CodegenContext )const {
	auto NewVal =Val->codegen(Builder,CodegenContext); 
	assert(NewVal);
	assert(!CodegenContext.ActiveRegs.count(Var));
  CodegenContext.	ActiveRegs[ Var] = NewVal;
}



void GreenStore::codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const {
	auto Val = getVal()->codegen(Builder,ActiveRegs);
	auto Ptr = getPtr()->codegen(Builder,ActiveRegs);
	
	Builder.CreateStore(Val, Ptr);
}


ArrayRef <const GreenNode * > GreenCall:: getChildren() const { return  ArrayRef<GreenNode*>((GreenNode**)&Operands[0],Operands.size());  }


void GreenCall:: codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const  {
	auto Callee = Operands[0]->codegen(Builder,ActiveRegs);
	SmallVector<Value *,8> Args;
	for (auto GArg : drop_begin( Operands,1)) {
		auto Arg = GArg->codegen(Builder, ActiveRegs);
		Args.push_back(Arg);
	}
	Builder.CreateCall(Callee, Args);
}


ArrayRef <const GreenExpr * > GreenExpr:: getExprChildren() const {
  auto Children = getChildren();
  return  ArrayRef<GreenExpr*>((GreenExpr**)&Children[0],Children.size()); 
}

Value* GreenConst::codegen(IRBuilder<> &Builder , CodegenContext &ActiveRegs)const  {
	return Const;
}

Value* GreenTrueLiteral:: codegen(IRBuilder<> &Builder , CodegenContext &ActiveRegs)const  {
  auto &Context = Builder.getContext();
return ConstantInt::get(Context, APInt(1 ,1));
}

Value* GreenFalseLiteral:: codegen(IRBuilder<> &Builder , CodegenContext &ActiveRegs)const  {
  auto &Context = Builder.getContext();
  return ConstantInt::get(Context, APInt(1 ,0));
}


Value* GreenArg:: codegen(IRBuilder<> &Builder , CodegenContext &ActiveRegs)const {
  return Arg;
}


Value* GreenReg:: codegen(IRBuilder<> &Builder, CodegenContext &CodegenCtx )const {
	auto Result = CodegenCtx.ActiveRegs.lookup(Var);
	assert(Result);
	return Result;
}


Value* GreenCtrl:: codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const{
	auto Result = ActiveRegs.AtomVals.lookup(Atom );
	assert(Result);
	return Result;
}

ArrayRef <const GreenNode * > GreenGEP:: getChildren() const { return  ArrayRef<GreenNode*>((GreenNode**)&Operands[0],Operands.size());  }


Value*GreenGEP:: codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs ) const {
	SmallVector<Value*,2> Indices;
	for (auto Green : getIndices()) {
		auto IndexVal=	Green->codegen(Builder,ActiveRegs);
		Indices.push_back(IndexVal);
	}

	auto BaseVal = getBase()->codegen(Builder,ActiveRegs);

	auto Result =Builder.CreateGEP(BaseVal, Indices );
return Result;
}


void  GreenICmp::printLine(raw_ostream &OS) const  {
	 getLHS()->printLine(OS);
	 OS << " ";
	 OS << CmpInst::getPredicateName(Predicate);
	 OS << " ";
	 getRHS()->printLine(OS);
}



ArrayRef <const GreenNode * > GreenICmp:: getChildren() const  {
	return  ArrayRef<GreenNode*>((GreenNode**)&Operands[0],(size_t)2); 
}


void GreenLogicOr:: printLine(raw_ostream &OS) const  {
  OS << "("  ;
  Operands[0]->printLine(OS);
  for (auto Op: drop_begin(Operands,1)) {
    OS<<" || ";
    Op->printLine(OS);
  }
  OS << ")";
}

 ArrayRef <const GreenNode * > GreenLogicOr:: getChildren() const  {return  ArrayRef<GreenNode*>((GreenNode**)&Operands[0],Operands.size());  }

 Value* GreenLogicOr:: codegen(IRBuilder<> &Builder, CodegenContext &CodegenCtx )const {
   // TODO: Shortcut-eval?

   auto Result = Operands[0]->codegen(Builder, CodegenCtx);
   for (auto Op : drop_begin(Operands,1 )) {
     auto OpResult = Op->codegen(Builder, CodegenCtx);
     Result = Builder.CreateOr(Result, OpResult);
   }

  return Result;
 }


 void GreenLogicAnd:: printLine(raw_ostream &OS) const  {
   OS << "("  ;
   Operands[0]->printLine(OS);
   for (auto Op: drop_begin(Operands,1)) {
     OS<<" && ";
     Op->printLine(OS);
   }
   OS << ")";
 }

 ArrayRef <const GreenNode * > GreenLogicAnd:: getChildren() const  {return  ArrayRef<GreenNode*>((GreenNode**)&Operands[0],Operands.size());  }

 Value* GreenLogicAnd:: codegen(IRBuilder<> &Builder, CodegenContext &CodegenCtx )const {
   // TODO: Shortcut-eval?

   auto Result = Operands[0]->codegen(Builder, CodegenCtx);
   for (auto Op : drop_begin(Operands,1 )) {
     auto OpResult = Op->codegen(Builder, CodegenCtx);
     Result = Builder.CreateAnd(Result, OpResult);
   }

   return Result;
 }


Value*GreenICmp:: codegen(IRBuilder<> &Builder , CodegenContext &ActiveRegs)const  {
	auto LHS = getLHS()->codegen(Builder,ActiveRegs);
	auto RHS = getRHS()->codegen(Builder,ActiveRegs);
  auto Result=	Builder.CreateICmp(Predicate, LHS, RHS);
  return Result;
}

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
void neverCalled2() {
	GreenRoot::create(nullptr)->dump();
}
#endif
