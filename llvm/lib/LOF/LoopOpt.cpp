

#include "LoopOpt.h"
#include "GreenTree.h"
#include "RedTree.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Transforms/Utils/LoopUtils.h"


using namespace llvm;

namespace {
	class StagedSequence;

	class StagedLoop {
	public:
		Loop *L ;
    StagedSequence *Body;
		Value *Iterations;
		AtomDisjointSet *Atoms;

		explicit StagedLoop(Loop *LoopInfoLoop, Value *Iterations, AtomDisjointSet *Atoms)  ;
	};

	
	class StagedSequence {
	public:
		SmallVector<llvm::PointerUnion< GreenStmt*, StagedLoop*> ,16> Stmts;
    SmallVector<const GreenExpr*,16> Predicates;
	SmallVector<AtomDisjointSet*,16> Atoms;

		void appendStmt(GreenStmt *Stmt, const GreenExpr *Predicate, AtomDisjointSet *AtomSet) { Stmts.push_back(Stmt);Predicates.push_back(Predicate); Atoms.push_back(AtomSet); }
		void appendLoop(StagedLoop *Loop, const GreenExpr *Predicate, AtomDisjointSet *AtomSet) { Stmts.push_back(Loop);Predicates.push_back(Predicate);   Atoms.push_back(AtomSet); } 
	};


	StagedLoop::StagedLoop(Loop *LoopInfoLoop, Value *Iterations, AtomDisjointSet *Atoms): L(LoopInfoLoop), Body(new StagedSequence()) , Iterations(Iterations),  Atoms(Atoms)  {	}



	class LoopOptimizerImpl : public LoopOptimizer {
	private:
		Function *Func;

		LoopInfo *LI;
		ScalarEvolution *SE;


		GreenRoot *OriginalRoot=nullptr;


		GreenLoop *createHierarchy(Function *F) const;
		GreenLoop *createHierarchy(Loop *L) const;
		GreenStmt *createHierarchy(BasicBlock *BB) const;

		GreenExpr *createExpr(Value *I);


		DenseMap <Value *, GreenExpr*> ExprCache;
		GreenExpr *getGreenExpr(Value *C);

    const  GreenExpr* GreenTrue = GreenTrueLiteral::create();
    const  GreenExpr* GreenFalse = GreenFalseLiteral::create();
  const  GreenExpr* getGreenBool(bool Val) {
      return Val ? GreenTrue : GreenFalse;
    }



  DenseMap <CtrlAtom *, GreenCtrl*> CtrlExprCache;
  const GreenCtrl* getGreenCtrl(CtrlAtom *Atom ) {
		auto Result = CtrlExprCache[Atom];
		if (!Result) {
			Result = GreenCtrl::create(Atom);
		}
		return Result;
	}



    const  GreenExpr *getGreenLogicOr(const GreenExpr *LHS,const GreenExpr *RHS) {
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

    const  GreenExpr *getGreenLogicAnd(const GreenExpr *LHS,const GreenExpr *RHS) {
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
        return  getGreenBool(true);
      case 1:
        return Operands[0];
      default:
        return GreenLogicAnd::create(Operands);
      }
    }


		DenseMap <Instruction *, GreenInst*> InstCache; // FIXME: Instructions may not be re-usable, so do not cache.
		GreenInst *getGreenInst(Instruction *I) ;

		GreenStmt *createGreenStmt(const GreenExpr *MustExecutePredicate,const GreenExpr *MayExecutePredicate,ArrayRef<GreenInst*> Insts,AtomDisjointSet *Atoms);



		GreenLoop* createGreenLoop(const GreenExpr* Predicate,StagedLoop *Staged ) ;
		GreenSequence* createGreenSequence(StagedSequence *Sequence) ;
		GreenRoot *createGreenRoot(StagedSequence *TopLoop) ;


	const	GreenSequence *parallelizeSequence(const GreenSequence *Seq) {
			bool Changed = false;
			SmallVector<const GreenBlock*,32> NewBlocks;
			for (auto Block : Seq->blocks()) {
				const GreenBlock *NewBlock;
				if (auto Stmt = dyn_cast<GreenStmt>(Block)) {
					NewBlock = Stmt;
				} else if (auto Loop = dyn_cast<GreenLoop>(Block)) {
					NewBlock = parallelizeLoop(Loop);
				} else 
					llvm_unreachable("unexpected");

				if (Block != NewBlock)
					Changed=true;

				NewBlocks.push_back(NewBlock);
			}	

			if (!Changed)
				return Seq;

			return GreenSequence::create(NewBlocks);
		}

	const	GreenLoop *parallelizeLoop(const GreenLoop *Loop) {
			auto L = Loop->getLoopInfoLoop();
			if (!L)
				return Loop;

      auto HasPragmaParallelize = getBooleanLoopAttribute(L,  "llvm.loop.parallelize_thread.enable"    );
			if (!HasPragmaParallelize && !L->isAnnotatedParallel())
				return Loop;
			if (Loop->isExecutedInParallel())
				return Loop;

			auto Cloned = Loop->clone();
			Cloned->setExecuteInParallel();
			return Cloned;
		}



	public:
		LoopOptimizerImpl(Function *Func, LoopInfo*LI, ScalarEvolution *SE) : Func(Func), LI(LI) ,SE(SE){}

		GreenRoot * buildOriginalLoopTree();
		const GreenRoot *parallelize(const GreenRoot *Root);
		void codegen(const GreenRoot *Root);
		 

		bool optimize()override ;

		void view(const GreenRoot *Root);

		void print(raw_ostream &OS) override {
			OS << "Nothing to print yet\n";
		}
	};
}

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


GreenExpr *LoopOptimizerImpl::createExpr(Value *I)  {
	if (auto P = dyn_cast<Argument>(I)) {
		auto Green = GreenArg::create(P);
		return Green;
	}

	if (auto C = dyn_cast<Constant>(I)) {
		auto Green = GreenConst::create(C);
		return Green;
	}
	
	if (auto PHI = dyn_cast<PHINode>(I)) {
		// FIXME: This is not really a register use; replace by case instruction or other closed-form (AddRecExpr) expression 
		auto Green = GreenReg::create(PHI);
		return Green;
	}

	if (auto GEP = dyn_cast<GetElementPtrInst>(I)) {
		SmallVector<GreenExpr*,4> Exprs;
		for (auto Op : GEP->operand_values()) {
			auto GreenOp = getGreenExpr(Op);
			Exprs.push_back(GreenOp);
		}
		auto Green = GreenGEP::create(Exprs);
		return Green;
	}

	if (auto E = dyn_cast<ICmpInst>(I)) {
		assert(E->getNumOperands()==2);
		auto LHS = getGreenExpr(E->getOperand(0));
		auto RHS = getGreenExpr(E->getOperand(1));
		return GreenICmp::create( E->getPredicate(), LHS, RHS);
	}

	llvm_unreachable("unimplemented");
}



GreenInst *LoopOptimizerImpl::getGreenInst(Instruction *I) {
	auto &Result = InstCache[I];
	if (!Result) {
		if (auto S = dyn_cast<StoreInst>(I)) {
			auto Val = getGreenExpr(S->getValueOperand());
			auto Ptr = getGreenExpr(S->getPointerOperand());
			Result = GreenStore::create(Val, Ptr);
		}	else if (auto C = dyn_cast<CallInst>(I) ) {
			auto Callee =   getGreenExpr( C->getCalledOperand());
			SmallVector<const  GreenExpr *,8> Ops;
			for (auto &Op : C->args()) {
				auto Val = getGreenExpr(Op.get());
				Ops.push_back(Val); 
			}
			Result = GreenCall::create(Callee, Ops);
		} else {
			// Register definition
			assert(!I->mayHaveSideEffects());
			auto Expr = getGreenExpr(I);
			Result = GreenSet::create(I,Expr);
		}
	}
	return Result;
}


GreenStmt *LoopOptimizerImpl:: createGreenStmt(const GreenExpr *MustExecutePredicate,const GreenExpr *MayExecutePredicate,ArrayRef<GreenInst*> Insts, AtomDisjointSet *Atoms) {
	return GreenStmt::create(MustExecutePredicate,MayExecutePredicate,Insts,Atoms);
}


GreenLoop* LoopOptimizerImpl:: createGreenLoop(const GreenExpr* Predicate, StagedLoop *Staged ) {
	auto Seq = createGreenSequence(Staged->Body);
auto Iters=	getGreenExpr(Staged->Iterations);
	return	GreenLoop::create(Predicate,Predicate,Iters, Staged->L->getCanonicalInductionVariable(), Seq, Staged->L, Staged->Atoms);
}

GreenSequence* LoopOptimizerImpl:: createGreenSequence(StagedSequence *Sequence) {
	SmallVector<GreenBlock *,32> Blocks;

	for(auto Pair : zip( Sequence->Stmts,Sequence->Predicates)) {
    auto Block = std::get<0>(Pair);
    auto Predicate = std::get<1>(Pair);
		if (auto Stmt = Block.dyn_cast<GreenStmt*>()) {
			Blocks.push_back(Stmt);
		} else if (auto Loop = Block.dyn_cast<StagedLoop*>()) {
			auto GLoop = createGreenLoop(Predicate,Loop);
			Blocks.push_back(GLoop);
} else 
llvm_unreachable("Something is wrong");
			}

	auto Green = GreenSequence::create(Blocks);
	return Green;
		} 

GreenRoot *LoopOptimizerImpl:: createGreenRoot(StagedSequence *TopLoop) {
	auto GSeq = createGreenSequence(TopLoop);
	auto Green = GreenRoot::create(GSeq);
	return Green;
}






GreenRoot *LoopOptimizerImpl::buildOriginalLoopTree() {

	DenseMap<BasicBlock*, AtomDisjointSet* > SuccessorAtoms;
	for (auto &BB : *Func) {
		auto Term = BB.getTerminator();
		if (auto Br = dyn_cast<BranchInst>(Term)) {
			AtomDisjointSet *AtomSet;
			if (Br->isUnconditional()) {
				AtomSet =  new AtomDisjointSet(1);
			} else {
				AtomSet =  new AtomDisjointSet(2);
			}
			SuccessorAtoms[&BB] = AtomSet;
			continue;
		} else if (isa<ReturnInst>(Term)) {
			// No successors
			continue;
		} else if (isa<UnreachableInst>(Term)) {
			// No successors
			continue;
		}
		llvm_unreachable("unimplemented terminator");
	}


	DenseMap<Loop*, AtomDisjointSet* > LoopExitAtoms;
	for (auto L : LI->getLoopsInPreorder()) {
		SmallVector<BasicBlock*,4> ExitBlocks;
		L->getExitBlocks(ExitBlocks);
		auto AtomSet = new AtomDisjointSet(ExitBlocks.size());
		LoopExitAtoms[L] = AtomSet;
	}



	
	
	DenseMap<Loop *, StagedLoop *> LoopMap;
  LoopMap[nullptr] = new StagedLoop(nullptr, nullptr, LoopExitAtoms.lookup(nullptr) );
  StagedSequence *RootBlock = LoopMap[nullptr]->Body;
  for (auto L : LI->getLoopsInPreorder()) {
    // TODO: Use own analysis on loop tree instead of SCEV.
    auto Taken = SE->getBackedgeTakenCount(L);
    Taken = SE->getSCEVAtScope(Taken, L->getParentLoop());

    // auto IterCount = SE->getMinusSCEV(Taken,  SE->getSCEV(
    // ConstantInt::get(Context, APInt(2, -1, true) ) ) );
    Value *IterCountV;
    if (isa<SCEVSMaxExpr>(Taken)) {
      IterCountV = cast<SCEVUnknown>(cast<SCEVSMaxExpr>(Taken)->getOperand(1))->getValue();
    } else {
      IterCountV = cast<SCEVConstant>(Taken)->getValue();
    }

    // FIXME: This assume the form without loop-rotation
    //    for (int = 0; i < 0; i+=1)
    assert(IterCountV);
    LoopMap[L] = new StagedLoop(L, IterCountV, LoopExitAtoms.lookup(L) );
  }

  DenseMap<BasicBlock *, const GreenExpr *> BBPredicate;
  BBPredicate[&Func->getEntryBlock()] = getGreenBool(true); // Assume no branch to entry




  // Build a temporary loop tree.
  ReversePostOrderTraversal<Function *> RPOT(Func);
  for (auto Block : RPOT) {
    auto Loop = LI->getLoopFor(Block);
    auto SLoop = LoopMap.lookup(Loop);
    auto SBody = SLoop->Body;

    bool isLoopHeader = Loop && Block == Loop->getHeader();

    auto &Predicate = BBPredicate[Block];
	int NPreds = 0;
    if (!Predicate) {
      Predicate = getGreenBool(false);
	  for (auto Pred : predecessors(Block)) {
		  if (isLoopHeader && Loop->contains(Pred)) {
			  // We assume that loops are the only reason for back-edges => no irreducible loops.
			  continue;
		  }

		  auto PredLoop = LI->getLoopFor(Pred);
		  CtrlAtom *PredAtom = nullptr;
		  if (PredLoop&& PredLoop->isLoopExiting(Block)) {
			  assert(PredLoop);
			  // If this is exiting from a loop, take the loop's own predicate to
			  // ignore the exit-loop condition, i.e. assume that the loop will
			  // leave eventually.
			  // FIXME: This assumes we're exiting at most one loop.

			  // Find the number of the exit loop
			  SmallVector<BasicBlock*, 4> ExitBlocks;
			  PredLoop->getExitBlocks(ExitBlocks);
			  int ExitIdx = -1;
			  for (int i = 0; i < ExitBlocks.size(); i += 1) {
				  if (ExitBlocks[i] != Pred) {
					  ExitIdx = i;
					  break;
				  }
			  }
			  assert(ExitIdx >= 0);

			  auto ExitAtomSet = LoopExitAtoms[PredLoop];
			  PredAtom = ExitAtomSet->getAtom(ExitIdx);
	  }	else {
			auto BlockAtomSet = SuccessorAtoms[Pred];

			if (auto Br = dyn_cast<BranchInst>(Pred->getTerminator())) {
				int SuccIdx = -1;
		
				for (int i = 0; i < Br->getNumSuccessors(); i+=1) {
					if (Br->getSuccessor(i) ==Block ) {
						// FIXME: This assumes that there can be only a single edge from predecessor to block (which is problably true because PHINodes cannot differentiate edges)
					SuccIdx=i;
					break;
					}
				}

				PredAtom = BlockAtomSet->getAtom(SuccIdx);
			} else 
				llvm_unreachable("Unhandled terminator");
		
			NPreds += 1;
		  }
		assert(PredAtom);
		auto PredCond=  getGreenCtrl(PredAtom);
		Predicate = getGreenLogicOr(Predicate, PredCond);

#if 0
        auto PredLoop = LI->getLoopFor(Pred);
        const GreenExpr *PredPredicate;

        if (!isLoopHeader && (Loop != PredLoop)) {
          // If this is exiting from a loop, take the loop's own predicate to
          // ignore the exit-loop condition, i.e. assume that the loop will
          // leave eventually.
          // FIXME: Multiple loop exits => atoms.
          PredPredicate = BBPredicate.lookup(LoopMap.lookup(PredLoop)->L->getHeader());
        } else {
          PredPredicate = BBPredicate.lookup(Pred);
        }

        assert(PredPredicate);


        if (Loop && Loop->isLoopExiting(Pred)) {
          // Ignore continue-loop conditions to avoid them to be propagated to
          // the statement predicates; these are implicit by the surrounding
          // loop
          Predicate = getGreenLogicOr(Predicate, PredPredicate);
          continue;
        } else if (auto Br = dyn_cast<BranchInst>(Pred->getTerminator())) {
          const GreenExpr *Cond;
          if (Br->isUnconditional()) {
            Cond = getGreenBool(true);
          } else {
            Cond = getGreenExpr(Br->getCondition());
          }

          auto AndCond = getGreenLogicAnd(PredPredicate, Cond);
          Predicate = getGreenLogicOr(Predicate, AndCond);
          continue;
        }
        llvm_unreachable("unimplemented terminator");
#endif
      }

    }

    if (isLoopHeader) {
      auto ParentLoop = Loop->getParentLoop();
      auto ParentSLoop = LoopMap.lookup(ParentLoop);
      auto ParentSBody = ParentSLoop->Body;
      ParentSBody->appendLoop(SLoop, Predicate, LoopExitAtoms.lookup(SLoop->L) );
    }

    for (auto &I : *Block) {
      if (I.isTerminator())
        continue;
      if (!I.mayHaveSideEffects())
        continue;
      auto Inst = getGreenInst(&I);
      auto Stmt = createGreenStmt(Predicate, Predicate, {Inst}, nullptr);

      SBody->appendStmt(Stmt, Predicate,  nullptr);
    }

	auto Term = getGreenInst(Block->getTerminator());
	auto TermStmt = createGreenStmt(Predicate, Predicate, {Term}, SuccessorAtoms.lookup(Block));
	SBody->appendStmt(TermStmt, Predicate, SuccessorAtoms.lookup(Block) );

  }

  OriginalRoot = createGreenRoot(RootBlock);
  return OriginalRoot;
}



const GreenRoot *LoopOptimizerImpl::parallelize(const GreenRoot *Root){
	auto NewSeq= parallelizeSequence(Root->getSequence());
	if (NewSeq==Root->getSequence())
		return Root;

	auto NewRoot = Root->clone();
	NewRoot->setSequence(NewSeq);
	return NewRoot;
}





void LoopOptimizerImpl::codegen(const GreenRoot *Root) {
	auto M = Func->getParent();
	auto &Context = M->getContext();

	// Make a clone of the original function.
	FunctionType *FT = Func->getFunctionType()  ;
	std::string FuncName = Func->getName();
	Function *NewFunc = Function::Create(FT, Func->getLinkage(), Twine(), M);
	NewFunc->addFnAttr("lof-output");
	// TODO: Carry-over function attributes

	BasicBlock *EntryBB = BasicBlock::Create(Context, "entry", NewFunc);

	IRBuilder<> Builder(EntryBB);
  CodegenContext CodegenCtx;
	auto & ActiveRegs=CodegenCtx.ActiveRegs;
	for ( auto P : zip( Func->args(), NewFunc->args() )  ) {
		ActiveRegs.insert({ &std::get<0>(P) ,&std:: get<1>(P) } );
	}

	Root->getSequence()->codegen(Builder, CodegenCtx);

	if (FT->getReturnType()->isVoidTy())
		Builder.CreateRetVoid();
	else 
		Builder.CreateRet( Builder.getInt32(0) );
	


	// Remove old function
	// FIXME: Cannot remove function while being processed, so we just make it 'unused' here at rely on some cleanup pass to actually remove it. 
	// FIXME: FunctionPassManager has not been written for removing/adding functions during passes. It will ignore added functions and continue to process the currently processed function even if it was removed. We may need to switch to be a CGSCC pass, which supports adding/removing functions, bu will compute a call graph that we do not need. Howver, when we want to process OpenMP frontend-outlined subfunctions, we will need to become an CGSCC pass. 
	Func->replaceAllUsesWith(NewFunc); // This might be evil in a FunctionPass
	Func->setLinkage(GlobalValue::PrivateLinkage);
	Func->setName(Twine(".") + FuncName + Twine(".") );

	// Assign the name after removing the previous to avoid name disambiguation.
	NewFunc->setName(FuncName);
}



bool LoopOptimizerImpl::optimize() {
	auto OrigTree = buildOriginalLoopTree();

  auto OrigRedTree = RedRoot::Create(OrigTree);
  OrigRedTree->findAllDefinitions();

	auto OptimizedTree = parallelize(OrigTree);
	if (OptimizedTree == OrigTree)
		return false;

	view(OptimizedTree);

	codegen(OptimizedTree);
	return true;
}




template <> 
struct llvm::GraphTraits<const llvm::GreenNode *> {
	using GraphRef = const GreenNode *;
	using NodeRef = const GreenNode *;

	static NodeRef getEntryNode(GraphRef L) { return L; }

	using ChildIteratorType =  ArrayRef<NodeRef>::iterator ;
	static ChildIteratorType child_begin(NodeRef N) { return N->getChildren().begin(); }
	static ChildIteratorType child_end(NodeRef N) { return N->getChildren().end(); }

	using nodes_iterator = df_iterator<NodeRef>;
	static nodes_iterator nodes_begin(GraphRef RI) {
		return nodes_iterator::begin(getEntryNode(RI));
	}
	static nodes_iterator nodes_end(GraphRef RI) {
		return nodes_iterator::end(getEntryNode(RI));
	}
};


template <> 
struct llvm:: DOTGraphTraits<const llvm:: GreenNode *> : public DefaultDOTGraphTraits {
	using GraphRef = const GreenNode *;
	using NodeRef = const GreenNode *;

	DOTGraphTraits(bool isSimple = false) : DefaultDOTGraphTraits(isSimple) {}

	std::string getNodeLabel(NodeRef Node, GraphRef Graph) {
		SmallString<256> Result;
		raw_svector_ostream OS(Result);
		if (isSimple()) 
			Node->printLine(OS);
		else
			Node->printText(OS);
		return OS.str();
	}
};



void LoopOptimizerImpl:: view(const GreenRoot *Root) {
  ViewGraph<const GreenNode *>(Root, "lof", false, "Loop Hierarchy Graph");
}




LoopOptimizer *llvm::createLoopOptimizer(Function*Func,LoopInfo*LI,ScalarEvolution *SE) {
	return new LoopOptimizerImpl(Func,LI,SE);
}

