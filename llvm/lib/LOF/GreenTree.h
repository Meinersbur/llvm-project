#ifndef LLVM_LOF_GREENTREE_H
#define LLVM_LOF_GREENTREE_H

#include "llvm/Support/raw_ostream.h"
#include <vector>
#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/Instruction.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/ADT/SetVector.h"

namespace llvm {
	enum class LoopHierarchyKind {
#define HANDLE_NODE(ShortName, LongName) \
  ShortName,
#define HANDLE_SUBKIND(ShortName, LongName, First, Last) \
  ShortName ## _First = First,   \
  ShortName ## _Last = Last,
  // TODO: Rename ShortName ## FirstVal, like Value.def
#include "NodeKinds.def"
	};


	class GreenInst;
	class GreenExpr;
	class GreenBlock;
	class CtrlAtom;





  // TOOD: Refactor-out codegen; does not need to be part of the central data structure. 
  // Maybe use visitor-pattern.
  class CodegenContext {
  public:
    DenseMap<Value*,Value*> ActiveRegs;
	DenseMap<CtrlAtom* ,Value*> AtomVals;

    SmallVector<std::pair<const GreenExpr* ,BasicBlock*> , 8>  PrecomputedPredicates;
	DenseMap<CtrlAtom*,BasicBlock*> PrecomputedAtoms;
  };

	



	// Immutable (sub-)tree, only contains references to its children.
	class GreenNode {
	public:
		virtual ~GreenNode() {};

		virtual LoopHierarchyKind getKind() const = 0;
		static bool classof(const GreenNode *) {	return true;	}

		void dump() const { printText(errs()); }
		virtual void printLine(raw_ostream &OS) const {}
		virtual void printText(raw_ostream &OS) const { printLine(OS); OS << '\n'; }

		virtual ArrayRef <const  GreenNode * > getChildren() const = 0;
    iterator_range <ArrayRef <const  GreenNode * > ::const_iterator> children() const {
      auto Children = getChildren();
      return make_range(Children.begin(), Children.end());
    }

		// TODO: Use general tree search algorithm
	// TODO: Consider caching result
		virtual void findRegUses(SetVector <Value*>  &UseRegs) const {
			for (auto Child : getChildren()) 
				Child->findRegUses(UseRegs);
		}
		virtual void findRegDefs(SetVector<Instruction*>  &DefRegs) const {
			for (auto Child : getChildren()) 
				Child->findRegDefs(DefRegs);
		}
	};


	// TODO: Integrate into GreenLoop: Everything is a loop (possible with just one iteration) 
	class GreenSequence final : public GreenNode {
	private:
		// TODO: Do the same allocation trick.; Since GreenSequence is always part of either a GreenRoot or GreenLoop, can also allocate in their memory
		std::vector<const GreenBlock *> Blocks;

	public:
		GreenSequence (ArrayRef<const GreenBlock*> Blocks): Blocks(Blocks) {}
		virtual ~GreenSequence() {};

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Sequence; }
		static bool classof(const GreenNode *Node) { return  Node->getKind() == LoopHierarchyKind::Sequence; }
		static bool classof(const GreenSequence *) {	return true;	}

		 void printLine(raw_ostream &OS) const  override {OS << "Sequence";}

		virtual ArrayRef <const  GreenNode * > getChildren() const override ;

		iterator_range<	 decltype(Blocks)::const_iterator>  blocks() const { return llvm::make_range (Blocks.begin(), Blocks.end()) ; }

		static 	GreenSequence *create(ArrayRef<const GreenBlock*> Blocks) {  return new GreenSequence(Blocks); }

		 void codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const;
	};





	
	class GreenRoot : public GreenNode {
	private :
		const GreenSequence *Sequence;

	public:
		GreenRoot (const GreenSequence *Sequence): Sequence(Sequence) {}
		GreenRoot *clone() const { auto That =create(Sequence); return That; }
		virtual ~GreenRoot() {};

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Root; }
		static bool classof(const GreenNode *Node) { return  Node->getKind() == LoopHierarchyKind::Root; }
		static bool classof(const GreenRoot *) {	return true;	}

		 void printLine(raw_ostream &OS) const override {OS << "Root";}

		virtual ArrayRef <const  GreenNode * > getChildren() const override ;

		const GreenSequence *getSequence()const { return Sequence; }
		void setSequence(const GreenSequence *Sequence) { this->Sequence=Sequence; }

		static 	GreenRoot *create(const  GreenSequence *Sequence) {  return new GreenRoot(  Sequence); }
	}; 




  class AtomDisjointSet;

  class CtrlAtom {
  private:
    AtomDisjointSet *Parent;
  public:
	explicit  CtrlAtom  ( AtomDisjointSet *Parent): Parent(Parent) {}
  };




  // In such a set, exactly one of the atoms will be true
  // Unconditional branch: One atoms (Tautology)
  // Conditional branch: Two atoms (IfTrue, IfFalse)
  // switch: As many atoms as cases + default
  // indirectbr: As many atoms as valid target labels
  // invoke: Two atoms (normal, unwind)
  // Instruction::mayThrow(): Could be two atoms (Or more efficient implementation with a flag that causes control dependencies)
  // Loop: One atom per exiting edge
  class AtomDisjointSet {
  private:
    // Number of loop nests
    int Dims = 0;

    SmallVector<CtrlAtom*,2> Atoms;
  public:
    AtomDisjointSet(int NumAtoms) {  
      Atoms.resize(NumAtoms);
      for (auto &Atom : Atoms)
          Atom = new CtrlAtom(this);
    }

    auto atoms() const -> llvm::iterator_range<decltype(Atoms.begin())> { return make_range(Atoms.begin(), Atoms.end()); }
	CtrlAtom* getAtom(int Idx) const { return Atoms[Idx]; }

	int getNumAtoms() const { return Atoms.size(); }
  };


	// A statement or loop (abstract class)
	class GreenBlock  : public GreenNode  {
	private :
   const GreenExpr *MustExecutePredicate;
   const GreenExpr *MayExecutePredicate;

   protected:
    AtomDisjointSet *AtomSet;

  protected:
    GreenBlock(const GreenExpr *MustExecutePredicate,const GreenExpr *MayExecutePredicate, AtomDisjointSet*Atoms) :
      GreenNode(), MustExecutePredicate(MustExecutePredicate), MayExecutePredicate(MayExecutePredicate), AtomSet(Atoms) {
    }
	GreenBlock(const GreenExpr *MustExecutePredicate,const GreenExpr *MayExecutePredicate, int NumAtoms) : 
		GreenBlock(MustExecutePredicate, MayExecutePredicate, new AtomDisjointSet(NumAtoms) ){}


    void printLineCond(raw_ostream &OS) const;
	public:
		//GreenBlock (ArrayRef<const GreenSequence*const> Stmts): Stmts(Stmts) {}
		//virtual ~GreenBlock() {};

		// virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Block; }
		static bool classof(const GreenNode *Node) {	auto Kind =Node-> getKind(); return LoopHierarchyKind::Block_First <= Kind && Kind <= LoopHierarchyKind::Block_Last;	}
		static bool classof(const GreenBlock *) {	return true;	}


	// TODO: Should these be returned by children() as well?
    const GreenExpr * getMustExecutePredicate() const {return MustExecutePredicate;}
    const GreenExpr * getMayExecutePredicate() const {return MayExecutePredicate;}

    auto atoms() const -> decltype(AtomSet->atoms()) { return AtomSet->atoms(); }
    
		//	virtual ArrayRef <const GreenNode * const> getChildren() const override { return Stmts;}// ArrayRef<const GreenNode * const>( Stmts.data(), Stmts.size()); };
	
		void codegen(IRBuilder<> &Builder, CodegenContext &CodegenCtx)const;
		virtual void codegenBody(IRBuilder<> &Builder, CodegenContext &CodegenCtx)const=0;

    //void codegenPredicate(IRBuilder<> &Builder, CodegenContext &ActiveRegs, const std::function <void(IRBuilder<> &, CodegenContext &)> &CodegenBody) const;
	}; 



	class GreenLoop final : public GreenBlock {
	private:
		GreenExpr *Iterations;
		Instruction *IndVar;
		GreenSequence *Sequence;

		Loop *LoopInfoLoop;

		bool ExecuteInParallel = false;

		StructType *getIdentTy(Module *M) const;
		Function * codegenSubfunc(Module *M, SmallVectorImpl<Value*>& UseRegs)const 			;
	public:
		GreenLoop (const GreenExpr *MustExecutePredicate,const GreenExpr *MayExecutePredicate,GreenExpr *Iterations, Instruction *IndVar,GreenSequence *Sequence,Loop* LoopInfoLoop, AtomDisjointSet*AtomSet): GreenBlock(MustExecutePredicate, MayExecutePredicate,AtomSet), Iterations(Iterations), IndVar(IndVar), Sequence(Sequence), LoopInfoLoop(LoopInfoLoop) {}
		GreenLoop *clone() const { auto That = create(getMustExecutePredicate(), getMayExecutePredicate(),Iterations,IndVar,Sequence,nullptr , AtomSet ); That->ExecuteInParallel= this->ExecuteInParallel; return That; }
		virtual ~GreenLoop() {};

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Loop; }
		static bool classof(const GreenNode *Node) {	return Node->getKind() == LoopHierarchyKind::Loop; 	}
		static bool classof(const GreenLoop *) {	return true;	}

		 void printLine(raw_ostream &OS) const override {
       printLineCond(OS);
       OS << "Loop";
     }

		virtual ArrayRef <const GreenNode * > getChildren() const override ;
		Loop *getLoopInfoLoop() const {return LoopInfoLoop;}

		bool isExecutedInParallel() const {return ExecuteInParallel;}
		void setExecuteInParallel(bool ExecuteInParallel = true) { this->ExecuteInParallel= ExecuteInParallel;  }

		static 	GreenLoop *create(const GreenExpr *MustExecutePredicate,const GreenExpr *MayExecutePredicate,GreenExpr *Iterations,Instruction *IndVar,GreenSequence *Sequence,Loop* LoopInfoLoop, AtomDisjointSet *AtomSet) {  return new GreenLoop(MustExecutePredicate, MayExecutePredicate,Iterations,IndVar, Sequence, LoopInfoLoop,AtomSet); }
	
		void codegenBody(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const override;
	};





	class GreenStmt final : public GreenBlock {
	private:
		std::vector<GreenInst*> Insts;


	public:
		GreenStmt(const GreenExpr *MustExecutePredicate,const GreenExpr *MayExecutePredicate, ArrayRef<GreenInst*> Insts, AtomDisjointSet *AtomsSet);

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Stmt; }
		static bool classof(const GreenNode *Node) {	return Node->getKind() == LoopHierarchyKind::Stmt; 	}
		static bool classof(const GreenStmt *) {	return true;	}

		void printLine(raw_ostream &OS) const override ;

		virtual ArrayRef <const GreenNode * > getChildren() const override;

		static GreenStmt*create(const GreenExpr *MustExecutePredicate,const GreenExpr *MayExecutePredicate, ArrayRef<GreenInst*> Insts, AtomDisjointSet *AtomSet) { return new GreenStmt(MustExecutePredicate, MayExecutePredicate,Insts, AtomSet); };

		void codegenBody(IRBuilder<> &Builder, CodegenContext &ActiveRegs) const override;
	};




	class GreenInst : public GreenNode {
	private:
	public:
		static bool classof(const GreenNode *Node)  {	auto Kind =Node-> getKind(); return LoopHierarchyKind::Inst_First <= Kind && Kind <= LoopHierarchyKind::Inst_Last;	}
		static bool classof(const GreenInst *) {	return true;	}

		virtual void codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const =0;
	};




	class GreenSet  : public GreenInst {
	private:
		// For now, the llvm::Instruction is the 'name' of the register that is set.
		Instruction *Var;

		GreenExpr *Val;


	public:
		GreenSet(Instruction *Var, GreenExpr *Val) : Var{Var}, Val{Val} {}

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Set; }
		static bool classof(const GreenNode *Node) { return Node->getKind() == LoopHierarchyKind::Set; }
		static bool classof(const GreenSet *) { return true; }

    Instruction *getVar() const { return Var; }
		GreenExpr * getVal() const { return Val; }
		//GreenExpr * &getVal()  {return Val; }

		void printLine(raw_ostream &OS) const override {
			Var->printAsOperand(OS, false);
			OS << " = ...";
		}

		virtual ArrayRef <const GreenNode * > getChildren() const override;

		static GreenSet*create(Instruction *Var, GreenExpr *Val) { return new GreenSet(Var, Val); };

		void codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const override;


		void findRegDefs(SetVector<Instruction*>  &DefRegs) const override {
			DefRegs.insert(Var);
			GreenInst::findRegDefs(DefRegs);
		}
	};






	class GreenStore  : public GreenInst {
	private:
		GreenExpr *Operands [2];


	public:
		GreenStore(GreenExpr *Val, GreenExpr *Ptr) : Operands{Val,Ptr} {}

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Store; }
		static bool classof(const GreenNode *Node) { return Node->getKind() == LoopHierarchyKind::Store; }
		static bool classof(const GreenStore *) { return true; }

		GreenExpr * getVal() const {return Operands[0]; }
		//GreenExpr * &getVal()  {return Operands[0]; }
		GreenExpr * getPtr() const {return Operands[1]; }
		//GreenExpr * &getPtr()  {return Operands[1]; }

		void printLine(raw_ostream &OS) const override {OS << "Store";}

		virtual ArrayRef <const GreenNode * > getChildren() const override ;

		static GreenStore*create(GreenExpr *Val, GreenExpr *Ptr) { return new GreenStore(Val, Ptr); };

		 void codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const override;
	};





	class GreenCall  : public GreenInst {
	private:
		SmallVector<const GreenExpr *, 4> Operands;


	public:
		GreenCall(const GreenExpr *Func,ArrayRef<const GreenExpr*> Arguments)  {
			Operands.push_back(Func);
			Operands.insert(Operands.end(), Arguments.begin(), Arguments.end());
		}

		virtual LoopHierarchyKind getKind() const override { return LoopHierarchyKind::Call; }
		static bool classof(const GreenNode *Node) { return Node->getKind() == LoopHierarchyKind::Call; }
		static bool classof(const GreenCall *) { return true; }

		void printLine(raw_ostream &OS) const override {OS << "Call";}

		virtual ArrayRef <const GreenNode * > getChildren() const override;

		static GreenCall*create(const GreenExpr *Func,ArrayRef<const GreenExpr*> Operands) { return new GreenCall(Func, Operands); };

		void codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const override;
	};








	class GreenExpr: public GreenNode {
	private:
	public:
		static bool classof(const GreenNode *Node)  {	auto Kind =Node-> getKind(); return LoopHierarchyKind::Expr_First <= Kind && Kind <= LoopHierarchyKind::Expr_Last;	}
		static bool classof(const GreenInst *) {	return true;	}

    ArrayRef <const GreenExpr * > getExprChildren() const ;

		virtual Value* codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const =0;
	};


	// Expression tree leaf
	class GreenConst final : public GreenExpr {
		Constant *Const;
	public:
		GreenConst(Constant *Const) : Const(Const) {}

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Const; }
		static bool classof(const GreenNode *Node) {	return Node->getKind() == LoopHierarchyKind::Const; 	}
		static bool classof(const GreenConst *) {	return true;	}

		void printLine(raw_ostream &OS) const override {
			Const->printAsOperand(OS, false);
		}

		virtual ArrayRef <const GreenNode * > getChildren() const override { return {}; };

		static  GreenConst *create(Constant *C) { return new GreenConst(C); }

		 Value* codegen(IRBuilder<> &Builder , CodegenContext &ActiveRegs)const override;
	};


  class GreenTrueLiteral final : public GreenExpr {
  private:
  protected:
    GreenTrueLiteral() {}
  public:
    virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::LiteralTrue; }
    static bool classof(const GreenNode *Node) {	return Node->getKind() == LoopHierarchyKind::LiteralTrue; 	}
    static bool classof(const GreenTrueLiteral *) {	return true;	}

    void printLine(raw_ostream &OS) const override { OS <<"true"; }

    virtual ArrayRef <const GreenNode * > getChildren() const override { return {}; };

    static  GreenTrueLiteral *create() { return new GreenTrueLiteral(); }

    Value* codegen(IRBuilder<> &Builder , CodegenContext &ActiveRegs)const override;
  };




  class GreenFalseLiteral final : public GreenExpr {
  private:
  protected:
    GreenFalseLiteral() {}
  public:
    virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::LiteralFalse; }
    static bool classof(const GreenNode *Node) {	return Node->getKind() == LoopHierarchyKind::LiteralFalse; 	}
    static bool classof(const GreenFalseLiteral *) {	return true;	}

    void printLine(raw_ostream &OS) const override { OS <<"false"; }

    virtual ArrayRef <const GreenNode * > getChildren() const override { return {}; };

    static  GreenFalseLiteral *create() { return new GreenFalseLiteral(); }

    Value* codegen(IRBuilder<> &Builder , CodegenContext &ActiveRegs)const override;
  };




  // Something that is set before the tree, but fixed during the tree's execution, i.e. an Argument or global.
  // TODO: Common superclass for GreenConst and GreenArg?
  class GreenArg final : public GreenExpr {
    Value *Arg;
  public:
    GreenArg(Value *Arg) : Arg(Arg) {}

    virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Arg; }
    static bool classof(const GreenNode *Node) {	return Node->getKind() == LoopHierarchyKind::Arg; 	}
    static bool classof(const GreenArg *) {	return true;	}

    void printLine(raw_ostream &OS) const override {
      Arg->printAsOperand(OS, false);
    }

    virtual ArrayRef <const GreenNode * > getChildren() const override { return {}; };

    static  GreenArg *create(Value *Arg) { return new GreenArg(Arg); }

    Value* codegen(IRBuilder<> &Builder , CodegenContext &ActiveRegs)const override;
  };



	// Expression tree leaf
	class GreenReg final : public GreenExpr {
		Instruction *Var; 
	public:
		GreenReg(Instruction *Var) : Var(Var) {}

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Reg; }
		static bool classof(const GreenNode *Node) {	return Node->getKind() == LoopHierarchyKind::Reg; 	}
		static bool classof(const GreenReg *) {	return true;	}

		void printLine(raw_ostream &OS) const override {
			Var->printAsOperand(OS, false);
		}

		virtual ArrayRef <const GreenNode * > getChildren() const override { return {}; };
    Value *getVar() const {return Var;}


		static  GreenReg *create(Instruction *Var) { return new GreenReg(Var); }

		Value* codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const override;

		 void findRegUses(SetVector<Value*>  &UseRegs) const override {
			UseRegs.insert(Var);
		}
	};


  // Join with GreenReg?
  class GreenCtrl final : public GreenExpr {
  private:
	  CtrlAtom *Atom;

  public:
    GreenCtrl( CtrlAtom *Atom) : Atom(Atom) {}

    virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::Ctrl; }
    static bool classof(const GreenNode *Node) {	return Node->getKind() == LoopHierarchyKind::Ctrl; 	}
    static bool classof(const GreenCtrl *) {	return true;	}

    void printLine(raw_ostream &OS) const override {
      OS << "Ctrl";
    }

    virtual ArrayRef <const GreenNode * > getChildren() const override { return {}; };
    static  GreenCtrl *create( CtrlAtom *Atom) { return new GreenCtrl(Atom); }

	Value* codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs)const override;
  };





	class GreenGEP final : public GreenExpr {
	private:
		// TODO: Do the trick to allocate the variable array after the object
		SmallVector<GreenExpr *, 3> Operands;

	public:
		// TODO: Make private
		GreenGEP(ArrayRef<GreenExpr*>Operands ) : Operands(Operands.begin(), Operands.end()) {}

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::GEP; }
		static bool classof(const GreenNode *Node) { return Node->getKind() == LoopHierarchyKind::GEP; 	}
		static bool classof(const GreenGEP *) {	return true;	}

		ArrayRef<GreenExpr*> getOperands() const { return Operands; }
		GreenExpr *getBase() const { assert(Operands.size() >=1); return Operands[0]; }
		ArrayRef<GreenExpr*> getIndices() const { return ArrayRef<GreenExpr*>(Operands).drop_front(1) ; }

		void printLine(raw_ostream &OS) const override {OS << "GetElementPtr";}

		virtual ArrayRef <const GreenNode * > getChildren() const override;

		static  GreenGEP *create(ArrayRef<GreenExpr*>Operands) { return new GreenGEP(Operands); }

		Value* codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const override;
	};




	class GreenICmp final : public GreenExpr {
	private:
		// TODO: Use a mixin for 
		GreenExpr *Operands [2];

		ICmpInst::Predicate Predicate;
	public:
		GreenICmp(ICmpInst::Predicate Predicate, GreenExpr *LHS, GreenExpr *RHS ) : Operands{LHS,RHS} , Predicate(Predicate) {}

		virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::ICmp; }
		static bool classof(const GreenNode *Node) { return Node->getKind() == LoopHierarchyKind::ICmp; 	}
		static bool classof(const GreenICmp *) {	return true;	}

		GreenExpr * getLHS() const { return Operands[0]; }
		GreenExpr * getRHS() const { return Operands[1]; }

		void printLine(raw_ostream &OS) const override ;

		virtual ArrayRef <const GreenNode * > getChildren() const override;

	


		Value* codegen(IRBuilder<> &Builder, CodegenContext &ActiveRegs )const override;

    static  GreenICmp *create(ICmpInst::Predicate Predicate, GreenExpr *LHS, GreenExpr *RHS) { return new GreenICmp(Predicate,LHS,RHS); }
	};


  class GreenLogicOr final : public GreenExpr {
  private:
    SmallVector<const GreenExpr*,2> Operands;
  protected:
    GreenLogicOr(ArrayRef<const GreenExpr*> Operands): Operands(Operands.begin(), Operands.end()){};
  public:
    virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::LogicOr; }
    static bool classof(const GreenNode *Node) { return Node->getKind() == LoopHierarchyKind::LogicOr; 	}
    static bool classof(const GreenLogicOr *) {	return true;	}

    void printLine(raw_ostream &OS) const override ;

    virtual ArrayRef <const GreenNode * > getChildren() const override;

    Value* codegen(IRBuilder<> &Builder, CodegenContext &CodegenCtx )const override;

    static  GreenLogicOr *create( GreenExpr *LHS, GreenExpr *RHS) { 
      GreenExpr* A[] = {LHS,RHS};
      return new GreenLogicOr(makeArrayRef(A)); 
    }
    static  GreenLogicOr *create(ArrayRef<const GreenExpr*> Operands) { 
      return new GreenLogicOr(Operands);
    }
  };




  class GreenLogicAnd final : public GreenExpr {
  private:
    SmallVector<const GreenExpr*,2> Operands;
  protected:
    GreenLogicAnd(ArrayRef<const GreenExpr*> Operands): Operands(Operands.begin(), Operands.end()){};
  public:
    virtual LoopHierarchyKind getKind() const override {return LoopHierarchyKind::LogicAnd; }
    static bool classof(const GreenNode *Node) { return Node->getKind() == LoopHierarchyKind::LogicAnd; 	}
    static bool classof(const GreenLogicAnd *) {	return true;	}

    void printLine(raw_ostream &OS) const override ;

    virtual ArrayRef <const GreenNode * > getChildren() const override;

    Value* codegen(IRBuilder<> &Builder, CodegenContext &CodegenCtx )const override;

    static  GreenLogicAnd *create( GreenExpr *LHS, GreenExpr *RHS) { 
      GreenExpr* A[] = {LHS,RHS};
      return new GreenLogicAnd(makeArrayRef(A)); 
    }
    static  GreenLogicAnd *create(ArrayRef<const GreenExpr*> Operands) { 
      return new GreenLogicAnd(Operands);
    }
  };



}

#endif /* LLVM_LOF_GREENTREE_H */
