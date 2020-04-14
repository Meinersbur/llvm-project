#ifndef LLVM_LOF_GREEN_H
#define LLVM_LOF_GREEN_H

#include "llvm/IR/Instruction.h"
#include "llvm/IR/Operator.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/SetVector.h"
#include "LOFUtils.h"


namespace llvm {
  namespace lof {
    class Green;
    class GSymbol;
    class GExpr;
    class GOpExpr;
    class GCommon;
  }
  template<>
  struct GraphTraits<lof::Green*>;



  namespace lof {

    // We define our manual iterator to make it available for GraphTraits<lof:: Green *>::ChildIteratorType
    class  green_child_iterator :public iterator_facade_base<green_child_iterator, std::random_access_iterator_tag, Green*>
    {
      const GCommon* Parent;
      size_t Idx;
    public:
      green_child_iterator() = default;
      green_child_iterator(const GCommon* Parent, size_t Idx) : Parent(Parent), Idx(Idx) {}
      green_child_iterator(const green_child_iterator& That) : Parent(That.Parent), Idx(That.Idx) {}

      green_child_iterator& operator=(const green_child_iterator& That) {
        Parent = That.Parent;
        Idx = That.Idx;
        return *this;
      }

      GCommon* operator*() const;

      bool operator==(const green_child_iterator& That) const {
        return Parent == That.Parent && Idx == That.Idx;
      }

      green_child_iterator& operator+=(std::ptrdiff_t N) {
        Idx += N;
        return *this;
      }

      green_child_iterator& operator-=(std::ptrdiff_t N) {
        Idx -= N;
        return *this;
      }

      std::ptrdiff_t operator-(const green_child_iterator& That) const {
        return Idx - That.Idx;
      }

      bool operator<(const green_child_iterator& That) const {
        return Idx < That.Idx;
      }
    };

  } // namespace lof



  template <> 
  struct GraphTraits<lof:: Green *> {
    using GraphRef =lof::Green*;
    using NodeRef = lof::Green*;

    // Green as graph node; enumerate children
    //using  ChildIteratorType =lof:: green_child_iterator;
    using  ChildIteratorType = ArrayRef<NodeRef>::iterator;
    static inline ChildIteratorType child_begin(NodeRef N);
    static inline ChildIteratorType child_end(NodeRef N);

    // Green as graph representing its subtree; enumerate all nodes in subtree
    using  nodes_iterator = df_iterator<NodeRef, df_iterator_default_set<NodeRef>, false,  GraphTraits<NodeRef>>;    
    static inline nodes_iterator nodes_begin(lof::Green* RI);
    static inline nodes_iterator nodes_end(lof::Green* RI);
    static NodeRef getEntryNode(lof::Green *G) { return G; }
  };


  namespace lof {
    class Green;
    class GreenBuilder;

    class Operation {
    public:
      enum Kind {
        Unknown,

        Nop, // Effectively only does an assignment

        LLVMInst,
        LLVMFloating,

        False,
        True,
        Negation,
        Conjuction,
        Disjunction,

        // Should work with normalized loop induction variables.
        AddRecExpr,

        Last = Disjunction
      };

    private:
      Kind K = Unknown;

      // Using this only to avoid duplicating the representation of an LLVM instruction. It's SSA value and operands have no meaning in the green DAG.
      llvm::Value* Inst = nullptr;



      void assertOK() const {
        switch (K) {
        case Operation::Unknown:
          assert(Inst==nullptr);
          return;
        case Operation::Nop:
          assert(Inst==nullptr); // There is no "nop" LLVM instruction
          break;
        case Operation::LLVMFloating:
        case Operation::LLVMInst:
          assert(Inst);
          assert(PointerType::isValidElementType( Inst->getType()) && "Must be allocatable");
          assert(!isa<PHINode>(Inst )&& "PHIs are not regular operation");
          if (auto I = dyn_cast <Instruction>(Inst)) {
            assert( !cast<Instruction>(I)->isTerminator() && "Operations don't cover control-flow");
          }
          break;
        case Operation::True:
          if (Inst) {
            auto C = cast<ConstantInt>(Inst);
            assert(C->getType()->isIntegerTy(1));
            assert(C->isOne());
          }
            break;
        case Operation::False:
          if (Inst) {
            auto C = cast<ConstantInt>(Inst);
            assert(C->getType()->isIntegerTy(1));
            assert(C->isZero());
          }
          break;
        case Operation::Negation:
          assert(!Inst);
          break;
        case Operation::Conjuction:
          if (Inst) {
            auto O = cast<BinaryOperator >(Inst);
            assert(O->getType()->isIntegerTy(1));
            assert(O->getOpcode() == Instruction::And );
          }
          break;
        case Operation::Disjunction:
          if (Inst) {
            auto O = cast<BinaryOperator >(Inst);
            assert(O->getType()->isIntegerTy(1));
            assert(O->getOpcode() == Instruction::Or );
          }
          break;
          default:
            llvm_unreachable("Not supported kind?!?");
        }
      }


    public:
      Operation() {  }
      Operation(Kind K, llvm::Value* Inst) : K(K), Inst(Inst) { 
        assertOK();
      }

      Kind getKind() const { return K; }
      Value* getLLVMInst() const {
        assert(K == LLVMInst || K==LLVMFloating);
        return Inst;
      }

      bool isValid() const {
        return K != Unknown;
      }

      bool isSpeculable() const {
        assert(isValid());
        return K != LLVMInst;
      }

      int getNumInputs() const {
        switch (K)        {
        case Nop:
          return 1;
        case LLVMInst:
        case LLVMFloating:
          if (auto C = dyn_cast<Constant>(Inst))
            return 0;
          return cast<llvm::Operator >(Inst)->getNumOperands();
        case False:
        case True:
          return 0;
        case Negation:
          return 1;
        case Conjuction:
        case Disjunction:
          return 2;
        default:
          llvm_unreachable("Don't know this operation");
        }
        llvm_unreachable("Not implemented kind");
      }

      int getNumOutputs() const {
        switch (K)        {
        case Nop:
          return 1;
        case LLVMInst:
        case LLVMFloating:
          if (Inst->getType()->isVoidTy())
            return 0;
          return 1;
        case False:
        case True:
        case Negation:
        case Conjuction:
        case Disjunction:
          return 1;
        default:
          llvm_unreachable("Don't know this operation");
        }
        llvm_unreachable("Not implemented kind");
      }

    }; // class Operation




     


    class Dep {
    private:
    public:
      virtual ~Dep() {}

      virtual bool isScalar() const  = 0;
    };



    class GCommon {
    public:
      virtual ~GCommon() {  }

    public:
      enum Kind { Unknown, RefExpr, OpExpr, Stmt };
      virtual Kind getKind() const = 0;
      static bool classof(const GCommon *) { return true; }

    public:
      bool isExpr() const { return isa<GExpr>(this); }
      bool isStmt() const { return getKind() == Stmt; }
      virtual bool isInstruction() const { return false; }
      virtual bool isContainer() const { return false;  }
      virtual bool isLoop() const { return false;  }

    public:
      // TODO: whether something is read, killed or written depends on conditions; Current idea: DenseMap to conditions/GreenNode when this happens 
      // TODO: Use SetVector
      void determineScalars(DenseSet<GSymbol*>& Reads, DenseSet<GSymbol*>& Kills, DenseSet<GSymbol*>& Writes,  DenseSet<GSymbol*>& AllReferences );
      
#if 0
      DenseSet<GSymbol*> reads() {
        DenseSet<GSymbol*> Reads;
        DenseSet<GSymbol*> Kills;
        DenseSet<GSymbol*> Writes;
        determineScalars(Reads, Kills,Writes );
        return Reads;
      }
      DenseSet<GSymbol*> kills() {}
      DenseSet<GSymbol*> writes() {}
#endif
    };



    class GExpr : public GCommon {
    public:
      static bool classof(const GCommon* O) { auto K = O->getKind(); return (K == Kind::RefExpr) || (K == Kind::OpExpr); }

    public:
          virtual ~GExpr() {  }

         // virtual bool isRef() const { return false;  }
         // virtual bool isOp() const { return false;  }

       static GSymbol* createRef(GSymbol* Sym) { assert(Sym); return Sym; }
       static GOpExpr* createOp(Operation Op, ArrayRef<GExpr*> Args);
    };

    class GSymbol : public GExpr {
    public:
      Kind getKind() const override { return Kind::RefExpr; }
      static bool classof(const GCommon* O) { return O->getKind() == GExpr::RefExpr; }

    private:
      Value* LLVMName;

      // TODO: Define exactly what this means
      bool IsScalar =true;

      // Number of subscript dimensions
      // For Arrays: Need the size of each dimension
      // For Scalar: Number of loop surrounding the definition of the scalar; can be indexed by `last`, or surrounding loop counter value of the definition we want to use
      int Dims = 0;

    public:
      Value* getLLVMValue() const { return LLVMName; }


    private:
      std::string Name;
    public:
      StringRef getName() const { return Name; }

    private:
      Type* Ty;
    public:
      Type* getType() const { return Ty; }


    private:
      GSymbol(StringRef Name , Value* LLVMName, Type *Ty): Name(Name), LLVMName(LLVMName) , Ty(Ty) {
        assert(Ty);
        assert(PointerType::isValidElementType(Ty) );
      }
    public:
      static GSymbol* createLLVM(Value* LLVMName) { return new GSymbol(LLVMName->getName(), LLVMName, LLVMName->getType() ); }
      static GSymbol* createFromScratch(StringRef  Name, Type *Ty) { return new GSymbol(Name, nullptr, Ty); }
    };
    using GRefExpr = GSymbol;


    /// Speculable, single result operation
    class GOpExpr : public GExpr {
    private:
      Operation Op;
      SmallVector<GExpr*, 4> Args;

    public:
      const Operation& getOperation() const { return Op; }
      const auto &args() const { return Args; }
      const auto &getArguments() const { return Args; }

    private:
      GOpExpr(Operation Op, ArrayRef<GExpr*> Args) : Op(Op), Args(Args.begin(), Args.end()) {}
    public:
      static GOpExpr* create(Operation Op, ArrayRef<GExpr*> Args) {
        assert(Op.isSpeculable());
        assert(Op.getNumInputs() ==Args.size());
        return new GOpExpr(Op, Args);
      }

    public:
       Kind getKind() const override { return GExpr::OpExpr; }
      static bool classof(const GCommon* O) { return O->getKind() == GExpr::OpExpr; }


    public:
      // TODO: It's a singleton, or more general: uniqueing
      // TODO: move GExpr, how it's implemented doesn't matter
      static GOpExpr* createTrueExpr() {
        return createOp (Operation(Operation::True, nullptr), {});
      }
      static GOpExpr* createFalseExpr() {
        return createOp(Operation(Operation::False, nullptr), {});
      }

      static GOpExpr* createConstExpr(Constant *Val) {
        return createOp(Operation(Operation::LLVMFloating, Val), {});
      }

      static GOpExpr* createNotExpr(GExpr* Subexpr) {
        return createOp(Operation(Operation::Negation, nullptr), {Subexpr});
      }

      // TOOD: List of operands
      static GOpExpr* createConjunctionExpr(GExpr* LHS, GExpr* RHS) {
        return createOp(Operation(Operation::Conjuction, nullptr), {LHS,RHS});
      }
      static GOpExpr* createDisjunctionExpr(GExpr* LHS, GExpr* RHS) {
        return createOp(Operation(Operation::Disjunction, nullptr), {LHS,RHS});
      }
    };












    // TODO: rename Green to GStmt; rename GCommon to Green
    class Green : public GCommon {
    public:
      friend class GreenBuilder;
      friend class green_child_iterator;

    private:
      //bool Staging = true;
      //bool IsFloating = false;


  



      // TOOD: Generalize marking regions in the original IR.
    private:
      Instruction* OrigBegin = nullptr;
      Instruction* OrigEnd   = nullptr;
      Loop* OrigLoop = nullptr;
    public:
      std::pair<Instruction*, Instruction*> getOrigRange() const { return { OrigBegin, OrigEnd }; }
      Loop* getOrigLoop() const { return OrigLoop; }




    private:
      /// Loop: Condition for executing first / another iterations 
      /// Stmt (Instruction or Sequence): Not yet used, but might also be used as execution condition (in addition to the parent's ChildConds)
      GExpr* ExecCond=nullptr;
    public:
      GExpr* getExecCond() const {
        return ExecCond;
      }




    private:
      Optional< SmallSetVector<GSymbol*, 4> > ScalarReads;
      Optional< SmallSetVector<GSymbol*, 4>> ScalarKills;
      Optional< SmallSetVector<GSymbol*, 4>> ScalarWrites;
    public:
      const auto& getScalarReads() const { return ScalarReads.getValue(); }
      const auto& getScalarKills() const { return ScalarKills.getValue() ;  }
      const auto& getScalarWrites() const { return ScalarWrites.getValue() ;   }
      bool hasComputedScalars() const {
        return ScalarReads.hasValue() && ScalarKills.hasValue() && ScalarWrites.hasValue();
      }

    private:
      SmallVector<GExpr*, 8 > ChildConds;
      SmallVector<Green*, 8> Children;


    private:
      Green(GExpr *ExecCond, ArrayRef<Green*> Children ,   ArrayRef<GExpr*> Conds ,  bool IsLooping, Instruction *OrigBegin, Instruction *OrigEnd, Optional< ArrayRef<GSymbol*>> ScalarReads,  Optional< ArrayRef<GSymbol*>> ScalarKills, Optional< ArrayRef<GSymbol*>> ScalarWrites) : ExecCond(ExecCond), Children(Children.begin(), Children.end()), ChildConds(Conds.begin(), Conds.end()),  IsLoop(IsLooping) , OrigBegin(OrigBegin), OrigEnd(OrigEnd) {
        if (ScalarReads.hasValue()) {
          this->  ScalarReads.emplace( ScalarReads.getValue().begin() , ScalarReads.getValue().end());
        }
        if (ScalarKills.hasValue()) {
          this->  ScalarKills.emplace( ScalarKills.getValue().begin() , ScalarKills.getValue().end());
        }
        if (ScalarWrites.hasValue()) {
          this->  ScalarWrites.emplace( ScalarWrites.getValue().begin() , ScalarWrites.getValue().end());
        }
      }


    private:
      Operation Op;
      SmallVector <GExpr*, 1> Arguments;
      SmallVector <GSymbol*, 1> Assignments;
     
    public:
      auto getArguments() const { return Arguments; }
      auto getAssignments() const { return Assignments; }


    private:
      Green(Operation Op, ArrayRef<GExpr*> Arguments, ArrayRef<GSymbol*> Assignments,Instruction *OrigBegin, Instruction *OrigEnd, Loop *OrigLoop): Op(Op) , Arguments(Arguments.begin(), Arguments.end()), Assignments(Assignments.begin(), Assignments.end()), OrigBegin(OrigBegin), OrigEnd(OrigEnd), OrigLoop(OrigLoop) {
        assertOK();
      }
    public:
     static  Green* createInstruction(Operation Op,  ArrayRef<GExpr*> Arguments,  ArrayRef<GSymbol*> Assignments,Instruction *OrigInst) {
        return new Green(Op, Arguments, Assignments,OrigInst,OrigInst->getNextNode(),nullptr);
      }
      bool isInstruction() const override  { return Op.isValid(); }





  

   
    public:
      Operation& getOperation() {
        return Op;
      }


    public:
      Kind getKind() const { return GCommon::Stmt ; }
      static bool classof(const GCommon* O) { return O->getKind() == GCommon::Stmt; }


      void assertOK() const {
#if 0
        if (Staging)
          return;

        if (isOperation() && isStmt() && !isExpr() && !isLoop()) {
          // A non-floating operation
          int a = 0;
        } else if (isOperation() && !isStmt() && isExpr() && !isLoop()) {
          // A floating operation
          int a = 0;
        } else if (!isOperation() && isStmt() && !isExpr() && !isLoop()) {
          // A non-floating sequence of sub-stmts
          int a = 0;
        } else if (!isOperation() && !isStmt() && isExpr() && !isLoop()) {
          // A floating sequence of sub-stmts
          int a = 0;
        } else if (!isOperation() && isStmt() && isExpr() && isLoop()) {
          // A non-floating loop of sub-stmts
          int a = 0;
        }  else {
          // Currently not allowed:
          // * floating loops
          llvm_unreachable("Inconsistent kind of green node");
        }

        if (isOperation()) {
          assert(Op.isValid());
          //assert(NumInputs == Op.getNumInputs());
          //assert(NumIntermediate==0 );
          //assert(NumOutputs == Op.getNumOutputs());
          assert(Children.empty());
        } else {
          assert(Op.getKind() == Operation::Unknown);
        }
#endif
      }


      /// A loop is a stmt that executes its children multiple times
    private:
      bool IsLoop = false;
public:
      bool isLoop() const override { return IsLoop; }
      bool isContainer() const override { return ! Op.isValid(); }



     auto children() const {
      // return make_range( green_child_iterator(this, 0), green_child_iterator(this, Children.size())  );
       return ArrayRef<Green*>(Children.data(), Children.size()  );
      }

      auto descententsDepthFirst() {
        return depth_first(this);
      }

      auto descententsBreadtFirst() {
        return breadth_first(this);
      }

      LLVM_DUMP_METHOD void dump() const;
      void print(raw_ostream& OS) const;
    };





  } // namespace lof


#if 0
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
#endif



  GraphTraits<lof:: Green *>::ChildIteratorType GraphTraits<lof:: Green *>::child_begin(NodeRef N) { return N->children ().begin(); }
  GraphTraits<lof:: Green *>::ChildIteratorType GraphTraits<lof:: Green *>::child_end(NodeRef N)   { return N->children ().end(); }


  GraphTraits<lof:: Green *>:: nodes_iterator GraphTraits<lof:: Green *>:: nodes_begin(lof:: Green* RI) { return nodes_iterator::begin(getEntryNode(RI));  }
  GraphTraits<lof:: Green *>:: nodes_iterator GraphTraits<lof:: Green *>:: nodes_end  (lof:: Green *RI) { return nodes_iterator::end(getEntryNode(RI));    }

} // namespace llvm
#endif /* LLVM_LOF_GREEN_H */
