#ifndef LLVM_LOF_GREEN_H
#define LLVM_LOF_GREEN_H

#include "LLVM.h"
#include "LOFUtils.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Format.h"

// TODO: Should be at most included in the .cpp
#include "llvm/IR/Operator.h"
#include "llvm/IR/BasicBlock.h"





namespace lof {
  class Green;
  class GSymbol;
  class GExpr;
  class GOpExpr;
  class GCommon;
  class Dep;
  class LoopContext;

  class Red;
} // namespace lof


namespace llvm {
  template<> struct GraphTraits<lof::Green*>;
  class Loop;
  class Value;
  class Inst;
  class Constant;
  class Operator;
} // namespace llvm


namespace lof {

    class  green_child_iterator :public map_index_iterator< green_child_iterator, GCommon*, GCommon*, std::ptrdiff_t, GCommon**, GCommon* > {
    public:
      green_child_iterator(GCommon* Parent, size_t Idx) : map_index_iterator(Parent, Idx) {}

    public:
      GCommon* operator*() const;
    }; // class green_child_iterator
       
#if 0
    // We define our manual iterator to make it available for GraphTraits<lof:: Green *>::ChildIteratorType
    class  green_child_iterator :public llvm:: iterator_facade_base<green_child_iterator, std::random_access_iterator_tag, GCommon*, ptrdiff_t,GCommon**,GCommon* /* operator* returns RValue */ >
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
#endif
  } // namespace lof


  namespace llvm {

    template <>
    struct GraphTraits<lof::Green*> {
      using GraphRef = lof::Green*;
      using NodeRef = lof::Green*;

      // Green as graph node; enumerate children
      //using  ChildIteratorType =lof:: green_child_iterator;
      using  ChildIteratorType = ArrayRef<NodeRef>::iterator;
      static inline ChildIteratorType child_begin(NodeRef N);
      static inline ChildIteratorType child_end(NodeRef N);

      // Green as graph representing its subtree; enumerate all nodes in subtree
      using  nodes_iterator = df_iterator<NodeRef, df_iterator_default_set<NodeRef>, false, GraphTraits<NodeRef>>;
      static inline nodes_iterator nodes_begin(lof::Green* RI);
      static inline nodes_iterator nodes_end(lof::Green* RI);
      static NodeRef getEntryNode(lof::Green* G) { return G; }
    };
  } // namespace llvm

  namespace lof {
    class Green;
    class GreenBuilder;

    class Operation {
    public:
      enum Kind {
        Unknown,

        // Do an assignment
        Nop, 

        // Logical operations
        False,
        True,
        Negation,
        Conjuction,
        Disjunction,

        // Arithmetic operations
        //Const,
        Add,

        // Should work with normalized loop induction variables.
        AddRecExpr,

        // Affine expression: Requires ISL 
        ISLPwAff,

        // LLVM operations
        LLVMInst,
        LLVMFloating,

        // MLIR operations
        MLIROperation,
        MLIRAff,
        MLIRFlatAff,

        Last = Disjunction
      };

    private:
      Kind K = Unknown;

      // Using this only to avoid duplicating the representation of an LLVM instruction. It's SSA value and operands have no meaning in the green DAG.
      llvm::Value* Inst = nullptr;



      void assertOK() const;


    public:
      Operation() {  }
      Operation(Kind K, llvm::Value* Inst) : K(K), Inst(Inst) { 
        assertOK();
      }

      Kind getKind() const { return K; }
   llvm::   Value* getLLVMInst() const {
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
          if (auto C = dyn_cast<llvm::Constant>(Inst))
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
        case Add:
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
        case Add:
          return 1;
        default:
          llvm_unreachable("Don't know this operation");
        }
        llvm_unreachable("Not implemented kind");
      }

      public:
        void printLine(llvm::raw_ostream& OS) const {
          switch (K)          {
          case lof::Operation::Unknown:
            OS << "???";
            break;
          case lof::Operation::Nop:
            OS << "nop";
            break;
          case lof::Operation::False:
            OS << "false";
            break;
          case lof::Operation::True:
            OS << "true";
            break;
          case lof::Operation::Negation:
            OS << "not";
            break;
          case lof::Operation::Conjuction:
            OS << "and";
            break;
          case lof::Operation::Disjunction:
            OS << "or";
            break;
          case lof::Operation::Add:
            OS << "+";
            break;
          case lof::Operation::LLVMInst:
            if (Inst)
              Inst->print(OS);
            else
              OS << "llvm::Inst";
            break;
          case lof::Operation::LLVMFloating:
            if (Inst) {     
              OS << "(Speculable) ";
              Inst->print(OS);
            }            else
              OS << "llvm::Value";
            break;
          default:
            OS << "Kind=" << K;
          }
        }

    }; // class Operation




     




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
      /// In case we add support to modify after node creation, set staging to false to avoid further modification after the object might also refenced that assumes the object's immutability.
      bool isStaging() const { return false; }

    public:
      virtual size_t getNumChildren() const { return 0; }
      virtual  GCommon* getChild(int i) const { llvm_unreachable("No children"); }
       auto children() {
         return make_range(green_child_iterator(this, 0), green_child_iterator(this, getNumChildren()) );
      }



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

    public:
      Red* asRedRoot() ;


    public:
      virtual void printLine(llvm::raw_ostream& OS) const = 0;

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
      LLVM_DUMP_METHOD void dump() const;
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
      llvm::Value* LLVMName;

      // TODO: Define exactly what this means
      bool IsScalar =true;

      // Number of subscript dimensions
      // For Arrays: Need the size of each dimension
      // For Scalar: Number of loop surrounding the definition of the scalar; can be indexed by `last`, or surrounding loop counter value of the definition we want to use
      int Dims = 0;

    public:
      llvm::  Value* getLLVMValue() const { return LLVMName; }


    private:
      std::string Name;
    public:
      StringRef getName() const { return Name; }
      void printReprName(llvm::raw_ostream &OS) const {
        if (!Name.empty()) {
          OS << Name;
          return;
        }
        OS << llvm::format_hex((uintptr_t)this, 0, true);
      }


    private:
      llvm:: Type* Ty;
    public:
      llvm::  Type* getType() const { return Ty; }

    private:
    void  assertOK() const {
        // Ty == nullptr means arbitrary integer width. To be validated at codegen.
        assert(!Ty || llvm::PointerType::isValidElementType(Ty) );
      }

    private:
      GSymbol(StringRef Name ,llvm:: Value* LLVMName,llvm:: Type *Ty): Name(Name), LLVMName(LLVMName) , Ty(Ty) {
        assertOK();
      }
    public:
      static GSymbol* createLLVM(llvm::Value* LLVMName) { return new GSymbol(LLVMName->getName(), LLVMName, LLVMName->getType() ); }
      static GSymbol* createFromScratch(StringRef  Name, llvm::Type *Ty) { return new GSymbol(Name, nullptr, Ty); }

    public:
      void printLine(llvm::raw_ostream& OS) const override {
        OS << "GRefExpr";
        if (Ty) {
          OS << ' ';
          Ty->print(OS);
        }
        if (!Name.empty())
          OS << ' ' << Name;
      }

    }; // class GSymbol
    using GRefExpr = GSymbol;


    /// Speculable, single result operation
    class GOpExpr : public GExpr {
    private:
      Operation Op;
      SmallVector<GExpr*, 4> Args;

    public:
      const Operation& getOperation() const { return Op; }
      ArrayRef<GExpr*> args() const { return Args; }
       ArrayRef<GExpr*> getArguments() const { return Args; }

    public:
      size_t getNumChildren() const override  { return Args.size(); }
      GCommon* getChild(int i) const override { return Args[i]; }

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

      static GOpExpr* createConstExpr(llvm::Constant *Val) {
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

    //  static GOpExpr* createConst(LoopContext& Ctx, int Val);

    public:
      void printLine(llvm::raw_ostream& OS) const override {
        OS << "GOpExpr";
        Op.printLine(OS);
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
      llvm:: Instruction* OrigBegin = nullptr;
      llvm:: Instruction* OrigEnd   = nullptr;
      llvm:: Loop* OrigLoop = nullptr;
    public:
      std::pair<llvm::Instruction*,llvm:: Instruction*> getOrigRange() const { return { OrigBegin, OrigEnd }; }
      llvm::    Loop* getOrigLoop() const { return OrigLoop; }




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
       ArrayRef<GSymbol*>  getScalarReads() const { return ScalarReads.getValue() .getArrayRef() ; }
       ArrayRef<GSymbol*>  getScalarKills() const { return ScalarKills.getValue() .getArrayRef() ;  }
       ArrayRef<GSymbol*> getScalarWrites() const { return ScalarWrites.getValue() .getArrayRef() ;   }
      bool hasComputedScalars() const {
        return ScalarReads.hasValue() && ScalarKills.hasValue() && ScalarWrites.hasValue();
      }

    private:
      SmallVector<GExpr*, 8 > ChildConds;
      SmallVector<Green*, 8> Children;

    private:
      /// Counter always rises from 0 step 1 with an unsigned integer of undefined width (infinite, but will eventually be truncated).
      GSymbol* CanonicalCounter;
    public:
      GSymbol* getCanonicalCounter() const { return CanonicalCounter; }

    private:
      Green(GExpr *ExecCond, ArrayRef<Green*> Children ,   ArrayRef<GExpr*> Conds ,  bool IsLooping,llvm:: Instruction *OrigBegin,llvm:: Instruction *OrigEnd, Optional< ArrayRef<GSymbol*>> ScalarReads,  Optional< ArrayRef<GSymbol*>> ScalarKills, Optional< ArrayRef<GSymbol*>> ScalarWrites, GSymbol * CanonicalCounter) : 
          ExecCond(ExecCond), Children(Children.begin(), Children.end()), ChildConds(Conds.begin(), Conds.end()),  IsLoop(IsLooping) , OrigBegin(OrigBegin), OrigEnd(OrigEnd) , CanonicalCounter(CanonicalCounter) {
        if (ScalarReads.hasValue()) {
          this->  ScalarReads.emplace( ScalarReads.getValue().begin() , ScalarReads.getValue().end());
        }
        if (ScalarKills.hasValue()) {
          this->  ScalarKills.emplace( ScalarKills.getValue().begin() , ScalarKills.getValue().end());
        }
        if (ScalarWrites.hasValue()) {
          this->  ScalarWrites.emplace( ScalarWrites.getValue().begin() , ScalarWrites.getValue().end());
        }

        assertOK();
      }


    private:
      Operation Op;
      SmallVector <GExpr*, 1> Arguments;
      SmallVector <GSymbol*, 1> Assignments;
     
    public:
      ArrayRef<GExpr*> getArguments() const { return Arguments; }
      ArrayRef<GSymbol*> getAssignments() const { return Assignments; }


    private:
      Green(Operation Op, ArrayRef<GExpr*> Arguments, ArrayRef<GSymbol*> Assignments,llvm::Instruction *OrigBegin,llvm:: Instruction *OrigEnd, llvm::Loop *OrigLoop): Op(Op) , Arguments(Arguments.begin(), Arguments.end()), Assignments(Assignments.begin(), Assignments.end()), OrigBegin(OrigBegin), OrigEnd(OrigEnd), OrigLoop(OrigLoop) {
        assertOK();
      }

    public:
     static  Green* createInstruction(Operation Op,  ArrayRef<GExpr*> Arguments,  ArrayRef<GSymbol*> Assignments,llvm::Instruction *OrigInst) {
       llvm::Instruction* OrigEnd = nullptr;
       if (OrigInst)
         OrigEnd = OrigInst->getNextNode();
        return new Green(Op, Arguments, Assignments,OrigInst,OrigEnd,nullptr);
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
        if (IsLoop) {
          assert(CanonicalCounter);
        }

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


       size_t getNumChildren() const override  { return Children.size(); }
       ArrayRef<Green*> getChildren() const {
         return Children;
       }
       Green* getChild(int i) const { return Children[i]; }
       GExpr* getChildCond(int i) const { return ChildConds[i]; }

     auto children() const {
      // return make_range( green_child_iterator(this, 0), green_child_iterator(this, Children.size())  );
       return getChildren();
      }

      auto descententsDepthFirst() {
        return llvm::depth_first(this);
      }

      auto descententsBreadtFirst() {
        return llvm:: breadth_first(this);
      }

      //LLVM_DUMP_METHOD void dump() const;
     // void print(raw_ostream& OS) const;


    public:
      std::vector<Dep*> getAllDependencies() ;

          public:
            void printLine(llvm::raw_ostream& OS) const override {
              if (isLoop()) {
                OS << "Loop";
              } else if (isInstruction()) {
                OS << "Inst ";
                Op.printLine(OS);
              } else if (isStmt()) {
                OS << "Sequence";
              }              else {
                OS << "???";
              }

              auto PrintSymbolList = [&OS](auto Name, auto X) {
                if (X) {
                  OS << " " << Name << "=[";
                  for (auto A : llvm::enumerate(X.getValue())) {
                    if (A.index() > 0)
                      OS << ", ";
                    A.value()->printReprName(OS);
                  }
                  OS << "]";
                }
              };

              PrintSymbolList("ScalarReads", ScalarReads);
              PrintSymbolList("ScalarKills", ScalarKills);
              PrintSymbolList("ScalarWrites", ScalarWrites);
            }

    }; // class Green







    class GreenDumper {
    private:
      llvm::raw_ostream &OS;
      bool OnlyStmts;

      int Indent = 0;

      void dumpNode(GCommon *Node, StringRef Role) {
        OS.indent(Indent*2);
        if (!Role.empty())
          OS << Role << ": ";
        Node->printLine(OS);
        OS << '\n';
        Indent += 1;
        if (Node->isContainer()) {
          auto G = cast<Green>(Node);
          auto NumChildren = G->getNumChildren();
          for (int i = 0; i < NumChildren; i += 1) {
            auto Cond = G->getChildCond(i);
            auto Child = G->getChild(i);
            if (OnlyStmts ||( isa<GOpExpr>(Cond) && cast<GOpExpr>(Cond)->getOperation().getKind() == Operation::True)) {
              dumpNode(Child, StringRef());
            } else {
              dumpNode(Cond, "Cond");
              Indent += 1;
              dumpNode(Child, StringRef());
              Indent -= 1;
            }
          }
        } else if (Node->isInstruction()) {
          if (!OnlyStmts) {
            auto G = cast<Green>(Node);
            for (auto Dst : G->getAssignments()) {
              dumpNode(Dst, "Assign");
            }
            for (auto Arg : G->getArguments()) {
              dumpNode(Arg, "Arg");
            }
          }
        } else if (isa<GOpExpr>(Node)) {
          if (!OnlyStmts) {
            auto Expr = cast<GOpExpr>(Node);
            for (auto Arg : Expr->getArguments()) {
              dumpNode(Arg, "Arg");
            }
          }
        }
        
        for (auto Child : Node->children()) {
          if (OnlyStmts && !Child->isStmt())
            continue;
          dumpNode(Child, StringRef());
        }
        Indent -= 1;
      }

    public:
      GreenDumper(llvm::raw_ostream& OS, bool OnlyStmts =false) : OS(OS) , OnlyStmts(OnlyStmts) {}

      void dump(GCommon *Node) {
        dumpNode(Node, StringRef());
      }

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


namespace llvm {
  GraphTraits<lof:: Green *>::ChildIteratorType GraphTraits<lof:: Green *>::child_begin(NodeRef N) { return N->children ().begin(); }
  GraphTraits<lof:: Green *>::ChildIteratorType GraphTraits<lof:: Green *>::child_end(NodeRef N)   { return N->children ().end(); }

  GraphTraits<lof:: Green *>:: nodes_iterator GraphTraits<lof:: Green *>:: nodes_begin(lof:: Green* RI) { return nodes_iterator::begin(getEntryNode(RI));  }
  GraphTraits<lof:: Green *>:: nodes_iterator GraphTraits<lof:: Green *>:: nodes_end  (lof:: Green *RI) { return nodes_iterator::end(getEntryNode(RI));    }
} // namespace llvm




#endif /* LLVM_LOF_GREEN_H */
