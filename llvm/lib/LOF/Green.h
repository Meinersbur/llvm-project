#ifndef LLVM_LOF_GREEN_H
#define LLVM_LOF_GREEN_H

#include "llvm/IR/Instruction.h"
#include "llvm/IR/Operator.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/iterator.h"


namespace llvm {
  namespace lof {
    class Green;
  }
  template<>
  struct GraphTraits<lof::Green*>;



  namespace lof {
    // We define our manual iterator to make it available for GraphTraits<lof:: Green *>::ChildIteratorType
    class  green_child_iterator :public iterator_facade_base<green_child_iterator, std::random_access_iterator_tag, Green*>
    {
      const Green* Parent;
      size_t Idx;
    public:
      green_child_iterator() = default;
      green_child_iterator( const Green* Parent, size_t Idx) : Parent(Parent), Idx(Idx) {}
      green_child_iterator(const green_child_iterator& That) : Parent(That.Parent), Idx(That.Idx) {}

      green_child_iterator& operator=(const green_child_iterator& That) {
        Parent = That.Parent;
        Idx = That.Idx;
        return *this;
      }

      Green* operator*() const;

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
    using NodeRef = lof::Green*;

    // Green as graph node; enumerate children
    using  ChildIteratorType =lof:: green_child_iterator;
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
        Nop,
        LLVMInst,
        LLVMFloating,
        False,
        True,
        Negation,
        Conjuction,
        Disjunction,

        Last = Disjunction
      };

    private:
      Kind K = Unknown;

      // Using this only to avoid duplicating the representation of an LLVM instruction. It's SSA value and operands have no meaning in the green DAG.
      llvm::Value* Inst = nullptr;
    public:
      Operation() {  }
      Operation(Kind K, llvm::Value* Inst) : K(K), Inst(Inst) {  }

      Kind getKind() const { return K; }

      bool isValid() const {
        return K != Unknown;
      }
      bool isFloating() const {
        assert(isValid());
        return K != LLVMInst;

      }

      int getNumInputs() const {
        switch (K)        {
        case Nop:
          return 1;
        case LLVMInst:
        case LLVMFloating:
          return cast<llvm::Operator >(Inst)->getNumOperands();
        case False:
        case True:
          return 0;
        case Negation:
          return 1;
        case Conjuction:
        case Disjunction:
          return 2;
        case Unknown:
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
        case Unknown:
          llvm_unreachable("Don't know this operation");
        }
        llvm_unreachable("Not implemented kind");
      }

    };




     


    class Dep {
    private:
    public:
      virtual ~Dep() {}

      virtual bool isScalar() const  = 0;
    };


    class GUse;

    class GVal {
      friend class GreenBuilder;

      enum Kind {
        Unknown,

        // Arguments are the input slots that can be used as values by the children inside a green node
        Argument,

        // Outputs are definitions of values by children to be used inside the green node.
        Output,

        // Expressions (e.g. constants) are not added as children of a green node; their time of evaluation do not matter as long a their scalar dependencies are fulfilled.
        // TODO: not used atm.
        Expr,
      };

    private:
      Green* Parent;
      Kind K;
      GUse* FirstUse=nullptr;

      // Argument kind
      int ArgumentIdx=-1;
      Value* LLVMVal=nullptr;

      // Output Kind
      int ChildIdx=-1;
      int OutputIdx=-1;

      GVal(Green* Parent, int ArgumentIdx, Value* LLVMVal) : Parent(Parent), K(Argument) , ArgumentIdx(ArgumentIdx), LLVMVal(LLVMVal) {}
      GVal(Green* Parent, int ChildIdx,   int OutputIdx ) : Parent(Parent), K(Output) , LLVMVal(LLVMVal) {}
    public:
     static  GVal* createArgument(Green* Parent, int ArgumentIdx, Value* LLVMVal ) {
        return new GVal(Parent, ArgumentIdx, LLVMVal);
      }

    static  GVal* createOutput(Green* Parent,int ChildIdx,   int OutputIdx  ) {
        return new GVal(Parent, ChildIdx, OutputIdx);
      }

    Value* getLLVMVal() const {
      assert(K==Argument);
      return LLVMVal;
    }

    };

    class GUse : Dep {
      friend class GreenBuilder;

      enum Kind {
        Unknown,
        Result,
        Input,
      };

    private:
      GVal* Def;
      Kind K;
      GUse* NextUse=nullptr;

      // Result kind
      int ResultIdx=-1;
      Instruction* LLVMVal=nullptr;

      // Input kind
      int ChildIdx=-1;
      int InputIdx=-1;

      GUse(GVal* Def, int ResultIdx, Instruction *LLVMVal) : Def(Def), K(Result), ResultIdx(ResultIdx), LLVMVal(LLVMVal) {}
      GUse(GVal* Def, int ChildIdx,int InputIdx) : Def(Def), K(Result), ChildIdx(ChildIdx), InputIdx(InputIdx) {}
    public:
     static  GUse* createResult(GVal* Def , int ResultIdx, Instruction *LLVMVal) {
        return new GUse(Def, ResultIdx, LLVMVal);
      }

    static   GUse* createInput(GVal* Def , int ChildIdx,  int InputIdx ) {
        return new GUse(Def, ChildIdx, InputIdx);
      }


    GVal* getDef() const {
      return Def;
    }

    Instruction* getLLVMVal() const {
      assert(K==Result);
      return LLVMVal;
    }

      bool isScalar() const override { return true; }
    };




    class Green {
    public:
      friend class GreenBuilder;
      friend class green_child_iterator;

    private:
      bool Staging = true;
      bool IsFloating = false;
      bool IsLooping = false;

    private:
      struct ChildMeaning {
        friend class green_child_iterator;
      
        Green* Child;
        int ChildIdx;

        SmallVector<GUse*, 4> InputMeaning;
        SmallVector<GVal*, 2> OutputMeaning;

    
        ChildMeaning(Green *Child, int ChildIdx, ArrayRef<GUse*> InputMeaning, ArrayRef<GVal*> OutputMeaning): 
          Child(Child), ChildIdx(ChildIdx), InputMeaning(InputMeaning.begin(), InputMeaning.end()), OutputMeaning(OutputMeaning.begin(), OutputMeaning.end()) {
        }

        Green* getChild() const { return Child; }
      };
      SmallVector<ChildMeaning, 8> Children;



    private:
      Operation Op;
     

      // DontOptimize, DontCanonicalize


      // Execution Predicate (for Stmt,Loop)
      Green* ExecPred = nullptr;

      // Re-execution condition (for Loop)
      Green* BackedgePred = nullptr;


      SmallVector<GVal*, 8> Arguments;
      SmallVector<GUse*, 8> Results;


      //SmallVector<Green*, 8> Children;


      int NumInputs;
      //SmallVector<Dep*, 8> InputConsumers;

      int NumIntermediate = 0; // Don't need?

      int NumOutputs;
      //SmallVector<Dep*, 8> OutputProducers;


      // An operation
      Green(Operation Op) : 
        Op(Op), NumInputs(Op.getNumInputs()), NumOutputs(Op.getNumOutputs()), IsFloating(Op.isFloating())
      { 
        validate();
      }

      // A sequence of stmts
      Green() { 
      }

      static Green* createStaged() {
        auto Result = new Green();
        Result->Staging = true;
        return Result;
      }


    public:
      Operation& getOperation() {
        return Op;
      }

      // TODO: It's a singleton
      static Green* createTrueExpr() {
        return new Green(Operation(Operation::True, nullptr));
      }
      static Green* createFalseExpr() {
        return new Green(Operation(Operation::False, nullptr));
      }

      static Green* createConstExpr(Constant *Val) {
        return new Green(Operation(Operation::LLVMFloating,  Val));
      }




#if 0

      static Green* createExpr(Operation Op, ArrayRef<Green*> Ops) {
        assert(Op.isFloating());
        return new Green(Op, Ops);
      }
#endif
      void validate() const {
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
          assert(NumInputs == Op.getNumInputs());
          assert(NumIntermediate==0 );
          assert(NumOutputs == Op.getNumOutputs());
          assert(Children.empty());
        } else {
          assert(Op.getKind() == Operation::Unknown);
        }
      }

      static Green* createOperation(Operation Op) {
        return new Green(Op);
      }


    //  static Green* createLoop(int NumInputs, ArrayRef<Dep*> InputConsumers, int NumIntermediate, int NumOutputs, ArrayRef<GreenMeaning> Children);
    //  static Green* createFunc(int NumInputs, ArrayRef<Dep*> InputConsumers, int NumIntermediate, int NumOutputs, ArrayRef<GreenMeaning> Children);

#if 0
      static Green* createSequence(ArrayRef <Green*> Insts) {
        return new Green(Operation(), Insts);
      }
#endif

      /// Operation is a leaf in the LOF.
      bool isOperation() const { return Op.isValid(); }

      /// An expression is not anyone's child. It cannot have side-effects and must be speculable.
      bool isExpr() const { return IsFloating; }

      /// A statement needs to the root or someone's child
      bool isStmt() const { return !IsFloating ; }

      /// A loop is a stmt that executes its children multiple times
      bool isLoop() const { return IsLooping; }
      //bool isContainer() const { return IsContainer; }


      int getNumInputs() const {
        return NumInputs;
      }
      int getNumOutputs() const {
        return NumOutputs;
      }


      GVal* getArgument(int ArgIdx) const {
        return Arguments [0];
      }

     auto children() const {
       return make_range( green_child_iterator(this, 0), green_child_iterator(this, Children.size())  );
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


    Green* green_child_iterator ::operator*() const {
      return Parent->Children[Idx].Child;
    }


  } // namespace lof






  GraphTraits<lof:: Green *>::ChildIteratorType GraphTraits<lof:: Green *>::child_begin(NodeRef N) { return N->children ().begin(); }
  GraphTraits<lof:: Green *>::ChildIteratorType GraphTraits<lof:: Green *>::child_end(NodeRef N)   { return N->children ().end(); }


  GraphTraits<lof:: Green *>:: nodes_iterator GraphTraits<lof:: Green *>:: nodes_begin(lof:: Green* RI) { return nodes_iterator::begin(getEntryNode(RI));  }
  GraphTraits<lof:: Green *>:: nodes_iterator GraphTraits<lof:: Green *>:: nodes_end  (lof:: Green *RI) { return nodes_iterator::end(getEntryNode(RI));    }

} // namespace llvm
#endif /* LLVM_LOF_GREEN_H */
