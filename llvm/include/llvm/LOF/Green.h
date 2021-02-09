#ifndef LLVM_LOF_GREEN_H
#define LLVM_LOF_GREEN_H

#include "LOFUtils.h"
#include "llvm/ADT/BreadthFirstIterator.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/iterator.h"
#include "llvm/LOF/LLVM.h"
#include "llvm/Support/Format.h"

// TODO: Should be at most included in the .cpp
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Operator.h"

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
template <> struct GraphTraits<lof::GCommon *>;
class Loop;
class Value;
class Inst;
class Constant;
class Operator;
} // namespace llvm

namespace lof {
class green_child_iterator
    : public map_index_iterator<green_child_iterator, GCommon *, GCommon *,
                                std::ptrdiff_t, GCommon **, GCommon *> {
public:
  green_child_iterator(GCommon *Parent, size_t Idx)
      : map_index_iterator(Parent, Idx) {}

public:
  GCommon *operator*() const;
}; // class green_child_iterator
} // namespace lof

namespace llvm {
template <> struct GraphTraits<lof::GCommon *> {
  // Green as graph node; enumerate children
  using NodeRef = lof::GCommon *;
  using ChildIteratorType = lof::green_child_iterator;
  static inline ChildIteratorType child_begin(NodeRef N);
  static inline ChildIteratorType child_end(NodeRef N);

  // Green as graph representing its subtree; enumerate all nodes in subtree
  using GraphRef = lof::GCommon *;
  using nodes_iterator = df_iterator<NodeRef, df_iterator_default_set<NodeRef>,
                                     false, GraphTraits<NodeRef>>;
  static inline nodes_iterator nodes_begin(lof::GCommon *G);
  static inline nodes_iterator nodes_end(lof::GCommon *G);
  static NodeRef getEntryNode(lof::GCommon *G) { return G; }
}; // template specialization GraphTraits<lof::GCommon*>
} // namespace llvm

namespace lof {
class Green;
class GreenBuilder;

void determineScalars(GCommon *Node, DenseSet<GSymbol *> &Reads,
                      DenseSet<GSymbol *> &Kills, DenseSet<GSymbol *> &Writes,
                      DenseSet<GSymbol *> &AllReferences);

class Operation {
public:
  enum Kind {
    Unknown,

    // Do an assignment
    Nop,

    // Reference to scalar variable (to replace GRefExpr)
    ScalarRef,

    // Assign first value for this the preceding condition is true
    // Args are: [condition1, value1, condition2, value2, ..., fallback-value]
    Select,

#if 0
        // Loop meta-instructions
        LoopCounter,
        LoopIsFirst,
#endif

    // Logical operations
    False,
    True,
    Negation,
    Conjuction,
    Disjunction,

    // Arithmetic operations
    // Const,
    Add,

    // Should work with normalized loop induction variables.
    AddRecExpr,

    // Array accesses
    // TODO: should define a byte array range (range at each dimension? range
    // subexpr?)
    LoadArrayElt,
    StoreArrayElt,

    // Reduction operations
    ReduceReturn,
    ReduceLast,
    ReduceAdd,

    // LLVM operations
    LLVMInst,
    LLVMSpeculable,

    // MLIR operations
    MLIROperation,
    MLIRAff,
    MLIRFlatAff,

    // Affine expression: Requires ISL
    ISLPwAff,

    Last = ISLPwAff
  };

private:
  Kind K = Unknown;

  /// Using this only to avoid duplicating the representation of an LLVM
  /// instruction. It's SSA value and operands have no meaning in the green DAG.
  llvm::Value *Inst = nullptr;

  /// Number of arguments for variable-argument instructions that are not an
  /// llvm::Value.
  int NArgs = -1;

  void assertOK() const;

public:
  Operation() {}
  Operation(Kind K, llvm::Value *Inst, int NArgs = -1)
      : K(K), Inst(Inst), NArgs(NArgs) {
    assertOK();
  }

  Kind getKind() const { return K; }
  llvm::Value *getLLVMInst() const {
    assert(K == LLVMInst || K == LLVMSpeculable);
    return Inst;
  }

  bool isValid() const { return K != Unknown; }

  bool isSpeculable() const {
    assert(isValid());
    return K != LLVMInst;
  }

  bool isReduction() const {
    switch (K) {
    case lof::Operation::ReduceReturn:
    case lof::Operation::ReduceLast:
    case lof::Operation::ReduceAdd:
      return true;
    default:
      return false;
    }
  }

  /// Used to detect reductions; currently only 'add' is supported
  bool isAssociative() const;

  int getNumInputs() const {
    switch (K) {
    case Nop:
      return 1;
    case Select:
      return NArgs;
#if 0
        case LoopCounter:
        case LoopIsFirst:
          return 0;
#endif
    case ReduceReturn:
    case ReduceLast:
    case ReduceAdd:
      return 1;

    case LLVMInst:
    case LLVMSpeculable:
      if (auto C = dyn_cast<llvm::Constant>(Inst))
        return 0;
      return cast<llvm::Operator>(Inst)->getNumOperands();
    case Operation::LoadArrayElt:
      return 2;
    case Operation::StoreArrayElt:
      return 3;
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
    switch (K) {
    case Nop:
      return 1;
    case Select:
      return 1;
#if 0
        case LoopCounter:
        case LoopIsFirst:
          return 1;
#endif
    case ReduceReturn:
      return 1;
    case ReduceLast:
    case ReduceAdd:
      return 1;

    case LLVMInst:
    case LLVMSpeculable:
      if (Inst->getType()->isVoidTy())
        return 0;
      return 1;
    case Operation::LoadArrayElt:
      return 1;
    case Operation::StoreArrayElt:
      return 0;
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
  void printLine(llvm::raw_ostream &OS) const {
    switch (K) {
    case lof::Operation::Unknown:
      OS << "???";
      break;
    case Select:
      OS << "select";
      break;
#if 0
          case LoopCounter:
            OS << "iter";
            break;
          case LoopIsFirst:
            OS << "isfirst";
            break;
#endif
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
    case lof::Operation::LLVMSpeculable:
      if (Inst) {
        OS << "(Speculable) ";
        Inst->print(OS);
      } else
        OS << "llvm::Value";
      break;
    case lof::Operation::LoadArrayElt:
      OS << "load[]";
      break;
    case lof::Operation::StoreArrayElt:
      OS << "store[]";
      break;
    default:
      OS << "Kind=" << K;
    }
  }

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  LLVM_DUMP_METHOD void dump() const;
#endif
}; // class Operation

class GCommon {
public:
  virtual ~GCommon() {}

public:
  enum Kind { Unknown, RefExpr, OpExpr, Cond, Stmt };
  virtual Kind getKind() const = 0;
  static bool classof(const GCommon *) { return true; }

public:
  bool isExpr() const { return isa<GExpr>(this); }
  bool isStmt() const { return getKind() == Stmt || getKind() == Cond; }
  virtual bool isInstruction() const { return false; }
  virtual bool isContainer() const { return false; }
  virtual bool isLoop() const { return false; }

  /// A node that is unit is supposed to be treated as an atomic unit an can
  /// only be rearranged as a whole in loop optimizations. It can still however
  /// be modified for normalization/canonicalization/low-lever analysis and
  /// optimzation. The outermost node that is a unit would be equivalent to a
  /// statement in polyhedral compilation.
  virtual bool isUnit() const { return true; }

public:
  /// In case we add support to modify after node creation, set staging to false
  /// to avoid further modification after the object might also refenced that
  /// assumes the object's immutability.
  bool isStaging() const { return false; }

public:
  virtual size_t getNumChildren() const { return 0; }
  virtual GCommon *getChild(int i) const { llvm_unreachable("No children"); }
  auto children() {
    return make_range(green_child_iterator(this, 0),
                      green_child_iterator(this, getNumChildren()));
  }

public:
  // TODO: whether something is read, killed or written depends on conditions;
  // Current idea: DenseMap to conditions/GreenNode when this happens
  // TODO: Use SetVector
  //  void determineScalars(DenseSet<GSymbol*>& Reads, DenseSet<GSymbol*>&
  //  Kills, DenseSet<GSymbol*>& Writes,  DenseSet<GSymbol*>& AllReferences );

private:
  Red *RedRoot = nullptr;

public:
  Red *asRedRoot();

public:
  virtual void printLine(llvm::raw_ostream &OS) const = 0;

#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  LLVM_DUMP_METHOD void dump() const;
#endif
};

class GExpr : public GCommon {
public:
  static bool classof(const GCommon *O) {
    auto K = O->getKind();
    return (K == Kind::RefExpr) || (K == Kind::OpExpr);
  }

public:
  virtual ~GExpr() {}

  // virtual bool isRef() const { return false;  }
  // virtual bool isOp() const { return false;  }

  static GSymbol *createRef(GSymbol *Sym) {
    assert(Sym);
    return Sym;
  }
  static GOpExpr *createOp(Operation Op, ArrayRef<GExpr *> Args);
}; // class GExpr

class GSymbol : public GExpr {
public:
  Kind getKind() const override { return Kind::RefExpr; }
  static bool classof(const GCommon *O) {
    return O->getKind() == GExpr::RefExpr;
  }

private:
  llvm::Value *LLVMName;

  // TODO: Define exactly what this means
  bool IsScalar = true;

  SmallVector<GExpr *, 4> DimSizes;

  // Number of subscript dimensions
  // For Arrays: Need the size of each dimension
  // For Scalar: Number of loop surrounding the definition of the scalar; can be
  // indexed by `last`, or surrounding loop counter value of the definition we
  // want to use
  int Dims = 0;

public:
  llvm::Value *getLLVMValue() const { return LLVMName; }

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
  llvm::Type *Ty;

public:
  llvm::Type *getType() const { return Ty; }

private:
  void assertOK() const {
    // Ty == nullptr means arbitrary integer width. To be validated at codegen.
    assert(!Ty || llvm::PointerType::isValidElementType(Ty));
  }

private:
  GSymbol(StringRef Name, llvm::Value *LLVMName, ArrayRef<GExpr *> DimSizes,
          llvm::Type *Ty)
      : Name(Name), LLVMName(LLVMName),
        DimSizes(DimSizes.begin(), DimSizes.end()), Ty(Ty) {
    assertOK();
  }

public:
  static GSymbol *createLLVM(llvm::Value *LLVMName) {
    return new GSymbol(LLVMName->getName(), LLVMName, {}, LLVMName->getType());
  }
  static GSymbol *createFromScratch(StringRef Name, llvm::Type *Ty) {
    return new GSymbol(Name, nullptr, {}, Ty);
  }
  static GSymbol *createArray(StringRef Name, ArrayRef<GExpr *> Sizes,
                              llvm::Type *ElemTy) {
    return new GSymbol(Name, nullptr, Sizes, ElemTy);
  }

public:
  void printLine(llvm::raw_ostream &OS) const override {
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
  SmallVector<GExpr *, 4> Args;

public:
  const Operation &getOperation() const { return Op; }
  ArrayRef<GExpr *> args() const { return Args; }
  ArrayRef<GExpr *> getArguments() const { return Args; }
  GExpr *getArgument(size_t Pos) const { return Args[Pos]; }
  size_t getNumArguments() const { return Args.size(); }

public:
  size_t getNumChildren() const override { return Args.size(); }
  GCommon *getChild(int i) const override { return Args[i]; }

private:
  GOpExpr(Operation Op, ArrayRef<GExpr *> Args)
      : Op(Op), Args(Args.begin(), Args.end()) {}

public:
  static GOpExpr *create(Operation Op, ArrayRef<GExpr *> Args) {
    assert(Op.isSpeculable());
    assert(Op.getNumInputs() == Args.size());
    return new GOpExpr(Op, Args);
  }

public:
  Kind getKind() const override { return GExpr::OpExpr; }
  static bool classof(const GCommon *O) {
    return O->getKind() == GExpr::OpExpr;
  }

public:
  // TODO: It's a singleton, or more general: uniqueing
  // TODO: move GExpr, how it's implemented doesn't matter
  static GOpExpr *createTrueExpr() {
    return createOp(Operation(Operation::True, nullptr), {});
  }
  static GOpExpr *createFalseExpr() {
    return createOp(Operation(Operation::False, nullptr), {});
  }

  static GOpExpr *createConstExpr(llvm::Constant *Val) {
    return createOp(Operation(Operation::LLVMSpeculable, Val), {});
  }

  static GOpExpr *createNotExpr(GExpr *Subexpr) {
    return createOp(Operation(Operation::Negation, nullptr), {Subexpr});
  }

  // TOOD: List of operands
  static GOpExpr *createConjunctionExpr(GExpr *LHS, GExpr *RHS) {
    return createOp(Operation(Operation::Conjuction, nullptr), {LHS, RHS});
  }
  static GOpExpr *createDisjunctionExpr(GExpr *LHS, GExpr *RHS) {
    return createOp(Operation(Operation::Disjunction, nullptr), {LHS, RHS});
  }

  //  static GOpExpr* createConst(LoopContext& Ctx, int Val);

public:
  void printLine(llvm::raw_ostream &OS) const override {
    OS << "GOpExpr";
    Op.printLine(OS);
  }
};

class GCond : public GCommon {
private:
  GExpr *Cond;
  GCommon *Child;

public:
  Kind getKind() const override { return GCommon::Cond; }

  size_t getNumChildren() const override { return 2; }
  GCommon *getChild(int i) const {
    switch (i) {
    case 0:
      return Cond;
    case 1:
      return Child;
    default:
      llvm_unreachable("out of range");
    }
  }
}; // class GCond

// TODO: rename Green to GStmt; rename GCommon to Green
class Green : public GCommon {
public:
  friend class GreenBuilder;
  friend class green_child_iterator;

private:
  /// If we make Green nodes edible after creation (to apply additional
  /// transformations when we know that the node is not used anywhere else),
  /// this flag indicated that this nodes is not yet used anywhere else.
  bool Staging = true;

  // bool IsFloating = false;

private:
  std::string Name;

public:
  StringRef getName() const { return Name; }

  Green *findNode(StringRef Name, bool Recursive = true);

  // TOOD: Generalize marking regions in the original IR.
private:
  llvm::Instruction *OrigBegin = nullptr;
  llvm::Instruction *OrigEnd = nullptr;
  llvm::Loop *OrigLoop = nullptr;

public:
  std::pair<llvm::Instruction *, llvm::Instruction *> getOrigRange() const {
    return {OrigBegin, OrigEnd};
  }
  llvm::Loop *getOrigLoop() const { return OrigLoop; }

private:
  /// Loop: Condition for executing first / another iterations
  /// Stmt (Instruction or Sequence): Not yet used, but might also be used as
  /// execution condition (in addition to the parent's ChildConds)
  GExpr *ExecCond = nullptr;

public:
  GExpr *getExecCond() const { return ExecCond; }

private:
  SmallSetVector<GSymbol *, 4> ScalarReads;
  SmallSetVector<GSymbol *, 4> ScalarKills;
  SmallSetVector<GSymbol *, 4> ScalarWrites;
  SmallSetVector<GSymbol *, 4> ScalarRecurrences;

public:
  ArrayRef<GSymbol *> getScalarReads() const {
    return ScalarReads.getArrayRef();
  }
  ArrayRef<GSymbol *> getScalarKills() const {
    return ScalarKills.getArrayRef();
  }
  ArrayRef<GSymbol *> getScalarWrites() const {
    return ScalarWrites.getArrayRef();
  }
  ArrayRef<GSymbol *> getScalarRecurrences() const {
    return ScalarRecurrences.getArrayRef();
  }

  bool hasScalarRead(GSymbol *Sym) const { return ScalarReads.count(Sym); }
  bool hasScalarWrite(GSymbol *Sym) const { return ScalarWrites.count(Sym); }
  bool hasScalarAccess(GSymbol *Sym) const {
    return hasScalarRead(Sym) || hasScalarWrite(Sym);
  }

  // bool hasComputedScalars() const {
  //   return ScalarReads.hasValue() && ScalarKills.hasValue() &&
  //   ScalarWrites.hasValue();
  // }

private:
  SmallVector<GExpr *, 8> ChildConds;
  SmallVector<Green *, 8> SubStmts;

private:
  /// [0] CanonicalCounter Counter always rises from 0 step 1 with an unsigned
  /// integer of undefined width (infinite, but will eventually be truncated).
  /// [1] IsFirstIteration Whether this is the first iteration of the loop;
  /// always exists
  /// TODO: Maybe list loop-carried scalars here as well
  SmallVector<GSymbol *, 2> DefinedInside;

public:
  GSymbol *getCanonicalCounter() const { return DefinedInside[0]; }
  GSymbol *getIsFirstIteration() const { return DefinedInside[1]; }

private:
  Green *TransformationOf = nullptr;
  DenseMap<GSymbol *, GExpr *> WhereMapping;
  mutable bool ConfirmedSemanticallyEquivalent = false;
  bool SemanticallyEquivalentIfChildrenAre = false;

public:
  Green *getTransformationOf() const { return TransformationOf; }

private:
  Green(StringRef Name, GExpr *ExecCond, ArrayRef<Green *> SubStmts,
        ArrayRef<GExpr *> Conds, bool IsLooping, llvm::Instruction *OrigBegin,
        llvm::Instruction *OrigEnd, ArrayRef<GSymbol *> ScalarReads,
        ArrayRef<GSymbol *> ScalarKills, ArrayRef<GSymbol *> ScalarWrites,
        ArrayRef<GSymbol *> ScalarRecurrences, GSymbol *LoopIsFirst,
        GSymbol *CanonicalCounter, Green *TransformationOf)
      : Name(Name), ExecCond(ExecCond),
        SubStmts(SubStmts.begin(), SubStmts.end()),
        ChildConds(Conds.begin(), Conds.end()), IsLoop(IsLooping),
        OrigBegin(OrigBegin),
        OrigEnd(OrigEnd), DefinedInside{CanonicalCounter, LoopIsFirst} {

    this->ScalarReads.insert(ScalarReads.begin(), ScalarReads.end());
    this->ScalarKills.insert(ScalarKills.begin(), ScalarKills.end());
    this->ScalarWrites.insert(ScalarWrites.begin(), ScalarWrites.end());
    this->ScalarRecurrences.insert(ScalarRecurrences.begin(),
                                   ScalarRecurrences.end());

    this->Staging = false;
    assertOK();
  }

private:
  Operation Op;
  SmallVector<GExpr *, 1> Arguments;
  SmallVector<GSymbol *, 1> Assignments;

public:
  size_t getNumArguments() const {
    assert(isInstruction());
    return Arguments.size();
  }
  ArrayRef<GExpr *> getArguments() const {
    assert(isInstruction());
    return Arguments;
  }
  GExpr *getArgument(size_t Pos) const { return Arguments[Pos]; }

  size_t getNumAssignments() const {
    assert(isInstruction());
    return Assignments.size();
  }
  ArrayRef<GSymbol *> getAssignments() const {
    assert(isInstruction());
    return Assignments;
  }
  GSymbol *getAssignment(size_t Pos) const { return Assignments[Pos]; }

private:
  Green(StringRef Name, Operation Op, ArrayRef<GExpr *> Arguments,
        ArrayRef<GSymbol *> Assignments, llvm::Instruction *OrigBegin,
        llvm::Instruction *OrigEnd, llvm::Loop *OrigLoop,
        Green *TransformationOf)
      : Name(Name), Op(Op), Arguments(Arguments.begin(), Arguments.end()),
        Assignments(Assignments.begin(), Assignments.end()),
        OrigBegin(OrigBegin), OrigEnd(OrigEnd), OrigLoop(OrigLoop),
        TransformationOf(TransformationOf) {

    DenseSet<GSymbol *> Reads, Kills, Writes, AllRefs;
    for (auto A : Arguments) {
      determineScalars(A, Reads, Kills, Writes, AllRefs);
    }
    this->ScalarReads.insert(Reads.begin(), Reads.end());
    this->ScalarWrites.insert(Assignments.begin(), Assignments.end());
    this->ScalarKills = this->ScalarWrites;

    this->Staging = false;
    assertOK();
  }

public:
  static Green *createInstruction(StringRef Name, Operation Op,
                                  ArrayRef<GExpr *> Arguments,
                                  ArrayRef<GSymbol *> Assignments,
                                  llvm::Instruction *OrigInst,
                                  Green *TransformationOf) {
    llvm::Instruction *OrigEnd = nullptr;
    if (OrigInst)
      OrigEnd = OrigInst->getNextNode();
    return new Green(Name, Op, Arguments, Assignments, OrigInst, OrigEnd,
                     nullptr, TransformationOf);
  }
  bool isInstruction() const override { return Op.isValid(); }

public:
  const Operation &getOperation() const { return Op; }

public:
  Kind getKind() const { return GCommon::Stmt; }
  static bool classof(const GCommon *O) {
    return O->getKind() == GCommon::Stmt;
  }

  void assertOK() const {
    if (Staging)
      return;

    assert(isStmt());
    assert(!isExpr());
    assert(isInstruction() ^ isContainer());

    if (isContainer()) {
      assert(isLoop() ^ isSequence());
    }

    if (isLoop()) {
      assert(isContainer());
      assert(getIsFirstIteration());
    }

    if (isSequence()) {
    }

    if (isInstruction()) {
      auto NumInputs = getOperation().getNumInputs();
      auto NumOutputs = getOperation().getNumOutputs();

      assert(getNumArguments() == NumInputs);
      assert(getNumAssignments() == NumOutputs);
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
  bool isContainer() const override { return !Op.isValid(); }
  bool isSequence() const { return isContainer() && !isLoop(); }

  /// TODO: Allow compiling instruction sequences into units.
  bool isUnit() const override { return isInstruction(); }

  ArrayRef<Green *> getSubStmts() const { return SubStmts; }
  Green *getSubStmt(int i) const { return SubStmts[i]; }
  auto getNumSubStmt() const { return SubStmts.size(); }
  GExpr *getSubCond(int i) const { return ChildConds[i]; }

  int getNumPrologueStmts() const {
    if (!isLoop())
      return 0;
    for (auto i = 0; i < SubStmts.size(); i += 1) {
      if (!SubStmts[i]->isInstruction())
        return i;
      if (SubStmts[i]->getOperation().getKind() != Operation::Select)
        return i;
    }
    return SubStmts.size();
  }

  int getNumEpilogueStmts() const {
    if (!isLoop())
      return 0;
    auto NumSubStmts = SubStmts.size();
    for (int i = NumSubStmts - 1; i >= 0; i -= 1) {
      if (!SubStmts[i]->isInstruction())
        return NumSubStmts - (i + 1);
      if (!SubStmts[i]->getOperation().isReduction())
        return NumSubStmts - (i + 1);
    }
    return NumSubStmts;
  }

  auto getPrologueStmts() const {
    auto NumProglogue = getNumPrologueStmts();
    return getSubStmts().slice(0, NumProglogue);
  }

  auto getMainSubStmts() const {
    auto NumTotal = getNumSubStmt();
    auto NumProglogue = getNumPrologueStmts();
    auto NumEpilogie = getNumEpilogueStmts();
    return getSubStmts().slice(NumProglogue,
                               NumTotal - NumProglogue - NumEpilogie);
  }

  auto getEpilogueStmts() const {
    auto NumTotal = getNumSubStmt();
    auto NumEpilogue = getNumEpilogueStmts();
    return getSubStmts().slice(NumTotal - NumEpilogue, NumEpilogue);
  }

  size_t getNumChildren() const override {
    if (isInstruction()) {
      assert(SubStmts.empty());
      return Arguments.size();
    }
    assert(Arguments.empty());
    return SubStmts.size();
  }
  ArrayRef<GCommon *> getChildren() const {
    if (isInstruction()) {
      return ArrayRef<GCommon *>(
          reinterpret_cast<GCommon *const *>(Arguments.data()),
          Arguments.size());
    }

    return ArrayRef<GCommon *>(
        reinterpret_cast<GCommon *const *>(SubStmts.data()), SubStmts.size());
  }
  GCommon *getChild(int i) const { return getChildren()[i]; }

  auto children() const {
    // return make_range( green_child_iterator(this, 0),
    // green_child_iterator(this, Children.size())  );
    return getChildren();
  }

#if 0
      auto descententsDepthFirst() {
        return llvm::depth_first(this);
      }

      auto descententsBreadtFirst() {
        return llvm:: breadth_first(this);
      }
#endif

  // LLVM_DUMP_METHOD void dump() const;
  // void print(raw_ostream& OS) const;

public:
public:
  void printLine(llvm::raw_ostream &OS) const override {
    if (isLoop()) {
      OS << "Loop";
    } else if (isInstruction()) {
      OS << "Inst ";
      Op.printLine(OS);
    } else if (isStmt()) {
      OS << "Sequence";
    } else {
      OS << "???";
    }

    if (!Name.empty()) {
      OS << " '" << Name << "' ";
    }

    auto PrintSymbolList = [&OS](auto Name, auto X) {
      // if (X) {
      OS << " " << Name << "=[";
      for (auto A : llvm::enumerate(X)) {
        if (A.index() > 0)
          OS << ", ";
        A.value()->printReprName(OS);
      }
      OS << "]";
      //}
    };

    PrintSymbolList("ScalarReads", ScalarReads);
    // PrintSymbolList("ScalarKills", ScalarKills);
    PrintSymbolList("ScalarWrites", ScalarWrites);
    if (isLoop())
      PrintSymbolList("ScalarRecurrences", ScalarRecurrences);
  }

}; // class Green

class GreenDumper {
private:
  llvm::raw_ostream &OS;
  GCommon *Highlight;
  bool OnlyStmts;
  int Indent = 0;
  DenseMap<GCommon *, int> Multiplicity;
  DenseSet<GCommon *> Visited;

  void recursiveCount(GCommon *Node) {
    auto &Count = Multiplicity[Node];
    Count += 1;

    // Already visited?
    if (Count >= 2)
      return;

    for (auto C : Node->children())
      recursiveCount(C);
  }

  void dumpNode(GCommon *Node, StringRef Role) {
    if (Node == Highlight) {
      OS << ">>";
      if ((Indent * 2 - 2) > 0)
        OS.indent(Indent * 2 - 2);
    } else {
      OS.indent(Indent * 2);
    }
    if (!Role.empty())
      OS << Role << ": ";

    if (isa<GRefExpr>(Node)) {
      Node->printLine(OS);
    } else {
      switch (Multiplicity.lookup(Node)) {
      case 0:
        llvm_unreachable("Should have been visited");
      default: {
        auto It = Visited.insert(Node);
        if (!It.second) {
          // print address
          OS << Node << '\n';
          return;
        }

        // Node->printLine(OS);
        // print address
        OS << Node << ' ';
        Node->printLine(OS);
      } break;
      case 1:
        Node->printLine(OS);
        break;
      }
    }
    OS << '\n';

    Indent += 1;
    if (Node->isContainer()) {
      auto G = cast<Green>(Node);
      auto NumChildren = G->getNumChildren();
      for (int i = 0; i < NumChildren; i += 1) {
        auto Cond = G->getSubCond(i);
        auto Child = G->getChild(i);
        if (OnlyStmts || (isa<GOpExpr>(Cond) &&
                          cast<GOpExpr>(Cond)->getOperation().getKind() ==
                              Operation::True)) {
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

    } else {
      for (auto Child : Node->children()) {
        if (OnlyStmts && !Child->isStmt())
          continue;
        dumpNode(Child, StringRef());
      }
    }
    Indent -= 1;
  }

public:
  GreenDumper(llvm::raw_ostream &OS, bool OnlyStmts = false,
              GCommon *Highlight = nullptr)
      : OS(OS), Highlight(Highlight), OnlyStmts(OnlyStmts) {}

  void dump(GCommon *Node) {
    Multiplicity.clear();
    Visited.clear();
    recursiveCount(Node);
    dumpNode(Node, StringRef());
  }

}; // class GreenDumper

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
GraphTraits<lof::GCommon *>::ChildIteratorType
GraphTraits<lof::GCommon *>::child_begin(NodeRef N) {
  return N->children().begin();
}
GraphTraits<lof::GCommon *>::ChildIteratorType
GraphTraits<lof::GCommon *>::child_end(NodeRef N) {
  return N->children().end();
}

GraphTraits<lof::GCommon *>::nodes_iterator
GraphTraits<lof::GCommon *>::nodes_begin(lof::GCommon *G) {
  return nodes_iterator::begin(getEntryNode(G));
}
GraphTraits<lof::GCommon *>::nodes_iterator
GraphTraits<lof::GCommon *>::nodes_end(lof::GCommon *G) {
  return nodes_iterator::end(getEntryNode(G));
}
} // namespace llvm

#endif /* LLVM_LOF_GREEN_H */
