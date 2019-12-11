//===--- TransformedTree.h --------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  Applies code transformations.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_ANALYSIS_TRANSFORMEDTREE_H
#define LLVM_CLANG_ANALYSIS_TRANSFORMEDTREE_H

#include "clang/AST/OpenMPClause.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/Stmt.h"
#include "clang/AST/StmtOpenMP.h"
#include "clang/Analysis/AnalysisTransform.h"
#include "clang/Basic/DiagnosticSema.h"
#include "llvm/ADT/SmallVector.h"

namespace clang {
template <typename Derived, typename NodeTy> class TransformedTreeBuilder;

struct DefaultExtractTransform : ExtractTransform<DefaultExtractTransform> {
  DefaultExtractTransform(ASTContext &ASTCtx,
                          const TransformExecutableDirective *Directive)
      : ExtractTransform(ASTCtx, Directive) {}

  // Ignore any diagnostic and its arguments.
  struct DummyDiag {
    template <typename T> DummyDiag operator<<(const T &) const { return {}; }
  };
  DummyDiag Diag(SourceLocation Loc, unsigned DiagID) { return {}; }
};

/// Represents an input of a code representation.
/// Current can reference the input code only by the AST node, but in the future
/// loops can also given identifiers to reference them.
class TransformInput {
  const Stmt *StmtInput = nullptr;
  Transform *PrecTrans = nullptr;
  int FollowupIdx = -1;

  TransformInput(const Stmt *StmtInput, Transform *PrecTrans, int FollowupIdx)
      : StmtInput(StmtInput), PrecTrans(PrecTrans), FollowupIdx(FollowupIdx) {}

public:
  TransformInput() {}

  static TransformInput createByStmt(const Stmt *StmtInput) {
    assert(StmtInput);
    return TransformInput(StmtInput, nullptr, -1);
  }

  // In general, the same clang::Transform can be reused multiple times with
  // different inputs, when referencing its followup using this constructor, the
  // clang::Transform can only be used once per function to ensure that its
  // followup can be uniquely identified.
  static TransformInput createByFollowup(Transform *Transform,
                                         int FollowupIdx) {
    assert(Transform);
    assert(0 <= FollowupIdx && FollowupIdx < Transform->getNumFollowups());
    return TransformInput(nullptr, Transform, FollowupIdx);
  }

  bool isByStmt() const { return StmtInput; }
  bool isByFollowup() const { return PrecTrans; }

  const Stmt *getStmtInput() const { return StmtInput; }

  Transform *getPrecTrans() const { return PrecTrans; }
  int getFollowupIdx() const { return FollowupIdx; }
};

/// Represents a transformation together with the input loops.
/// in the future it will also identify the generated loop.
struct NodeTransform {
  Transform *Trans = nullptr;
  llvm::SmallVector<TransformInput, 2> Inputs;

  NodeTransform() {}
  NodeTransform(Transform *Trans, TransformInput TopLevelInput) : Trans(Trans) {
    assert(Trans->getNumInputs() >= 1);

    Inputs.resize(Trans->getNumInputs());
    setInput(0, TopLevelInput);
  };

  void setInput(int Idx, TransformInput Input) { Inputs[Idx] = Input; }
};

/// This class represents a loop in a loop nest to which transformations are
/// applied to. It is intended to be instantiated for it specific purpose, that
/// is SemaTransformedTree for semantic analysis (consistency warnings and
/// errors) and CGTransformedTree for emitting IR.
template <typename Derived> class TransformedTree {
  template <typename, typename> friend class TransformedTreeBuilder;
  using NodeTy = Derived;

  Derived &getDerived() { return *static_cast<Derived *>(this); }
  const Derived &getDerived() const {
    return *static_cast<const Derived *>(this);
  }

protected:
  /// Is this the root node of the loop hierarchy?
  bool IsRoot = false;

  /// Does this node have a loop hint applied to it?
  bool HasLoopHint = false;

  /// Nested loops.
  llvm::SmallVector<Derived *, 2> Subloops;

  /// Origin of this loop.
  /// @{

  /// If not the result of the transformation, this is the loop statement that
  /// this node represents.
  Stmt *Original;

  Derived *BasedOn;
  int FollowupRole;
  /// @}

  /// Things applied to this loop.
  /// @{

  /// If this is the primary transformation input.
  Transform *TransformedBy = nullptr;

  // Primary/secondary transformation input
  Derived *PrimaryInput = nullptr;

  llvm::SmallVector<Derived *, 4> Followups;

  // Derived *Successor = nullptr;
  // First successor is the primary (the one that #pragma clang transform is
  // applied to)
  llvm::SmallVector<Derived *, 2> Successors;

  int InputRole = -1;
  /// @}

protected:
  TransformedTree(llvm::ArrayRef<Derived *> SubLoops, Derived *BasedOn,
                  clang::Stmt *Original, int FollowupRole)
      : Subloops(SubLoops.begin(), SubLoops.end()), Original(Original),
        BasedOn(BasedOn), FollowupRole(FollowupRole) {}

public:
  ArrayRef<Derived *> getSubLoops() const { return Subloops; }

  std::vector<Derived *> getLatestSubLoops() {
    std::vector<Derived *> Result;
    Result.reserve(Subloops.size());
    for (auto SubL : Subloops) {
      // TODO: Efficiency
      auto SubLatest = SubL->getLatestSuccessors();
      Result.insert(Result.end(), SubLatest.begin(), SubLatest.end());
    }
    return Result;
  }

  Derived *getPrimaryInput() const { return PrimaryInput; }
  Transform *getTransformedBy() const { return TransformedBy; }

  /// Return the transformation that generated this loop. Return nullptr if not
  /// the result of any transformation, i.e. it is an original loop.
  Transform *getSourceTransformation() const {
    assert(!BasedOn == isOriginal() &&
           "Non-original loops must be based on some other loop");
    if (isOriginal())
      return nullptr;

    assert(BasedOn);
    assert(BasedOn->isTransformationInput());
    Transform *Result = BasedOn->PrimaryInput->TransformedBy;
    assert(Result &&
           "Non-original loops must have a generating transformation");
    return Result;
  }

  Stmt *getOriginal() const { return Original; }
  Stmt *getInheritedOriginal() const {
    if (Original)
      return Original;
    if (BasedOn)
      return BasedOn->getInheritedOriginal();
    return nullptr;
  }

  Derived *getBasedOn() const { return BasedOn; }

  bool isRoot() const { return IsRoot; }

  bool hasLoopHint() const { return HasLoopHint; }

  void markLoopHint() { HasLoopHint = true; }

  ArrayRef<Derived *> getSuccessors() const { return Successors; }

  // TODO: Accept SmallArrayImpl to fill
  std::vector<Derived *> getLatestSuccessors() {
    // If the loop is not being consumed, this is the latest successor.
    if (!isTransformationInput()) {
      std::vector<Derived *> Result;
      Result.push_back(&getDerived());
      return Result;
      // return { &getDerived() };
    }

    std::vector<Derived *> Result;
    for (auto Succ : Successors) {
      auto SuccResult = Succ->getLatestSuccessors();
      Result.insert(Result.end(), SuccResult.begin(), SuccResult.end());
    }
    return Result;
  }

  bool isOriginal() const { return Original; }

  bool isTransformationInput() const {
    bool Result = InputRole >= 0;
    assert(Result == (PrimaryInput != nullptr));
    return Result;
  }

  bool isTransformationFollowup() const {
    bool Result = FollowupRole >= 0;
    assert(Result == (BasedOn != nullptr));
    return Result;
  }

  bool isPrimaryInput() const {
    bool Result = (InputRole == 0);
    assert(Result == (PrimaryInput == this));
    return PrimaryInput == this;
  }

  int getFollowupRole() const { return FollowupRole; }

  ArrayRef<Derived *> getFollowups() const { return Followups; }

  void applyTransformation(Transform *Trans,
                           llvm::ArrayRef<Derived *> Followups,
                           ArrayRef<Derived *> Successors) {
    assert(!isTransformationInput());
    assert(llvm::find(Successors, nullptr) == Successors.end());
    assert(Trans->getNumFollowups() == Followups.size());

    this->TransformedBy = Trans;
    this->Followups.insert(this->Followups.end(), Followups.begin(),
                           Followups.end());
    this->Successors.assign(Successors.begin(), Successors.end());
    this->PrimaryInput = &getDerived();
    this->InputRole = 0; // for primary

#ifndef NDEBUG
    assert(isTransformationInput() && isPrimaryInput());
    for (NodeTy *S : Followups) {
      assert(S->BasedOn == &getDerived());
    }
#endif
  }

  void applySuccessors(Derived *PrimaryInput, int InputRole,
                       llvm::ArrayRef<Derived *> Followups,
                       ArrayRef<Derived *> Successors) {
    assert(!isTransformationInput());
    assert(InputRole > 0);
    assert(llvm::find(Successors, nullptr) == Successors.end());

    this->PrimaryInput = PrimaryInput;
    this->Followups.insert(this->Followups.end(), Followups.begin(),
                           Followups.end());
    this->Successors.assign(Successors.begin(), Successors.end());
    this->InputRole = InputRole;

#ifndef NDEBUG
    assert(isTransformationInput() && !isPrimaryInput());
    for (NodeTy *S : Followups) {
      assert(S->BasedOn == &getDerived());
    }
#endif
  }
};

/// Constructs a loop nest from source and applies transformations on it.
template <typename Derived, typename NodeTy> class TransformedTreeBuilder {
  using BuilderTy = Derived;

  Derived &getDerived() { return *static_cast<Derived *>(this); }
  const Derived &getDerived() const {
    return *static_cast<const Derived *>(this);
  }

  ASTContext &ASTCtx;
  const LangOptions &LangOpts;
  llvm::SmallVectorImpl<NodeTy *> &AllNodes;
  llvm::SmallVectorImpl<Transform *> &AllTransforms;

private:
  /// Build the original loop nest hierarchy from the AST.
  void buildPhysicalLoopTree(Stmt *S, SmallVectorImpl<NodeTy *> &SubLoops,
                             llvm::DenseMap<Stmt *, NodeTy *> &StmtToTree,
                             bool MarkLoopHint = false) {
    if (!S)
      return;

    Stmt *Body;
    switch (S->getStmtClass()) {
    case Stmt::ForStmtClass:
      Body = cast<ForStmt>(S)->getBody();
      break;
    case Stmt::WhileStmtClass:
      Body = cast<WhileStmt>(S)->getBody();
      break;
    case Stmt::DoStmtClass:
      Body = cast<DoStmt>(S)->getBody();
      break;
    case Stmt::CXXForRangeStmtClass:
      Body = cast<CXXForRangeStmt>(S)->getBody();
      break;
    case Stmt::CapturedStmtClass:
      buildPhysicalLoopTree(cast<CapturedStmt>(S)->getCapturedStmt(), SubLoops,
                            StmtToTree, MarkLoopHint);
      return;
    case Expr::LambdaExprClass:
      // Call to getBody materializes its body, children() (which is called in
      // the default case) does not.
      buildPhysicalLoopTree(cast<LambdaExpr>(S)->getBody(), SubLoops,
                            StmtToTree);
      return;
    case Expr::BlockExprClass:
      buildPhysicalLoopTree(cast<BlockExpr>(S)->getBody(), SubLoops,
                            StmtToTree);
      return;
    case Expr::AttributedStmtClass:
      buildPhysicalLoopTree(
          cast<AttributedStmt>(S)->getSubStmt(), SubLoops, StmtToTree,
          llvm::any_of(cast<AttributedStmt>(S)->getAttrs(),
                       [](const Attr *A) { return isa<LoopHintAttr>(A); }));
      return;
    default:
      if (auto *O = dyn_cast<OMPExecutableDirective>(S)) {
        if (!O->hasAssociatedStmt())
          return;
        MarkLoopHint = true;
        Stmt *Associated = O->getAssociatedStmt();
        buildPhysicalLoopTree(Associated, SubLoops, StmtToTree, true);
        return;
      }

      for (Stmt *Child : S->children())
        buildPhysicalLoopTree(Child, SubLoops, StmtToTree);

      return;
    }

    SmallVector<NodeTy *, 8> SubSubLoops;
    buildPhysicalLoopTree(Body, SubSubLoops, StmtToTree);

    NodeTy *L = getDerived().createPhysical(SubSubLoops, S);
    if (MarkLoopHint)
      L->markLoopHint();

    SubLoops.push_back(L);
    assert(StmtToTree.count(S) == 0);
    StmtToTree[S] = L;

    getDerived().applyOriginal(L);
  }

  /// Collect all loop transformations in the function's AST.
  class CollectTransformationsVisitor
      : public RecursiveASTVisitor<CollectTransformationsVisitor> {
    Derived &Builder;
    llvm::DenseMap<Stmt *, NodeTy *> &StmtToTree;

  public:
    CollectTransformationsVisitor(Derived &Builder,
                                  llvm::DenseMap<Stmt *, NodeTy *> &StmtToTree)
        : Builder(Builder), StmtToTree(StmtToTree) {}

    /// Transformations collected so far.
    llvm::SmallVector<NodeTransform, 16> Transforms;

    bool shouldTraversePostOrder() const { return true; }

    bool
    VisitTransformExecutableDirective(const TransformExecutableDirective *S) {
      // TODO: Check if AttributeStmt with LoopHint or OpenMP is also also
      // present and error-out if it is.

      // This ExtractTransform does not emit any diagnostics. Diagnostics should
      // have been emitted in Sema::ActOnTransformExecutableDirective.
      DefaultExtractTransform ExtractTransform(Builder.ASTCtx, S);
      std::unique_ptr<Transform> Trans = ExtractTransform.createTransform();

      // We might not get a transform in non-instantiated templates or
      // inconsistent clauses.
      if (Trans) {
        const Stmt *TheLoop = getAssociatedLoop(S->getAssociated());
        Transforms.emplace_back(Trans.get(),
                                TransformInput::createByStmt(TheLoop));
        Builder.AllTransforms.push_back(Trans.release());
      }

      return true;
    }
  };

  /// Applies collected transformations to the loop nest representation.
  struct TransformApplicator {
    Derived &Builder;
    llvm::DenseMap<const Stmt *, llvm::SmallVector<NodeTransform *, 2>>
        TransByStmt;
    llvm::DenseMap<Transform *, llvm::SmallVector<NodeTransform *, 2>>
        TransformByFollowup;

    TransformApplicator(Derived &Builder) : Builder(Builder) {}

    void addNodeTransform(NodeTransform *NT) {
      TransformInput &TopLevelInput = NT->Inputs[0];
      if (TopLevelInput.isByStmt()) {
        TransByStmt[TopLevelInput.getStmtInput()].push_back(NT);
      } else if (TopLevelInput.isByFollowup()) {
        TransformByFollowup[TopLevelInput.getPrecTrans()].push_back(NT);
      } else
        llvm_unreachable("Transformation must apply to something");
    }

    void checkStageOrder(ArrayRef<NodeTy *> PrevLoops, Transform *NewTrans) {
      for (NodeTy *PrevLoop : PrevLoops) {

        // Cannot combine legacy disable pragmas (e.g. #pragma clang loop
        // unroll(disable)) and new transformations (#pragma clang transform).
        if (PrevLoop->hasLoopHint()) {
          Builder.Diag(NewTrans->getBeginLoc(),
                       diag::err_sema_transform_legacy_mix);
          return;
        }

        Transform *PrevSourceTrans = PrevLoop->getSourceTransformation();
        if (!PrevSourceTrans)
          continue;

        int PrevStage = PrevSourceTrans->getLoopPipelineStage();
        int NewStage = NewTrans->getLoopPipelineStage();
        if (PrevStage >= 0 && NewStage >= 0 && PrevStage > NewStage) {
          Builder.Diag(NewTrans->getBeginLoc(),
                       diag::warn_sema_transform_pass_order);

          // At most one warning per transformation.
          return;
        }
      }
    }

    NodeTy *applyTransform(Transform *Trans, NodeTy *MainLoop) {
      switch (Trans->getKind()) {
      case Transform::Kind::LoopUnrollKind:
        return applyUnrolling(cast<LoopUnrollTransform>(Trans), MainLoop);
      case Transform::Kind::LoopUnrollAndJamKind:
        return applyUnrollAndJam(cast<LoopUnrollAndJamTransform>(Trans),
                                 MainLoop);
      case Transform::Kind::LoopDistributionKind:
        return applyDistribution(cast<LoopDistributionTransform>(Trans),
                                 MainLoop);
      case Transform::Kind::LoopVectorizationKind:
        return applyVectorize(cast<LoopVectorizationTransform>(Trans),
                              MainLoop);
      case Transform::Kind::LoopInterleavingKind:
        return applyInterleave(cast<LoopInterleavingTransform>(Trans),
                               MainLoop);
      default:
        llvm_unreachable("unimplemented transformation");
      }
    }

    void inheritLoopAttributes(NodeTy *Dst, NodeTy *Src, bool IsAll,
                               bool IsSuccessor) {
      Builder.inheritLoopAttributes(Dst, Src, IsAll, IsSuccessor);
    }

    NodeTy *applyUnrolling(LoopUnrollTransform *Trans, NodeTy *MainLoop) {
      checkStageOrder({MainLoop}, Trans);

      NodeTy *Successor = nullptr;
      if (Trans->isFull()) {
        // Full unrolling has no followup-loop.
        MainLoop->applyTransformation(Trans, {}, {});
      } else {
        NodeTy *All = Builder.createFollowup(MainLoop->Subloops, MainLoop,
                                             LoopUnrollTransform::FollowupAll);
        NodeTy *Unrolled =
            Builder.createFollowup(MainLoop->Subloops, MainLoop,
                                   LoopUnrollTransform::FollowupUnrolled);
        NodeTy *Remainder =
            Builder.createFollowup(MainLoop->Subloops, MainLoop,
                                   LoopUnrollTransform::FollowupRemainder);
        Successor = Unrolled;
        inheritLoopAttributes(All, MainLoop, true, All == Successor);
        MainLoop->applyTransformation(Trans, {All, Unrolled, Remainder},
                                      Successor);
      }

      Builder.applyUnroll(Trans, MainLoop);
      return Successor;
    }

    NodeTy *applyUnrollAndJam(LoopUnrollAndJamTransform *Trans,
                              NodeTy *MainLoop) {
      // Search for the innermost loop that is being jammed.
      NodeTy *Cur = MainLoop;
      NodeTy *Inner = nullptr;
      auto LatestInner = Cur->getLatestSubLoops();
      if (LatestInner.size() == 1) {
        Inner = LatestInner[0];
      } else {
        Builder.Diag(Trans->getBeginLoc(),
                     diag::err_sema_transform_unrollandjam_expect_nested_loop);
        return nullptr;
      }

      if (!Inner) {
        Builder.Diag(Trans->getBeginLoc(),
                     diag::err_sema_transform_unrollandjam_expect_nested_loop);
        return nullptr;
      }

      if (Inner->Subloops.size() != 0) {
        Builder.Diag(Trans->getBeginLoc(),
                     diag::err_sema_transform_unrollandjam_not_innermost);
        return nullptr;
      }

      checkStageOrder({MainLoop, Inner}, Trans);

      NodeTy *TransformedAll = Builder.createFollowup(
          {}, nullptr, LoopUnrollAndJamTransform::FollowupAll);
      inheritLoopAttributes(TransformedAll, MainLoop, true, false);

      NodeTy *UnrolledOuter = Builder.createFollowup(
          {Inner}, MainLoop, LoopUnrollAndJamTransform::FollowupOuter);
      inheritLoopAttributes(UnrolledOuter, MainLoop, false, true);

      NodeTy *TransformedInner = Builder.createFollowup(
          Inner->Subloops, Inner, LoopUnrollAndJamTransform::FollowupInner);
      inheritLoopAttributes(TransformedInner, Inner, false, false);

      MainLoop->applyTransformation(
          Trans, {TransformedAll, UnrolledOuter, TransformedInner},
          UnrolledOuter);
      Inner->applySuccessors(MainLoop, LoopUnrollAndJamTransform::InputInner,
                             {TransformedInner}, TransformedInner);

      Builder.applyUnrollAndJam(Trans, MainLoop, Inner);
      return UnrolledOuter;
    }

    NodeTy *applyDistribution(LoopDistributionTransform *Trans,
                              NodeTy *MainLoop) {
      checkStageOrder({MainLoop}, Trans);

      NodeTy *All = Builder.createFollowup(
          MainLoop->Subloops, MainLoop, LoopDistributionTransform::FollowupAll);
      NodeTy *PrimarySuccessor;

      inheritLoopAttributes(All, MainLoop, true, false);
      MainLoop->applyTransformation(Trans, {All}, {});
      PrimarySuccessor = nullptr;

      Builder.applyDistribution(Trans, MainLoop);
      return PrimarySuccessor;
    }

    NodeTy *applyVectorize(LoopVectorizationTransform *Trans,
                           NodeTy *MainLoop) {
      checkStageOrder({MainLoop}, Trans);

      NodeTy *All =
          Builder.createFollowup(MainLoop->Subloops, MainLoop,
                                 LoopVectorizationTransform::FollowupAll);
      NodeTy *Vectorized = Builder.createFollowup(
          MainLoop->Subloops, MainLoop,
          LoopVectorizationTransform::FollowupVectorized);
      NodeTy *Epilogue =
          Builder.createFollowup(MainLoop->Subloops, MainLoop,
                                 LoopVectorizationTransform::FollowupEpilogue);

      NodeTy *Successor = Vectorized;

      inheritLoopAttributes(All, MainLoop, true, All == Successor);
      MainLoop->applyTransformation(Trans, {All, Vectorized, Epilogue},
                                    Successor);
      Builder.applyVectorization(Trans, MainLoop);
      return Successor;
    }

    NodeTy *applyInterleave(LoopInterleavingTransform *Trans,
                            NodeTy *MainLoop) {
      checkStageOrder({MainLoop}, Trans);

      NodeTy *All = Builder.createFollowup(
          MainLoop->Subloops, MainLoop, LoopInterleavingTransform::FollowupAll);
      NodeTy *Vectorized = Builder.createFollowup(
          MainLoop->Subloops, MainLoop,
          LoopInterleavingTransform::FollowupInterleaved);
      NodeTy *Epilogue =
          Builder.createFollowup(MainLoop->Subloops, MainLoop,
                                 LoopInterleavingTransform::FollowupEpilogue);

      NodeTy *Successor = Vectorized;

      inheritLoopAttributes(All, MainLoop, true, All == Successor);
      MainLoop->applyTransformation(Trans, {All, Vectorized, Epilogue},
                                    Successor);
      Builder.applyInterleaving(Trans, MainLoop);
      return Successor;
    }

    void traverseSubloops(NodeTy *L) {
      // TODO: Instead of recursively traversing the entire subtree, in case we
      // are re-traversing after a transformation, only traverse followups of
      // that transformation.
      for (NodeTy *SubL : L->getSubLoops()) {
        auto Latest = SubL->getLatestSuccessors();
        for (auto SubL : Latest)
          traverse(SubL);
      }
    }

    bool applyTransform(NodeTy *L) {
      if (L->isRoot())
        return false;
      // assert(L == L->getLatestSuccessor() && "Loop must not have been
      // consumed by another transformation");

      // Look for transformations that apply syntactically to this loop.
      Stmt *OrigStmt = L->getInheritedOriginal();
      auto TransformsOnStmt = TransByStmt.find(OrigStmt);
      if (TransformsOnStmt != TransByStmt.end()) {
        auto &List = TransformsOnStmt->second;
        if (!List.empty()) {
          NodeTransform *Trans = List.front();
          applyTransform(Trans->Trans, L);
          List.erase(List.begin());

          return true;
        }
      }

      // Look for transformations that are chained to one of the followups.
      auto SourceTrans = L->getSourceTransformation();
      if (!SourceTrans)
        return false;
      auto Chained = TransformByFollowup.find(SourceTrans);
      if (Chained == TransformByFollowup.end())
        return false;
      auto LIdx = L->getFollowupRole();
      auto &List = Chained->second;
      for (auto It = List.begin(), E = List.end(); It != E; ++It) {
        NodeTransform *Item = *It;
        auto FollowupIdx = Item->Inputs[0].getFollowupIdx();
        if (LIdx != FollowupIdx)
          continue;

        applyTransform(Item->Trans, L);
        List.erase(It);
        return true;
      }

      return false;
    }

    void traverse(NodeTy *N) {
      auto Latest = N->getLatestSuccessors();
      for (auto L : Latest) {
        traverseSubloops(L);
        if (applyTransform(L)) {
          // Apply transformations on nested followups.
          traverse(L);
        }
      }
    }
  };

protected:
  TransformedTreeBuilder(ASTContext &ASTCtx, const LangOptions &LangOpts,
                         llvm::SmallVectorImpl<NodeTy *> &AllNodes,
                         llvm::SmallVectorImpl<Transform *> &AllTransforms)
      : ASTCtx(ASTCtx), LangOpts(LangOpts), AllNodes(AllNodes),
        AllTransforms(AllTransforms) {}

  NodeTy *createRoot(llvm::ArrayRef<NodeTy *> SubLoops) {
    auto *Result = new NodeTy(SubLoops, nullptr, nullptr, -1);
    AllNodes.push_back(Result);
    Result->IsRoot = true;
    return Result;
  }

  NodeTy *createPhysical(llvm::ArrayRef<NodeTy *> SubLoops,
                         clang::Stmt *Original) {
    assert(Original);
    auto *Result = new NodeTy(SubLoops, nullptr, Original, -1);
    AllNodes.push_back(Result);
    return Result;
  }

  NodeTy *createFollowup(llvm::ArrayRef<NodeTy *> SubLoops, NodeTy *BasedOn,
                         int FollowupRole) {
    auto *Result = new NodeTy(SubLoops, BasedOn, nullptr, FollowupRole);
    AllNodes.push_back(Result);
    return Result;
  }

public:
  NodeTy *
  computeTransformedStructure(Stmt *Body,
                              llvm::DenseMap<Stmt *, NodeTy *> &StmtToTree) {
    if (!Body)
      return nullptr;

    // Create original tree.
    SmallVector<NodeTy *, 8> TopLevelLoops;
    buildPhysicalLoopTree(Body, TopLevelLoops, StmtToTree);
    NodeTy *Root = getDerived().createRoot(TopLevelLoops);

    // Collect all loop transformations.
    CollectTransformationsVisitor Collector(getDerived(), StmtToTree);
    Collector.TraverseStmt(Body);
    auto &TransformList = Collector.Transforms;

    // Local function to apply every transformation that the predicate returns
    // true to. This is to emulate LLVM's pass pipeline that would apply
    // transformation in this order.
    auto SelectiveApplicator = [this, &TransformList, Root](auto Pred) {
      TransformApplicator OMPApplicator(getDerived());
      bool AnyActive = false;
      for (NodeTransform &NT : TransformList) {
        if (Pred(NT)) {
          OMPApplicator.addNodeTransform(&NT);
          AnyActive = true;
        }
      }

      // No traversal needed if no transformations to apply.
      if (!AnyActive)
        return;

      OMPApplicator.traverse(Root);

      // Report leftover transformations.
      for (auto &P : OMPApplicator.TransByStmt) {
        for (NodeTransform *NT : P.second) {
          getDerived().Diag(NT->Trans->getBeginLoc(),
                            diag::err_sema_transform_missing_loop);
        }
      }

      // Remove applied transformations from list.
      auto NewEnd =
          std::remove_if(TransformList.begin(), TransformList.end(), Pred);
      TransformList.erase(NewEnd, TransformList.end());
    };

    // Apply all others.
    SelectiveApplicator([](NodeTransform &NT) -> bool { return true; });
    assert(TransformList.size() == 0 && "Must apply all transformations");

    getDerived().finalize(Root);

    return Root;
  }
};

} // namespace clang
#endif /* LLVM_CLANG_ANALYSIS_TRANSFORMEDTREE_H */
