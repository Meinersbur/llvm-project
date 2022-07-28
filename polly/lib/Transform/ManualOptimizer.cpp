//===------ ManualOptimizer.cpp -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Handle pragma/metadata-directed transformations.
//
//===----------------------------------------------------------------------===//

#include "polly/ManualOptimizer.h"
#include "polly/CodeGen/CodeGeneration.h"
#include "polly/DependenceInfo.h"
#include "polly/LinkAllPasses.h"
#include "polly/ManualOptimizer.h"
#include "polly/Options.h"
#include "polly/ScheduleOptimizer.h"
#include "polly/ScheduleTreeTransform.h"
#include "polly/ScopInfo.h"
#include "polly/ScopPass.h"
#include "polly/Simplify.h"
#include "polly/Support/GICHelper.h"
#include "polly/Support/ISLFuncs.h"
#include "polly/Support/ISLOStream.h"
#include "polly/Support/ISLTools.h"
#include "polly/Support/ScopHelper.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/Sequence.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Metadata.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/LoopUtils.h"
#include "isl/ctx.h"
#include "isl/options.h"
#include "isl/printer.h"
#include "isl/schedule.h"
#include "isl/schedule_node.h"
#include "isl/union_map.h"
#include "isl/union_set.h"
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <string>
#include <tuple>
#include <vector>

#define DEBUG_TYPE "polly-opt-manual"

using namespace polly;
using namespace llvm;

namespace {

static cl::opt<bool> IgnoreDepcheck(
    "polly-pragma-ignore-depcheck",
    cl::desc("Skip the dependency check for pragma-based transformations"),
    cl::cat(PollyCategory));

/// Same as llvm::hasUnrollTransformation(), but takes a LoopID as argument
/// instead of a Loop.
static TransformationMode hasUnrollTransformation(MDNode *LoopID) {
  if (getBooleanLoopAttribute(LoopID, "llvm.loop.unroll.disable"))
    return TM_SuppressedByUser;

  Optional<int> Count =
      getOptionalIntLoopAttribute(LoopID, "llvm.loop.unroll.count");
  if (Count)
    return Count.value() == 1 ? TM_SuppressedByUser : TM_ForcedByUser;

  if (getBooleanLoopAttribute(LoopID, "llvm.loop.unroll.enable"))
    return TM_ForcedByUser;

  if (getBooleanLoopAttribute(LoopID, "llvm.loop.unroll.full"))
    return TM_ForcedByUser;

  if (hasDisableAllTransformsHint(LoopID))
    return TM_Disable;

  return TM_Unspecified;
}

// Return the first DebugLoc in the list.
static DebugLoc findFirstDebugLoc(MDNode *MD) {
  if (MD) {
    for (const MDOperand &X : drop_begin(MD->operands(), 1)) {
      Metadata *A = X.get();
      if (!isa<DILocation>(A))
        continue;
      return cast<DILocation>(A);
    }
  }

  return {};
}

static DebugLoc findTransformationDebugLoc(MDNode *LoopMD, StringRef Name) {
  // First find dedicated transformation location
  // (such as the location of #pragma clang loop)
  MDNode *MD = findOptionMDForLoopID(LoopMD, Name);
  if (DebugLoc K = findFirstDebugLoc(MD))
    return K;

  // Otherwise, fall back to the location of the loop itself
  return findFirstDebugLoc(LoopMD);
}

static llvm::Optional<int> findOptionalIntOperand(MDNode *LoopMD,
                                                  StringRef Name) {
  Metadata *AttrMD = findMetadataOperand(LoopMD, Name).getValueOr(nullptr);
  if (!AttrMD)
    return None;

  ConstantInt *IntMD = mdconst::extract_or_null<ConstantInt>(AttrMD);
  if (!IntMD)
    return None;

  return IntMD->getSExtValue();
}

/// Extract boolean property from an LoopID metadata node.
static llvm::Optional<bool> findOptionalBoolOperand(MDNode *LoopMD,
                                                    StringRef Name) {
  auto MD = findOptionMDForLoopID(LoopMD, Name);
  if (!MD)
    return None;

  switch (MD->getNumOperands()) {
  case 1:
    // When the value is absent it is interpreted as 'attribute set'.
    return true;
  case 2:
    ConstantInt *IntMD =
        mdconst::extract_or_null<ConstantInt>(MD->getOperand(1).get());
    return IntMD->getZExtValue() != 0;
  }
  llvm_unreachable("unexpected number of options");
}

static DebugLoc findOptionalDebugLoc(MDNode *LoopMD, StringRef Name) {
  auto MD = findOptionMDForLoopID(LoopMD, Name);
  if (!MD)
    return DebugLoc();

  // NOTE: .loc attributes can also have a second DebugLoc, in which case it is
  // the end of the SourceRange
  if (MD->getNumOperands() < 2)
    return DebugLoc();
  Metadata *AttrMD = MD->getOperand(1).get();

  auto StrMD = cast<DILocation>(AttrMD);
  return StrMD;
}

template <typename Derived, typename... Args>
struct ScheduleNodeRewriteVisitor
    : public RecursiveScheduleTreeVisitor<Derived, isl::schedule_node,
                                          Args...> {
  using BaseTy =
      RecursiveScheduleTreeVisitor<Derived, isl::schedule_node, Args...>;

  BaseTy &getBase() { return *this; }
  const BaseTy &getBase() const { return *this; }
  Derived &getDerived() { return *static_cast<Derived *>(this); }
  const Derived &getDerived() const {
    return *static_cast<const Derived *>(this);
  }

  isl::schedule_node visitNode(const isl::schedule_node &Node, Args... args) {
    if (!Node.has_children())
      return Node;

    auto Child = Node.first_child();
    while (true) {
      Child = getDerived().visit(Child, args...);
      if (!Child.has_next_sibling())
        return Child.parent();
      Child = Child.next_sibling();
    }
  }
};

template <typename Derived, typename... Args>
struct MarkRemover : public ScheduleNodeRewriteVisitor<Derived, Args...> {
  using BaseTy = ScheduleNodeRewriteVisitor<Derived, Args...>;

  BaseTy &getBase() { return *this; }
  const BaseTy &getBase() const { return *this; }
  Derived &getDerived() { return *static_cast<Derived *>(this); }
  const Derived &getDerived() const {
    return *static_cast<const Derived *>(this);
  }

  isl::schedule_node visitMark(const isl::schedule_node &Mark, Args... args) {
    auto OneRemoved = isl::manage(isl_schedule_node_delete(Mark.copy()));
    auto Result = getDerived().visit(OneRemoved, args...);
    assert(!Result.is_null());
    return Result;
  }
};

struct MarkRemoverPlain : public MarkRemover<MarkRemoverPlain> {};

static isl::schedule_node moveToBandMark(isl::schedule_node Band) {
  auto Cur = Band;
  if (isBand(Band))
    Cur = Band.parent();

  // Go up until we find a band mark.
  while (true) {
    if (isl_schedule_node_get_type(Cur.get()) != isl_schedule_node_mark)
      break;
    if (isBandMark(Cur))
      return Cur;

    auto Parent = Cur.parent();
    assert(!Parent.is_null());
    Cur = Parent;
  }
  if (isBand(Band))
    return Band; // Has no mark.
  return {};
}

// FIXME: What is the difference of returning nullptr vs None?
static llvm::Optional<MDNode *> findOptionalMDOperand(MDNode *LoopMD,
                                                      StringRef Name) {
  Metadata *AttrMD = findMetadataOperand(LoopMD, Name).getValueOr(nullptr);
  if (!AttrMD)
    return None;

  auto MD = dyn_cast<MDNode>(AttrMD);
  if (!MD)
    return None;
  return MD;
}

static isl::schedule_node removeMark(isl::schedule_node MarkOrBand) {
  MarkOrBand = moveToBandMark(MarkOrBand);
  while (isl_schedule_node_get_type(MarkOrBand.get()) ==
         isl_schedule_node_mark) {
    if (isBandMark(MarkOrBand))
      MarkOrBand = isl::manage(isl_schedule_node_delete(MarkOrBand.release()));
    else
      MarkOrBand = MarkOrBand.parent();
  }
  return MarkOrBand;
}

static llvm::Optional<StringRef> findOptionalStringOperand(MDNode *LoopMD,
                                                           StringRef Name) {
  Metadata *AttrMD = findMetadataOperand(LoopMD, Name).getValueOr(nullptr);
  if (!AttrMD)
    return None;

  auto StrMD = dyn_cast<MDString>(AttrMD);
  if (!StrMD)
    return None;

  return StrMD->getString();
}

static isl::id makeTransformLoopId(isl::ctx Ctx, MDNode *FollowupLoopMD,
                                   StringRef TransName,
                                   StringRef Name = StringRef()) {
  // TODO: Deprecate Name
  // TODO: Only return one when needed.
  // TODO: If no FollowupLoopMD provided, derive attributes heuristically

  BandAttr *Attr = new BandAttr();

  auto GivenName = findOptionalStringOperand(FollowupLoopMD, "llvm.loop.id")
                       .getValueOr(StringRef());
  if (GivenName.empty())
    GivenName = Name;
  if (GivenName.empty())
    GivenName =
        TransName; // TODO: Don't use trans name as LoopName, but as label
  Attr->LoopName = GivenName.str();
  Attr->Metadata = FollowupLoopMD;
  // TODO: Inherit properties if 'FollowupLoopMD' (followup) is not used
  // TODO: Set followup MDNode
  return getIslLoopAttr(Ctx, Attr);
}

static isl::schedule_node insertMark(isl::schedule_node Band, isl::id Mark) {
  assert(isl_schedule_node_get_type(Band.get()) == isl_schedule_node_band);
  assert(moveToBandMark(Band).is_equal(Band) &&
         "Don't add a two marks for a band");
  Band = isl::manage(
      isl_schedule_node_insert_mark(Band.release(), Mark.release()));
  return Band.child(0);
}

static isl::schedule applyLoopReversal(MDNode *LoopMD,
                                       isl::schedule_node BandToReverse) {
  assert(!BandToReverse.is_null());
  auto IslCtx = BandToReverse.ctx();

  auto Followup =
      findOptionalMDOperand(LoopMD, "llvm.loop.reverse.followup_reversed")
          .getValueOr(nullptr);

  BandToReverse = moveToBandMark(BandToReverse);
  BandToReverse = removeMark(BandToReverse);

  auto PartialSched = isl::manage(
      isl_schedule_node_band_get_partial_schedule(BandToReverse.get()));
  assert(PartialSched.dim(isl::dim::out) == 1);

  auto MPA = PartialSched.at(0);
  auto Neg = MPA.neg();

  auto Node = isl::manage(isl_schedule_node_delete(BandToReverse.copy()));
  Node = Node.insert_partial_schedule(Neg);

  if (Followup) {
    auto NewBandId = makeTransformLoopId(IslCtx, Followup, "reversed");
    Node = insertMark(Node, NewBandId);
  }

  return Node.get_schedule();
}

class LoopIdentification {
  Loop *ByLoop = nullptr;
  isl::id ByIslId;
  std::string ByName;
  MDNode *ByMetadata = nullptr;

public:
  Loop *getLoop() const {
    if (ByLoop)
      return ByLoop;
    if (!ByIslId.is_null()) {
      BandAttr *Attr = static_cast<BandAttr *>(ByIslId.get_user());
      return Attr->OriginalLoop;
    }
    if (ByMetadata) {
      // llvm_unreachable("TODO: Implement lookup metadata-to-loop");
    }
    return nullptr;
  }

  isl::id getIslId() const { return ByIslId; }

  isl::id getIslId(isl::ctx &Ctx) const {
    auto Result = ByIslId;
    if (Result.is_null()) {
      if (auto L = getLoop())
        Result = getIslLoopAttr(Ctx, L);
    }
    return Result;
  }

  StringRef getName() const {
    if (!ByName.empty())
      return ByName;
    if (!ByIslId.is_null()) {
      BandAttr *Attr = static_cast<BandAttr *>(ByIslId.get_user());
      return Attr->LoopName;
    }
    StringRef Result;
    if (auto L = getLoop()) {
      auto IdVal = findStringMetadataForLoop(L, "llvm.loop.id");
      if (IdVal)
        Result = cast<MDString>(IdVal.getValue()->get())->getString();
    }
    assert(!ByMetadata && "TODO: extract llvm.loop.id directly from Metadata");
    return Result;
  }

  MDNode *getMetadata() const {
    if (ByMetadata)
      return ByMetadata;
    if (auto L = getLoop())
      return L->getLoopID();

    if (!ByIslId.is_null()) {
      BandAttr *Attr = static_cast<BandAttr *>(ByIslId.get_user());
      return Attr->Metadata;
    }

    return nullptr;
  }

  static LoopIdentification createFromLoop(Loop *L) {
    assert(L);
    LoopIdentification Result;
    Result.ByLoop = L;
    Result.ByMetadata = L->getLoopID();
#if 0
     if (Result.ByMetadata) {
       auto IdVal = findStringMetadataForLoop(L, "llvm.loop.id");
       if (IdVal) 
         Result.ByName = cast<MDString>(IdVal.getValue()->get())->getString();
     }
#endif
    return Result;
  }

  static LoopIdentification createFromIslId(isl::id Id) {
    assert(!Id.is_null());
    LoopIdentification Result;
    Result.ByIslId = Id;
    if (!Id.is_null()) {
      Result.ByLoop = static_cast<BandAttr *>(Id.get_user())->OriginalLoop;
      Result.ByName = static_cast<BandAttr *>(Id.get_user())->LoopName;
      Result.ByMetadata = static_cast<BandAttr *>(Id.get_user())->Metadata;
    }
    // Result.ByName = Id.get_name();
    // Result.ByMetadata = Result.ByLoop->getLoopID();
#if 0
     if (Result.ByMetadata) {
       auto IdVal = findStringMetadataForLoop(Result.ByLoop, "llvm.loop.id");
       if (IdVal) 
         Result.ByName = cast<MDString>(IdVal.getValue())->getString();
     }
#endif
    return Result;
  }

  static LoopIdentification createFromMetadata(MDNode *Metadata) {
    assert(Metadata);

    LoopIdentification Result;
    Result.ByMetadata = Metadata;
    return Result;
  }

  static LoopIdentification createFromName(StringRef Name) {
    assert(!Name.empty());

    LoopIdentification Result;
    Result.ByName = Name.str();
    return Result;
  }

  static LoopIdentification createFromBand(isl::schedule_node Band) {
    auto Marker = moveToBandMark(Band);
    assert(isl_schedule_node_get_type(Marker.get()) == isl_schedule_node_mark);
    // TODO: somehow get Loop id even if there is no marker
    return createFromIslId(Marker.as<isl::schedule_node_mark>().get_id());
  }
};

static isl::schedule_node ignoreMarkChild(isl::schedule_node Node) {
  assert(!Node.is_null());
  while (isl_schedule_node_get_type(Node.get()) == isl_schedule_node_mark) {
    assert(Node.n_children() == 1);
    Node = Node.child(0);
  }
  return Node;
}

static isl::schedule_node collapseBands(isl::schedule_node FirstBand,
                                        int NumBands) {
  if (NumBands == 1)
    return ignoreMarkChild(FirstBand);

  assert(NumBands >= 2);
  // auto Ctx = FirstBand.get_ctx();
  SmallVector<isl::multi_union_pw_aff, 4> PartialMultiSchedules;
  SmallVector<isl::union_pw_aff, 4> PartialSchedules;
  isl::multi_union_pw_aff CombinedSchedule;

  FirstBand = moveToBandMark(FirstBand);

  int CollapsedBands = 0;
  int CollapsedLoops = 0;
  // assert(isl_schedule_node_get_type(FirstBand.get()) ==
  // isl_schedule_node_band);
  auto Band = FirstBand;

  while (CollapsedBands < NumBands) {
    while (isl_schedule_node_get_type(Band.get()) == isl_schedule_node_mark)
      Band = isl::manage(isl_schedule_node_delete(Band.release()));
    assert(isl_schedule_node_get_type(Band.get()) == isl_schedule_node_band);

    auto X =
        isl::manage(isl_schedule_node_band_get_partial_schedule(Band.get()));
    PartialMultiSchedules.push_back(X);

    if (!CombinedSchedule.is_null()) {
      CombinedSchedule = CombinedSchedule.flat_range_product(X);
    } else {
      CombinedSchedule = X;
    }

    for (auto i : seq<isl_size>(0, X.dim(isl::dim::out).release())) {
      auto Y = X.at(i);
      PartialSchedules.push_back(Y);
      CollapsedLoops += 1;
    }

    CollapsedBands += 1;

    Band = isl::manage(isl_schedule_node_delete(Band.release()));
  }

  // auto DomainSpace = PartialSchedules[0].get_space();
  // auto RangeSpace = isl::space(Ctx, 0, PartialSchedules.size());
  // auto Space = DomainSpace.map_from_domain_and_range(RangeSpace);

  Band = Band.insert_partial_schedule(CombinedSchedule);

  return Band;
}

// TODO: Use ScheduleTreeOptimizer::tileNode
static isl::schedule_node tileBand(isl::schedule_node BandToTile,
                                   ArrayRef<int64_t> TileSizes) {
  auto Ctx = BandToTile.ctx();

  BandToTile = removeMark(BandToTile);

  auto Space = isl::manage(isl_schedule_node_band_get_space(BandToTile.get()));
  auto Sizes = isl::multi_val::zero(Space);
  for (auto i : seq<isl_size>(0, Space.dim(isl::dim::set).release())) {
    auto tileSize = TileSizes[i];
    Sizes = Sizes.set_val(i, isl::val(Ctx, tileSize));
  }

  auto Result = isl::manage(
      isl_schedule_node_band_tile(BandToTile.release(), Sizes.release()));
  return Result;
}

// TODO: Assign names to separated bands
static isl::schedule_node separateBand(isl::schedule_node Band) {
  auto NumDims = isl_schedule_node_band_n_member(Band.get());
  for (int i = NumDims - 1; i > 0; i -= 1) {
    Band = isl::manage(isl_schedule_node_band_split(Band.release(), i));
  }
  return Band;
}

static void collectVerticalLoops(const isl::schedule_node &TopBand,
                                 int MaxDepth,
                                 SmallVectorImpl<isl::schedule_node> &Bands) {
  isl::schedule_node Cur = TopBand;
  for (int i = 0; i < MaxDepth; i += 1) {
    while (true) {
      if (isBand(Cur))
        break;
      assert(Cur.n_children() == 1);
      Cur = Cur.first_child();
    }

    Bands.push_back(Cur);

    Cur = Cur.first_child();
  }
}

static std::tuple<isl::pw_aff_list, isl::pw_aff_list, isl::pw_aff_list>
extractExtends(isl::map Map) {
  auto Ctx = Map.ctx();
  auto Dims = Map.dim(isl::dim::out).release();
  auto IndexSpace = Map.get_space().range();
  auto LocalIndexSpace = isl::local_space(IndexSpace);

  isl::pw_aff_list DimMins = isl::pw_aff_list(Ctx, Dims);
  isl::pw_aff_list DimSizes = isl::pw_aff_list(Ctx, Dims);
  isl::pw_aff_list DimEnds = isl::pw_aff_list(Ctx, Dims);
  for (auto i = 0; i < Dims; i += 1) {
    auto TheDim = Map.project_out(isl::dim::out, i + 1, Dims - i - 1)
                      .project_out(isl::dim::out, 0, i);
    auto Min = TheDim.lexmin_pw_multi_aff().at(0);
    auto Max = TheDim.lexmax_pw_multi_aff().at(0);

    auto One = isl::aff(isl::local_space(Min.get_space().domain()),
                        isl::val(LocalIndexSpace.ctx(), 1));
    auto Len = Max.add(Min.neg()).add(One);
    auto End = Max.add(One);

    DimMins = DimMins.add(Min);
    DimSizes = DimSizes.add(Len);
    DimEnds = DimEnds.add(End);
  }

  return {DimMins, DimSizes, DimEnds};
}

template <typename Derived, typename... Args>
struct UniqueStmtRewriter
    : public RecursiveScheduleTreeVisitor<
          Derived, std::pair<isl::schedule, isl::union_map>, bool, Args...> {
  using BaseTy = RecursiveScheduleTreeVisitor<
      Derived, std::pair<isl::schedule, isl::union_map>, bool, Args...>;
  using RetTy = std::pair<isl::schedule, isl::union_map>;

  isl::schedule_node NodeToUnique;

  UniqueStmtRewriter(isl::schedule_node NodeToUnique)
      : NodeToUnique(NodeToUnique) {
    // assert(isl_schedule_node_get_type(NodeToUnique.get()) ==
    // isl_schedule_node_leaf; );
  }

  BaseTy &getBase() { return *this; }
  const BaseTy &getBase() const { return *this; }
  Derived &getDerived() { return *static_cast<Derived *>(this); }
  const Derived &getDerived() const {
    return *static_cast<const Derived *>(this);
  }

  RetTy visit(const isl::schedule_node &Node, bool DoUniqueSubtree,
              Args... args) {
    DoUniqueSubtree = DoUniqueSubtree || Node.is_equal(NodeToUnique);
    return getBase().visit(Node, DoUniqueSubtree, args...);
  }

  isl::schedule visit(const isl::schedule &Schedule, Args... args) {
    return getBase().visit(Schedule, false, args...).first;
  }

  RetTy visitLeaf(const isl::schedule_node &Leaf, bool DoUniqueSubtree,
                  Args... args) {
    auto Domain = Leaf.get_domain();
    if (!DoUniqueSubtree) {
      auto IdMap = makeIdentityMap(Domain, true);
      auto LeafSched = isl::schedule::from_domain(Domain);
      return {LeafSched, IdMap};
      // return getBase().visitLeaf(Leaf, DoUniqueSubtree, args...);//
      // isl::schedule::from_domain(Domain);
    }

    auto SetList = Domain.get_set_list();
    auto ParamSpace = Domain.get_space();
    auto IdMap = emptyUMap(ParamSpace);
    auto Result = emptyUSet(ParamSpace);
    for (auto Dom : SetList) {
      simplify(Dom);
      auto Id = Dom.get_space().get_tuple_id(isl::dim::set);
      auto Stmt = static_cast<ScopStmt *>(Id.get_user());
      auto S = Stmt->getParent();
      auto OldDomainSpace = Dom.get_space();

      auto NewStmt = S->addClonedStmt(Stmt, Dom);

      // Remove domain of clone from old stmt. This assumes that each instance
      // is scheduled at most once. AFAIK isl does not allow scheduling the same
      // instances multiple times in the schedule tree.
      auto NewDomain = Stmt->getDomain().subtract(Dom);
      simplify(NewDomain);
      Stmt->setDomain(NewDomain);

      auto ClonedDomain = NewStmt->getDomain();
      Result = Result.unite(ClonedDomain);
      IdMap = IdMap.unite(
          isl::map::identity(OldDomainSpace.map_from_domain_and_range(
                                 ClonedDomain.get_space()))
              .intersect_range(ClonedDomain));
    }

    auto NewChildNode = isl::schedule::from_domain(Result);
    return {NewChildNode, IdMap};
  }

  RetTy visitDomain(const isl::schedule_node &Domain, bool DoUniqueSubtree,
                    Args... args) {
    return getDerived().visit(Domain.child(0), DoUniqueSubtree, args...);
  }

  RetTy visitBand(const isl::schedule_node &Band, bool DoUniqueSubtree,
                  Args... args) {
    // TODO: apply band properties (coincident, permutable)
    auto PartialSched =
        isl::manage(isl_schedule_node_band_get_partial_schedule(Band.get()));
    auto ChildResult =
        getDerived().visit(Band.child(0), DoUniqueSubtree, args...);
    auto NewSchedule = ChildResult.first;
    auto NewMap = ChildResult.second;

    auto UNewPartialSched = isl::union_map::from(PartialSched);
    UNewPartialSched = UNewPartialSched.apply_domain(NewMap);
    auto NewPartialSched =
        isl::multi_union_pw_aff::from_union_map(UNewPartialSched);

    auto ResultSchedule = NewSchedule.insert_partial_schedule(NewPartialSched);
    return {ResultSchedule, NewMap};
  }

  RetTy visitSequence(const isl::schedule_node &Sequence, bool DoUniqueSubtree,
                      Args... args) {
    auto NumChildren = isl_schedule_node_n_children(Sequence.get());
    assert(NumChildren >= 1);

    auto FirstChildResult =
        getDerived().visit(Sequence.child(0), DoUniqueSubtree, args...);
    auto NewNode = FirstChildResult.first;
    auto NewMap = FirstChildResult.second;
    for (int i = 1; i < NumChildren; i += 1) {
      auto ChildResult =
          getDerived().visit(Sequence.child(i), DoUniqueSubtree, args...);
      NewNode = NewNode.sequence(ChildResult.first);
      NewMap = NewMap.unite(ChildResult.second);
    }
    return {NewNode, NewMap};
  }

  RetTy visitMark(const isl::schedule_node &Mark, bool DoUniqueSubtree,
                  Args... args) {
    auto TheMark = Mark.as<isl::schedule_node_mark>().get_id();
    auto ChildResult =
        getDerived().visit(Mark.child(0), DoUniqueSubtree, args...);

    auto NewChild = ChildResult.first;
    auto NewMap = ChildResult.second;
    auto NewSchedule =
        NewChild.get_root().child(0).insert_mark(TheMark).get_schedule();
    return {NewSchedule, NewMap};
  }

  RetTy visitFilter(const isl::schedule_node &Filter, bool DoUniqueSubtree,
                    Args... args) {
    auto FilterDomain = Filter.as<isl::schedule_node_filter>().get_filter();
    auto ChildResult =
        getDerived().visit(Filter.child(0), DoUniqueSubtree, args...);

    auto NewMap = ChildResult.second.intersect_domain(FilterDomain);
    auto NewFilterDomain = NewMap.range();
    auto NewSchedule = ChildResult.first.intersect_domain(NewFilterDomain);
    return {NewSchedule, NewMap};
  }

  RetTy visitOther(const isl::schedule_node &Other, bool DoUniqueSubtree,
                   Args... args) {
    llvm_unreachable("Not implemented");
  }
};

class UniqueStmtRewriterPlain
    : public UniqueStmtRewriter<UniqueStmtRewriterPlain> {
public:
  UniqueStmtRewriterPlain(isl::schedule_node NodeToUnique)
      : UniqueStmtRewriter(NodeToUnique) {}
};

static isl::schedule applyLoopTiling(MDNode *LoopMD,
                                     const isl::schedule_node &TopBand) {
  auto IslCtx = TopBand.ctx();

  auto Depth =
      findOptionalIntOperand(LoopMD, "llvm.loop.tile.depth").getValueOr(0);
  assert(Depth >= 1);

  SmallVector<isl::schedule_node, 4> Bands;
  collectVerticalLoops(TopBand, Depth, Bands);
  assert((size_t)Depth == Bands.size());

  SmallVector<BandAttr *, 4> Attrs;
  SmallVector<int64_t, 4> TileSizes;
  SmallVector<MDNode *, 4> FloorIds;
  SmallVector<MDNode *, 4> TileIds;
  for (const auto &Band : Bands) {
    // auto Mark = moveToBandMark(Band);
    auto Attr = getBandAttr(Band);
    Attrs.push_back(Attr);

    auto Size = 0;
    // StringRef FloorId, TileId;
    if (Attr) {
      Size = findOptionalIntOperand(Attr->Metadata, "llvm.loop.tile.size")
                 .getValueOr(0);
      // FloorId = findOptionalStringOperand(Attr->Metadata, "llvm.")
      auto FloorId =
          findOptionalMDOperand(Attr->Metadata, "llvm.loop.tile.followup_floor")
              .getValueOr(nullptr);
      FloorIds.push_back(FloorId);
      auto TileId =
          findOptionalMDOperand(Attr->Metadata, "llvm.loop.tile.followup_tile")
              .getValueOr(nullptr);
      TileIds.push_back(TileId);
    }
    TileSizes.push_back(Size);
  }

  auto Peel = findOptionalStringOperand(getBandAttr(Bands[0])->Metadata,
                                        "llvm.loop.tile.peel")
                  .getValueOr("");
  auto RectangularPeel = (Peel == "rectangular");

  auto TheCollapsedBand = collapseBands(TopBand, Depth);
  auto TheBand = tileBand(TheCollapsedBand, TileSizes);

  auto OuterBand = TheBand;
  auto InnerBand = TheBand.child(0);

  InnerBand = separateBand(InnerBand);
  for (auto TileId : TileIds) {
    // TODO: Merge TileId
    auto Mark = makeTransformLoopId(IslCtx, TileId, "inner tile");
    InnerBand = insertMark(InnerBand, Mark);

    InnerBand = InnerBand.child(0);
  }

  // Jump back to first of the tile loops
  for (int i = TileIds.size(); i >= 1; i -= 1) {
    InnerBand = InnerBand.parent();
    InnerBand = moveToBandMark(InnerBand);
  }

  OuterBand = InnerBand.parent();

  OuterBand = separateBand(OuterBand);
  for (auto PitId : FloorIds) {
    // TODO: Merge PitId
    auto Mark = makeTransformLoopId(IslCtx, PitId, "outer floor");
    OuterBand = insertMark(OuterBand, Mark);

    OuterBand = OuterBand.child(0);
  }

  // Jump back to first of the pit loops
  for (int i = FloorIds.size(); i >= 1; i -= 1) {
    OuterBand = OuterBand.parent();
    OuterBand = moveToBandMark(OuterBand);
  }

  // Extract non-full tiles.
  if (RectangularPeel) {
    // union_pw_multi_aff
    auto Partial = isl::manage(
        isl_schedule_node_band_get_partial_schedule(TheCollapsedBand.get()));
    auto Domains = TheCollapsedBand.get_domain();

    auto Space = isl::space(IslCtx, 0, 1);
    auto LSpace = isl::local_space(Space);

    auto InsideDomains = Domains;

    // TODO: This assumes the entire floor is rectangular; however,
    // non-rectangular floors still can have full tiles.
    for (int i = 0; i < Depth; i += 1) {
      auto TileSize = TileSizes[i];
      auto Size = isl::aff(LSpace, isl::val(IslCtx, TileSize));
      auto SizeMinusOne = isl::aff(LSpace, isl::val(IslCtx, TileSize - 1));

      // { Domain[] -> Schedule[] }
      auto DimPartial = Partial.at(i);
      auto Sched = DimPartial.intersect_domain(Domains);
      auto SchedMap = isl::convert<isl::union_map>(Sched);
      auto SchedSpace = isl::set(SchedMap.range());

      isl::pw_aff_list DimMins;
      isl::pw_aff_list DimSizes;
      isl::pw_aff_list DimEnds;
      std::tie(DimMins, DimSizes, DimEnds) =
          extractExtends(isl::map::from_range(SchedSpace));
      auto DimMin = DimMins.at(0).add_dims(isl::dim::in, 1);
      auto DimEnd = DimEnds.at(0).add_dims(isl::dim::in, 1);

      auto EndFloor = DimEnd.div(Size).floor().mul(Size);
      auto MinCeil = DimMin.add(SizeMinusOne).div(Size).floor().mul(Size);

      auto I = isl::pw_aff::var_on_domain(LSpace, isl::dim::set, 0);
      auto UpperBound = I.lt_set(EndFloor);
      auto LowerBound = I.ge_set(MinCeil);

      auto Bounded = LowerBound.intersect(UpperBound);

      auto BoundedDoms = SchedMap.intersect_range(Bounded);
      auto InsideDoms = BoundedDoms.domain();
      InsideDomains = InsideDomains.intersect(InsideDoms);
    }

    auto OuterDomains = Domains.subtract(InsideDomains);
    // Empty OuterDomains means it was already rectangular.
    if (!OuterDomains.is_empty()) {
      auto Filters = isl::union_set_list(IslCtx, 2);
      Filters = Filters.add(InsideDomains);
      Filters = Filters.add(OuterDomains);
      auto InnerAndOuter = OuterBand.insert_sequence(Filters);

      MarkRemoverPlain MarkCleaner;
      auto Peeled = MarkCleaner.visit(InnerAndOuter.child(1));

      UniqueStmtRewriterPlain Visitor(Peeled);
      auto Result = Visitor.visit(Peeled.get_schedule());

      return Result;
    }
  }

  auto Transformed = OuterBand.get_schedule();

#if 0
   if (!D.isValidSchedule(S, Transformed)) {
     LLVM_DEBUG(dbgs() << "LoopReversal not semantically legal\n");
     return;
   }
#endif

  return Transformed;
}

static isl::schedule_node removeBandAndMarks(isl::schedule_node MarkOrBand) {
  while (true) {
    auto Parent = MarkOrBand.parent();
    if (isl_schedule_node_get_type(Parent.get()) != isl_schedule_node_mark)
      break;
    MarkOrBand = Parent;
  }

  while (true) {
    auto RemovedKind = isl_schedule_node_get_type(MarkOrBand.get());
    MarkOrBand = isl::manage(isl_schedule_node_delete(MarkOrBand.release()));
    if (RemovedKind != isl_schedule_node_mark)
      break;
  }
  return MarkOrBand;
}

static isl::schedule applyLoopInterchange(MDNode *LoopMD,
                                          const isl::schedule_node &TopBand) {
  auto IslCtx = TopBand.ctx();

  auto Depth = findOptionalIntOperand(LoopMD, "llvm.loop.interchange.depth")
                   .getValueOr(0);
  assert(Depth >= 2);

  SmallVector<isl::schedule_node, 4> Bands;
  collectVerticalLoops(TopBand, Depth, Bands);
  assert((size_t)Depth == Bands.size());

  SmallVector<isl::schedule_node, 4> NewOrder;
  NewOrder.resize(Depth);
  auto PermMD =
      findOptionMDForLoopID(LoopMD, "llvm.loop.interchange.permutation");
  int i = 0;
  for (auto &X : drop_begin(PermMD->operands(), 1)) {
    ConstantInt *IntMD = mdconst::extract_or_null<ConstantInt>(X.get());
    auto Pos = IntMD->getSExtValue();
    NewOrder[Pos] = Bands[i];
    i += 1;
  }
  assert(NewOrder.size() == Bands.size());

  // Remove old order
  auto Band = TopBand;
  for (int i = 0; i < Depth; i += 1)
    Band = removeBandAndMarks(Band);

  // Rebuild loop nest bottom-up according to new order.
  for (auto &OldBand : reverse(NewOrder)) {
    auto Attr = getBandAttr(OldBand);
    auto FollowUp =
        findOptionalMDOperand(Attr->Metadata,
                              "llvm.loop.interchange.followup_interchanged")
            .getValueOr(nullptr);

    // auto OldBand =   findBand(OldBands, NewBandId);
    // assert(OldBand);
    // TODO: Check that no band is used twice
    // auto OldMarker = LoopIdentification::createFromBand(OldBand);
    auto TheOldBand = ignoreMarkChild(OldBand);
    auto TheOldSchedule = isl::manage(
        isl_schedule_node_band_get_partial_schedule(TheOldBand.get()));

    auto Marker = makeTransformLoopId(IslCtx, FollowUp, "interchange");

    Band = Band.insert_partial_schedule(TheOldSchedule);
    Band = Band.insert_mark(Marker);
  }

  return Band.get_schedule();
}

struct CollectInnerSchedules
    : public RecursiveScheduleTreeVisitor<CollectInnerSchedules, void,
                                          isl::multi_union_pw_aff> {
  using BaseTy = RecursiveScheduleTreeVisitor<CollectInnerSchedules, void,
                                              isl::multi_union_pw_aff>;
  BaseTy &getBase() { return *this; }
  const BaseTy &getBase() const { return *this; }
  using RetTy = void;

  isl::union_map InnerSched;

  CollectInnerSchedules(isl::space ParamSpace)
      : InnerSched(emptyUMap(ParamSpace)) {}

  RetTy visit(isl::schedule_node Band, isl::multi_union_pw_aff PostfixSched) {
    return getBase().visit(Band, PostfixSched);
  }

  RetTy visit(isl::schedule_node Band) {
    auto Ctx = Band.ctx();
    auto List = isl::union_pw_aff_list(Ctx, 0);
    auto Empty =
        isl::multi_union_pw_aff(Band.get_universe_domain().get_space(), List);
    return visit(Band, Empty);
  }

  RetTy visitBand(isl::schedule_node_band Band,
                  isl::multi_union_pw_aff PostfixSched) {
    // auto NumLoops = isl_schedule_node_band_n_member(Band.get());
    auto PartialSched =
        isl::manage(isl_schedule_node_band_get_partial_schedule(Band.get()));
    auto Sched = PostfixSched.flat_range_product(PartialSched);
    return getBase().visitBand(Band, Sched);
  }

  RetTy visitLeaf(isl::schedule_node_leaf Leaf,
                  isl::multi_union_pw_aff PostfixSched) {
    auto Dom = Leaf.get_domain();
    auto Sched = PostfixSched.intersect_domain(Dom);
    InnerSched = InnerSched.unite(isl::union_map::from(Sched));
  }
};

static bool isSameNode(const isl::schedule_node &Node1,
                       const isl::schedule_node &Node2) {
  return isl::manage(isl_schedule_node_is_equal(Node1.get(), Node2.get()));
}

/// Return the (one-dimensional) set of numbers that are divisible by @p Factor
/// with remainder @p Offset. isDivisibleBySet(Ctx, 4, 0) = { [i] : floord(i,4)
/// = 0 } isDivisibleBySet(Ctx, 4, 1) = { [i] : floord(i,4) = 1 }
static isl::basic_set isDivisibleBySet(isl::ctx &Ctx, int64_t Factor,
                                       int64_t Offset) {
  auto ValFactor = isl::val(Ctx, Factor);
  auto Unispace = isl::space(Ctx, 0, 1);
  auto LUnispace = isl::local_space(Unispace);
  auto Id = isl::aff::var_on_domain(LUnispace, isl::dim::out, 0);
  auto AffFactor = isl::aff(LUnispace, ValFactor);
  auto ValOffset = isl::val(Ctx, Offset);
  auto AffOffset = isl::aff(LUnispace, ValOffset);
  auto DivMul = Id.mod(ValFactor);
  auto Divisible = isl::basic_map::from_aff(
      DivMul); //.equate(isl::dim::in, 0, isl::dim::out, 0);
  auto Modulo = Divisible.fix_val(isl::dim::out, 0, ValOffset);
  return Modulo.domain();
}

static isl::schedule unrollAndOrJam(isl::schedule_node BandToUnroll,
                                    isl::schedule_node BandToJam, int Factor,
                                    bool Full, MDNode *UnrolledID,
                                    ArrayRef<MDNode *> IntermediateIDs,
                                    MDNode *JammedID) {
  // BandToJam must be perfectly inside BandToUnroll
  assert(!BandToUnroll.is_null());
  assert(isBand(BandToUnroll));
  assert(!BandToJam.is_null());
  assert(isBand(BandToJam));
  auto Ctx = BandToUnroll.ctx();

  int JamDepthInBands = 0;
  int JamDepthInNodes = 0;
  auto Node = BandToUnroll;
  while (true) {
    if (isSameNode(Node, BandToJam))
      break;
    assert(Node.n_children() == 1 && "Constraints not met");
    Node = Node.first_child();
    if (isBand(Node))
      JamDepthInBands += 1;
    JamDepthInNodes += 1;
  }

  if (Full) {
    llvm_unreachable("unimplemented");
  } else if (Factor > 0) {
    // TODO: Could also do a strip-mining, then full unroll

    auto PartialSchedToUnroll = isl::manage(
        isl_schedule_node_band_get_partial_schedule(BandToUnroll.get()));
    assert(PartialSchedToUnroll.dim(isl::dim::out) == 1);

    auto PartialSchedToJam = isl::manage(
        isl_schedule_node_band_get_partial_schedule(BandToJam.get()));
    assert(PartialSchedToJam.dim(isl::dim::out) == 1);

    // { Stmt[] -> [x] }
    auto PartialSchedToUnrollUAff = PartialSchedToUnroll.at(0);
    auto PartialSchedToJamUAff = PartialSchedToJam.at(0);

    // Unrolling...
    // FIXME: Here we assume the schedule stride is one and starts with 0, which
    // is not necessarily the case.
    auto StridedPartialSchedUAff =
        isl::union_pw_aff::empty(PartialSchedToUnrollUAff.get_space());
    auto ValFactor = isl::val(Ctx, Factor);
    PartialSchedToUnrollUAff.foreach_pw_aff(
        [&StridedPartialSchedUAff, &ValFactor](isl::pw_aff PwAff) -> isl::stat {
          auto Space = PwAff.get_space();
          auto Universe = isl::set::universe(Space.domain());
          auto AffFactor = isl::manage(
              isl_pw_aff_val_on_domain(Universe.copy(), ValFactor.copy()));
          auto DivSchedAff = PwAff.div(AffFactor).floor().mul(AffFactor);
          StridedPartialSchedUAff =
              StridedPartialSchedUAff.union_add(DivSchedAff);
          return isl::stat::ok();
        });

    // Jamming...
    auto List = isl::manage(isl_union_set_list_alloc(Ctx.get(), Factor));
    for (int i = 0; i < Factor; i += 1) {
      // { Stmt[] -> [x] }
      isl::union_map UMap = isl::convert<isl::union_map>(PartialSchedToUnroll);

      // { [x] }
      auto Divisible = isDivisibleBySet(Ctx, Factor, i);

      // { Stmt[] }
      auto UnrolledDomain = UMap.intersect_range(Divisible).domain();

      List = List.add(UnrolledDomain);
    }

    // Parent -> BandToUnroll -> IntermediateLoops... -> BandToJam -> Body  ->
    // ...
    auto IntermediateLoops =
        isl::manage(isl_schedule_node_delete(BandToUnroll.copy()));
    // Parent -> IntermediateLoops... -> BandToJam -> Body
    auto UnrolledLoop =
        IntermediateLoops.insert_partial_schedule(StridedPartialSchedUAff);
    // Parent -> UnrolledLoop -> IntermediateLoops... -> BandToJam -> Body ->
    // ...
    // TODO: Apply permutable, coincident property

    auto NewBandId = makeTransformLoopId(
        Ctx, UnrolledID,
        (JamDepthInBands == 0) ? "unrolled" : "unrolled-and-jam");
    if (!NewBandId.is_null())
      UnrolledLoop = insertMark(UnrolledLoop, NewBandId);

    IntermediateLoops = UnrolledLoop;
    int k = 0;
    for (int i = 0; i < JamDepthInNodes; i += 1) {
      IntermediateLoops = IntermediateLoops.first_child();

      if (i != JamDepthInNodes - 1 && isBand(IntermediateLoops)) {
        auto TheID = IntermediateIDs[k];
        k += 1;
        auto X = makeTransformLoopId(Ctx, TheID, "immediate");
        IntermediateLoops = removeMark(IntermediateLoops);
        if (!X.is_null()) {
          IntermediateLoops = insertMark(IntermediateLoops, X);
        }
      }
    }

    auto LoopToJam = IntermediateLoops;

    // TODO: This loop is not participating more in unroll-and-jam than the
    // intermediate loops. Just keep any mark, if existing?
    if (JamDepthInBands > 0) {
      auto NewJammedBandId = makeTransformLoopId(Ctx, JammedID, "jammed");
      LoopToJam = removeMark(LoopToJam);
      if (!NewJammedBandId.is_null())
        LoopToJam = insertMark(LoopToJam, NewJammedBandId);
    }

#if 0
    for (auto P : zip(IntermediateLoops, IntermediateIDs)) {
     auto Loop = std::get<0>(P);
     auto ID = std::get<1>(P);
     auto NewID = makeTransformLoopId(Ctx, ID, "intermediate");
     Loop = removeMark(Loop);
     if (!NewID.is_null())
       Loop = insertMark(Loop, NewID);
    }
#endif

    auto Body = LoopToJam.first_child();

    // Parent -> UnrolledLoop -> IntermediateLoops... -> BandToJam -> ...
    // This copies the body for each list elements, but inserts the list's
    // filter in-between.
    Body = Body.insert_sequence(List);
    // Parent -> UnrolledLoop -> IntermediateLoops... -> BandToJam -> Sequence
    // -> filters -> ...

    return Body.get_schedule();
  }

  llvm_unreachable("Negative unroll factor");
}

static void collectMemAccsDomains(isl::schedule_node Node,
                                  const ScopArrayInfo *SAI,
                                  isl::union_map &Result, bool Inclusive) {
  switch (isl_schedule_node_get_type(Node.get())) {
  case isl_schedule_node_leaf:
    if (Inclusive) {
      auto Doms = Node.get_domain();
      for (auto Dom : Doms.get_set_list()) {
        auto Stmt = reinterpret_cast<ScopStmt *>(Dom.get_tuple_id().get_user());
        for (auto MemAcc : *Stmt) {
          if (MemAcc->getLatestScopArrayInfo() != SAI)
            continue;

          auto AccDom = MemAcc->getLatestAccessRelation().intersect_domain(
              Stmt->getDomain());
          if (!Result.is_null())
            Result = Result.unite(AccDom);
          else
            Result = AccDom;
        }
      }
    }

    break;
  default:
    auto n = Node.n_children().release();
    for (auto i = 0; i < n; i += 1)
      collectMemAccsDomains(Node.child(i), SAI, Result, true);
    break;
  }
}

static void
collectSubtreeAccesses(isl::schedule_node Node, const ScopArrayInfo *SAI,
                       SmallVectorImpl<polly::MemoryAccess *> &Accs) {
  if (isl_schedule_node_get_type(Node.get()) == isl_schedule_node_leaf) {
    auto UDomain = Node.get_domain();
    for (auto Domain : UDomain.get_set_list()) {
      auto Space = Domain.get_space();
      auto Id = Domain.get_tuple_id();
      auto Stmt = reinterpret_cast<ScopStmt *>(Id.get_user());

      for (auto *MemAcc : *Stmt) {
        if (MemAcc->getLatestScopArrayInfo() != SAI)
          continue;

        Accs.push_back(MemAcc);
      }
    }
  }

  auto n = Node.n_children().release();
  for (auto i = 0; i < n; i += 1) {
    auto Child = Node.child(i);
    collectSubtreeAccesses(Child, SAI, Accs);
  }
}

static void collectStmtDomains(isl::schedule_node Node, isl::union_set &Result,
                               bool Inclusive) {
  switch (isl_schedule_node_get_type(Node.get())) {
  case isl_schedule_node_leaf:
    if (Inclusive) {
      auto Dom = Node.get_domain();
      if (!Result.is_null())
        Result = Result.unite(Dom);
      else
        Result = Dom;
    }
    break;
  default:
    auto n = Node.n_children().release();
    for (auto i = 0; i < n; i += 1)
      collectStmtDomains(Node.child(i), Result, true);
    break;
  }
}

/// @return { PrefixSched[] -> Domain[] }
static isl::union_map collectParentSchedules(isl::schedule_node Node) {
  auto Ctx = Node.ctx();
  auto ParamSpace = Node.get_universe_domain().get_space();
  isl::union_set Doms = emptyUSet(ParamSpace);
  collectStmtDomains(Node, Doms, false);

  // { [] -> Stmt[] }
  auto Result = isl::union_map::from_range(Doms);

  SmallVector<isl::schedule_node, 4> Ancestors;
  auto Anc = Node.parent();
  while (true) {
    Ancestors.push_back(Anc);
    if (!Anc.has_parent())
      break;
    Anc = Anc.parent();
  }

  auto N = Ancestors.size();
  for (int i = N - 1; i >= 0; i -= 1) {
    auto Ancestor = Ancestors[i];
    switch (isl_schedule_node_get_type(Ancestor.get())) {
    case isl_schedule_node_band: {

      // { Domain[] -> PartialSched[] }
      auto Partial = isl::union_map::from(isl::manage(
          isl_schedule_node_band_get_partial_schedule(Ancestor.get())));

      Result = flat_domain_product(Result, Partial.reverse());
    } break;
    case isl_schedule_node_sequence: {
      auto PrevNode = Ancestors[i - 1];
      auto Pos = PrevNode.get_child_position().release();
      auto Domain = PrevNode.get_domain();

      auto LS = isl::local_space(isl::space(Ctx, 0, 0));
      auto Aff = isl::aff(LS, isl::val(Ctx, Pos));
      auto C = isl::basic_map::from_aff(Aff);
      auto S = C.range();
      auto M = isl::union_map::from_domain_and_range(S, Domain);

      Result = flat_domain_product(Result, M);
    } break;
    case isl_schedule_node_set:
      break;
    case isl_schedule_node_filter:
    case isl_schedule_node_domain: // root node
    case isl_schedule_node_mark:
      break;
    default:
      llvm_unreachable("X");
    }
  }

  return Result;
}

static std::vector<unsigned int> sizeBox(isl::pw_aff_list DimSizes) {
  auto Dims = DimSizes.size().release();
  std::vector<unsigned int> PackedSizes;
  PackedSizes.reserve(Dims);
  for (auto i = 0; i < Dims; i += 1) {
    auto Len = DimSizes.at(i);
    Len = Len.coalesce();

    // FIXME: Because of the interfaces of Scop::createScopArrayInfo, array
    // sizes currently need to be constant
    auto SizeBound = polly::getConstant(Len, true, false);
    assert(!SizeBound.is_null());
    assert(!SizeBound.is_infty());
    assert(!SizeBound.is_nan());
    assert(SizeBound.is_pos());
    PackedSizes.push_back(SizeBound.get_num_si()); // TODO: Overflow check
  }
  return PackedSizes;
}

static std::tuple<isl::map, std::vector<unsigned int>>
readPackingLayout(isl::union_map InnerSchedules, isl::union_map InnerInstances,
                  isl::union_map Accs, isl::set IslSize, isl::map IslRedirect) {
  auto Ctx = InnerSchedules.ctx();

  // { PostfixSched[] -> Domain[] }
  // InnerSchedules;

  // { PrefixSched[] -> Domain[] }
  // InnerInstances;

  // { Domain[] -> Data[] }
  // Accs;

  // { [PrefixSched[] -> PostfixSched[]] -> Domain[] }
  auto CombinedInstances = InnerInstances.domain_product(InnerSchedules);

  // { [PrefixSched[] -> PostfixSched[]] -> Data[] }
  auto CombinedAccesses =
      isl::map::from_union_map(CombinedInstances.apply_range(Accs));
  auto PrefixSchedSpace =
      CombinedAccesses.get_space().domain().unwrap().domain();
  auto PostfixSchedSpace =
      CombinedAccesses.get_space().domain().unwrap().range();
  auto DataSpace = CombinedAccesses.get_space().range();
  TupleNest CombinedAccessesNest(
      CombinedAccesses, "{ [PrefixSched[] -> PostfixSched[]] -> Data[] }");

  // { Packed[] }
  auto PackedId = isl::id::alloc(Ctx, "TmpPacked", nullptr);

  // { PrefixSched[] }
  auto PrefixDomain = singleton(InnerInstances.domain(), PrefixSchedSpace);

  // { Sizes[] }
  // IslSize;

  // { PrefixSched[] -> [Data[] -> Packed[]] }
  // IslRedirect;
  TupleNest IslRedirectNest(IslRedirect,
                            "{ PrefixSched[] -> [Data[] -> Packed[]] }");
  auto PackedSpace = IslRedirectNest["Packed"].Space;
  PackedSpace = PackedSpace.set_tuple_id(isl::dim::set, PackedId);
  PackedSpace = PackedSpace.align_params(DataSpace);

  auto Layout =
      castSpace(IslRedirect,
                PrefixSchedSpace.map_from_domain_and_range(
                    DataSpace.map_from_domain_and_range(PackedSpace).wrap()));
  assert(Layout.uncurry().is_single_valued() && "Target must be unique");
  TupleNest LayoutNest(Layout, "{ PrefixSched[] -> [Data[] -> Packed[]] }");

  // Restrict unbounded set
  Layout =
      Layout.uncurry()
          .intersect_domain(reverseDomain(CombinedAccesses).curry().range())
          .curry();

  std::vector<unsigned int> PackedSizes;
  if (!IslSize.is_null()) {
    auto SizeAff1 = isl::pw_multi_aff::from_map(isl::map::from_range(IslSize));
    auto SizeAff2 = isl::multi_pw_aff(SizeAff1);
    auto Dims = IslSize.dim(isl::dim::set).release();
    PackedSizes.resize(Dims);
    for (int i = 0; i < Dims; i += 1) {
      auto PwAff = SizeAff2.at(i);
      auto Size = polly::getConstant(PwAff, false, false);
      PackedSizes[i] = Size.get_num_si(); // TODO: Overflow check
    }
  } else {
    auto WorkingSet =
        rebuildNesting(Layout, "{ PrefixSched[] -> [Data[] -> Packed[]] }",
                       "{ PrefixSched[] -> Packed[] }");

    isl::pw_aff_list DimMins;
    isl::pw_aff_list DimSizes;
    isl::pw_aff_list DimEnds;
    std::tie(DimMins, DimSizes, DimEnds) = extractExtends(WorkingSet);

    PackedSizes = sizeBox(DimSizes);
  }

  return {Layout, PackedSizes};
}

static void negateCoeff(isl::constraint &C, isl::dim Dim) {
  auto N = dim(get_local_space(C), Dim).release();
  for (auto i = 0; i < N; i += 1) {
    auto V = get_coefficient_val(C, Dim, i);
    V = V.neg();
    C = set_coefficient_val(C, Dim, i, V);
  }
}

/// @param ScheduleToAccess { Schedule[] -> Data[] }
/// @param PackedSizes      Array to be reordered using the same permutation
///        Schedule[] is assumed to be left-aligned
static isl::basic_map
findDataLayoutPermutation(isl::union_map ScheduleToAccess,
                          std::vector<unsigned int> &PackedSizes) {
  // FIXME: return is not required to be a permutatation, any injective function
  // should work
  // TODO: We could apply this more generally on ever Polly-created array
  // (except pattern-based optmization which define their custom data layout)

  isl_size MaxSchedDims = 0;
  //   unsigned PackedDims = 0;
  for (auto Map : ScheduleToAccess.get_map_list()) {
    MaxSchedDims = std::max(MaxSchedDims, Map.dim(isl::dim::in).release());
    //     PackedDims = std::max(PackedDims, Map.dim(isl::dim::out));
  }

  auto PackedSpace = [&ScheduleToAccess]() -> isl::space {
    for (auto Map : ScheduleToAccess.get_map_list()) {
      return Map.get_space().range();
    }
    return {};
  }();
  assert(!PackedSpace.is_null());
  auto PackedDims = PackedSpace.dim(isl::dim::set).release();

  SmallVector<bool, 8> UsedDims;
  UsedDims.reserve(PackedDims);
  for (auto i = 0; i < PackedDims; i += 1) {
    UsedDims.push_back(false);
  }

  // { PackedData[] -> [] }
  auto Permutation = isl::basic_map::universe(from_domain(PackedSpace));
  SmallVector<unsigned int, 8> NewPackedSizes;
  NewPackedSizes.reserve(PackedDims); // reversed!

  // TODO: If schedule has been stripmined/tiled/unroll-and-jammed, also apply
  // on 'permutation'
  for (int i = MaxSchedDims - 1; i >= 0; i -= 1) {
    if (Permutation.dim(isl::dim::in).release() <=
        1 + Permutation.dim(isl::dim::out).release())
      break;

    for (auto Map : ScheduleToAccess.get_map_list()) {
      assert(PackedDims == Map.dim(isl::dim::out));

      auto SchedDims = Map.dim(isl::dim::in).release();
      if (SchedDims <= i)
        continue;

      // { PackedData[] -> [i] }
      auto ExtractPostfix = isolateDim(Map.reverse(), i);
      simplify(ExtractPostfix);

      SmallVector<isl::constraint, 32> Constraints;
      for (auto BMap : ExtractPostfix.get_basic_map_list()) {
        for (auto C : get_constraint_list(BMap)) {
          if (!isl_constraint_is_equality(C.get()))
            continue;

          auto Coeff = get_coefficient_val(C, isl::dim::out, 0);
          if (Coeff.is_zero())
            continue;

          // Normalize coefficients
          if (Coeff.is_pos()) {
            auto Cons = get_constant_val(C);
            Cons = Cons.neg();
            C = C.set_constant_val(Cons);
            negateCoeff(C, isl::dim::param);
            negateCoeff(C, isl::dim::in);
            negateCoeff(C, isl::dim::out);
            negateCoeff(C, isl::dim::div);
          }

          Constraints.push_back(C);
        }
      }

      SmallVector<int, 8> Depends;
      Depends.reserve(PackedDims);
      for (auto i = 0; i < PackedDims; i += 1) {
        Depends.push_back(0);
      }

      for (auto C : Constraints) {
        for (auto i : seq<isl_size>(0, PackedDims)) {
          auto Coeff = get_coefficient_val(C, isl::dim::in, i);
          if (Coeff.is_zero())
            continue;

          auto &Dep = Depends[i];
          if (Dep > 0)
            continue;

          Dep = Coeff.cmp_si(0);
        }
      }

      auto FindFirstDep = [&Depends, PackedDims, &UsedDims]() -> int {
        for (int i = PackedDims - 1; i >= 0; i -= 1) {
          if (UsedDims[i])
            continue;

          // TODO: If Depends[i] is negative, also reverse order in this
          // dimension
          if (Depends[i])
            return i;
        }
        return -1;
      };

      auto ChosenDim = FindFirstDep();
      if (ChosenDim < 0)
        continue;
      UsedDims[ChosenDim] = true;

      // { PackedSpace[] -> [ChosenDim] }
      auto TheDim = isolateDim(
          identity_map(PackedSpace.map_from_domain_and_range(PackedSpace)),
          ChosenDim);

      Permutation = flat_range_product(TheDim, Permutation);
      NewPackedSizes.push_back(PackedSizes[ChosenDim]);
    }
  }

  // Add all remaining dimensions in original order
  for (int i = PackedDims - 1; i >= 0; i -= 1) {
    if (UsedDims[i])
      continue;

    auto TheDim = isolateDim(
        identity_map(PackedSpace.map_from_domain_and_range(PackedSpace)), i);
    Permutation = flat_range_product(TheDim, Permutation);
    NewPackedSizes.push_back(PackedSizes[i]);
  }

  assert(Permutation.dim(isl::dim::in) == PackedDims);
  assert(Permutation.dim(isl::dim::in) == PackedDims);
  assert(NewPackedSizes.size() == (size_t)PackedDims);

  Permutation = castSpace(Permutation,
                          PackedSpace.map_from_domain_and_range(PackedSpace));
  for (int i = 0; i < PackedDims; i += 1)
    PackedSizes[i] = NewPackedSizes[PackedDims - i - 1];

  return Permutation;
}

/// @param InnerSchedules { PostfixSched[] -> Domain[] }
/// @param InnerInstances { PrefixSched[] -> Domain[] }
/// @param Accs { Domain[] -> Data[] }
/// @return { PrefixSched[] -> [Data[] -> PackedData[]] }
static std::tuple<isl::map, std::vector<unsigned int>>
findPackingLayout(isl::union_map InnerSchedules, isl::union_map InnerInstances,
                  isl::union_map Accs) {
  auto Ctx = InnerSchedules.ctx();

  // { [PrefixSched[] -> PostfixSched[]] -> Domain[] }
  auto CombinedInstances = InnerInstances.domain_product(InnerSchedules);

  // { [PrefixSched[] -> PostfixSched[]] -> Data[] }
  auto CombinedAccesses = CombinedInstances.apply_range(Accs);

  // { PostfixSched[] -> Data[] }
  auto UAccessedByPostfix = CombinedAccesses.domain_factor_range();

  auto CombinedAccessesSgl = isl::map::from_union_map(CombinedAccesses);

  // { PrefixSched[] -> Data[] }
  auto AccessedByPrefix = InnerInstances.apply_range(Accs);

  // { PrefixSched[] -> Data[] }
  auto WorkingSet = isl::map::from_union_map(AccessedByPrefix);
  auto IndexSpace = WorkingSet.get_space().range();
  auto LocalIndexSpace = isl::local_space(IndexSpace);

  // FIXME: Should PrefixSched be a PrefixDomain? Is it needed at all when
  // inserting into the schedule tree? { PrefixSched[] }
  auto PrefixSpace = WorkingSet.get_space().domain();
  auto PrefixUniverse = isl::set::universe(PrefixSpace);
  auto PrefixSet = WorkingSet.domain();

  // Get the rectangular shape
  // auto Dims = WorkingSet.dim(isl::dim::out);

#if 1
  isl::pw_aff_list DimMins;
  isl::pw_aff_list DimSizes;
  isl::pw_aff_list DimEnds;
  std::tie(DimMins, DimSizes, DimEnds) = extractExtends(WorkingSet);
#else
  SmallVector<isl::pw_aff, 4> Mins;
  SmallVector<isl::pw_aff, 4> Lens;

  isl::pw_aff_list DimSizes = isl::pw_aff_list::alloc(Ctx, Dims);
  isl::pw_aff_list DimMins = isl::pw_aff_list::alloc(Ctx, Dims);
  for (auto i = 0; i < Dims; i += 1) {
    auto TheDim = WorkingSet.project_out(isl::dim::out, i + 1, Dims - i - 1)
                      .project_out(isl::dim::out, 0, i);
    auto Min = TheDim.lexmin_pw_multi_aff().get_pw_aff(0);
    auto Max = TheDim.lexmax_pw_multi_aff().get_pw_aff(0);

    auto One = isl::aff(isl::local_space(Min.get_space().domain()),
                        isl::val(LocalIndexSpace.get_ctx(), 1));
    auto Len = Max.add(Min.neg()).add(One);

    Mins.push_back(Min);
    Lens.push_back(Len);

    DimMins = DimMins.add(Min);
    DimSizes = DimSizes.add(Len);
  }
#endif

  // { PrefixSched[] -> Data[] }
  auto SourceSpace = PrefixSpace.map_from_domain_and_range(IndexSpace);

  // { PrefixSched[] -> DataMin[] }
  isl::multi_pw_aff AllMins = isl::multi_pw_aff(SourceSpace, DimMins);

#if 1
  auto PackedSizes = sizeBox(DimSizes);
#else
  std::vector<unsigned int> PackedSizes;
  PackedSizes.reserve(Dims);
  for (auto i = 0; i < Dims; i += 1) {
    auto Len = DimSizes.get_pw_aff(i);
    Len = Len.coalesce();

    // FIXME: Because of the interfaces of Scop::createScopArrayInfo, array
    // sizes currently need to be constant
    auto SizeBound = polly::getConstant(Len, true, false);
    assert(SizeBound);
    assert(!SizeBound.is_infty());
    assert(!SizeBound.is_nan());
    assert(SizeBound.is_pos());
    PackedSizes.push_back(SizeBound.get_num_si()); // TODO: Overflow check
  }
#endif

  // auto TmpPackedId = isl::id::alloc(Ctx, (llvm::Twine("TmpPacked_") +
  // SAI->getName()).str().c_str(), nullptr);
  auto TmpPackedId = isl::id::alloc(Ctx, "TmpPacked", nullptr);
  auto TmpPackedSpace = IndexSpace.set_tuple_id(isl::dim::set, TmpPackedId);

  // { PrefixSched[] -> [Data[] -> PackedData[]] }
  auto TargetSpace = PrefixSpace.map_from_domain_and_range(
      IndexSpace.map_from_domain_and_range(TmpPackedSpace).wrap());

  // { [PrefixSched[] -> Data[]] -> [PrefixSched[] -> [Data[] -> PackedData[]]]
  // }
  auto Translator = isl::basic_map::universe(
      SourceSpace.wrap().map_from_domain_and_range(TargetSpace.wrap()));
  auto TranslatorLS = get_local_space(Translator);

  // PrefixSched[] = PrefixSched[]
  for (auto i = 0; i < PrefixSpace.dim(isl::dim::set); i += 1) {
    auto C = isl::constraint::alloc_equality(TranslatorLS);
    C = C.set_coefficient_si(isl::dim::in, i, 1);
    C = C.set_coefficient_si(isl::dim::out, i, -1);
    Translator = add_constraint(Translator, C);
  }

  // Data[] = Data[] - DataMin[]
  for (auto i : seq<isl_size>(0, IndexSpace.dim(isl::dim::set).release())) {
    auto C = isl::constraint::alloc_equality(TranslatorLS);
    // Min
    C = C.set_coefficient_si(isl::dim::in,
                             PrefixSpace.dim(isl::dim::set).release() + i, 1);
    // i
    C = C.set_coefficient_si(isl::dim::out,
                             PrefixSpace.dim(isl::dim::set).release() + i, -1);
    // x
    C = C.set_coefficient_si(isl::dim::out,
                             PrefixSpace.dim(isl::dim::set).release() +
                                 IndexSpace.dim(isl::dim::set).release() + i,
                             1);
    Translator = add_constraint(Translator, C);
  }

  // { PrefixSched[] -> [Data[] -> PackedData[]] }
  auto OrigToPackedIndexMap =
      isl::convert<isl::map>(AllMins).wrap().apply(Translator).unwrap();

  // TupleNest OrigToPackedIndexRef(OrigToPackedIndexMap, "{ PrefixSched[] ->
  // [Data[] -> PackedData[]] }"); TupleNest
  // CombinedAccessesRef(CombinedAccessesSgl, "{ [PrefixSched[] ->
  // PostfixSched[]] -> Data[] }");

  auto Permutation = findDataLayoutPermutation(UAccessedByPostfix, PackedSizes);

  //	Permutation = castSpace(Permutation,
  // TmpPackedSpace.map_from_domain_and_range(PackedSpace));
  Permutation = Permutation.set_tuple_id(isl::dim::in, TmpPackedId)
                    .set_tuple_id(isl::dim::out, TmpPackedId);

  OrigToPackedIndexMap = OrigToPackedIndexMap.uncurry()
                             .intersect_domain(WorkingSet.wrap())
                             .apply_range(Permutation)
                             .curry();

  return {OrigToPackedIndexMap, std::move(PackedSizes)};
}

/// Is output dimension i functionally determined by the map's domain and the
/// dimensions following it?
static bool isFunctionallyDetermined(isl::map Map, int OutDim) {
  auto PMap = Map.project_out(isl::dim::out, 0, OutDim);
  auto InDims = PMap.dim(isl::dim::in).release();
  auto OutDims = PMap.dim(isl::dim::out).release();
  auto MMap =
      PMap.move_dims(isl::dim::in, InDims, isl::dim::out, 1, OutDims - 1);
  return MMap.is_single_valued();
}

/// @param InnerSchedules { PostfixSched[] -> Domain[] }
/// @param InnerInstances { PrefixSched[] -> Domain[] }
/// @param Accs { Domain[] -> Data[] }
/// @return { PrefixSched[] -> [Data[] -> PackedData[]] }
static std::tuple<isl::map, std::vector<unsigned int>>
findPackingLayout2(isl::union_map InnerSchedules, isl::union_map InnerInstances,
                   isl::union_map Accs) {
  auto Ctx = InnerSchedules.ctx();

  // { PostfixSched[] -> Domain[] }
  // InnerSchedules;
  // auto PostfixSchedSpace = InnerSchedules.get_space().domain();

  // { PrefixSched[] -> Domain[] }
  // InnerInstances;
  // auto PrefixSchedSpace = InnerInstances.get_space().domain();

  // { Domain[] -> Data[] }
  // Accs;

  // { [PrefixSched[] -> PostfixSched[]] -> Domain[] }
  auto CombinedInstances = InnerInstances.domain_product(InnerSchedules);

  // { [PrefixSched[] -> PostfixSched[]] -> Data[] }
  auto CombinedAccesses =
      isl::map::from_union_map(CombinedInstances.apply_range(Accs));
  auto PrefixSchedSpace =
      CombinedAccesses.get_space().domain().unwrap().domain();
  auto PostfixSchedSpace =
      CombinedAccesses.get_space().domain().unwrap().range();
  auto DataSpace = CombinedAccesses.get_space().range();
  TupleNest CombinedAccessesNest(
      CombinedAccesses, "{ [PrefixSched[] -> PostfixSched[]] -> Data[] }");

  // { PrefixSched[] -> Data[] }
  auto AccessedByPrefix = InnerInstances.apply_range(Accs);

  // { PrefixSched[] -> Data[] }
  auto WorkingSet = isl::map::from_union_map(AccessedByPrefix);

  // { Packed[] }
  auto PackedId = isl::id::alloc(Ctx, "TmpPacked", nullptr);

  // Goal: { PrefixSched[] -> [Data[] -> Packed[]] }, RHS injective and (as
  // surjective as possible).

  // Assume [Data[] -> PostfixSched[]] to be the new layout. Data[] is added to
  // ensure injectiveness.
  auto Layout1 = rebuildMapNesting(
      CombinedAccessesNest,
      "{ [PrefixSched[] -> Data[]] -> [Data[] ->PostfixSched[]] }");

  // Use Packed[] as packed space.
  // { [PrefixSched[] -> Data[]] -> Packed[] }
  auto Layout2 = Layout1.project_out(isl::dim::out, 0, 0)
                     .set_tuple_id(isl::dim::out, PackedId);
  auto PackedSpace = Layout2.get_space().range();

  // Use only one address to store an element (=> injective).
  auto Layout3 = Layout2.lexmin();

  // Project-out Data[]
  // { PrefixSched[] -> Packed[] }
  auto PackedWorkingSet =
      rebuildNesting(Layout3, "{ [PrefixSched[] -> Data[]] -> Packed[] }",
                     "{ PrefixSched[] -> Packed[] }");
  // PackedWorkingSet = PackedWorkingSet.simple_hull();

  isl::pw_aff_list DimMins;
  isl::pw_aff_list DimSizes;
  isl::pw_aff_list DimEnds;
  std::tie(DimMins, DimSizes, DimEnds) = extractExtends(PackedWorkingSet);

  // { PrefixSched[] -> Packed[] }
  auto AllMins = isl::convert<isl::map>(isl::multi_pw_aff(
      PrefixSchedSpace.map_from_domain_and_range(PackedSpace), DimMins));
  TupleNest AllMinsNest(AllMins, "{ PrefixSched[] -> MinPacked[] }");

  // { [PrefixSched[] -> Data[]] -> Packed[] }
  auto AllMinsWithData =
      rebuildNesting({}, SpaceRef(AllMinsNest["PrefixSched"], DataSpace),
                     AllMinsNest["MinPacked"]);

  // Start first index at 0.
  auto Layout4 = Layout3.subtract(AllMinsWithData);

  auto PackedSizes = sizeBox(DimSizes);

  for (size_t i = 0; i < PackedSizes.size();) {
    auto PackedSize = PackedSizes[i];
    assert(PackedSizes[i] > 0);

    if (PackedSize == 1 ||
        isFunctionallyDetermined(Layout4.curry(),
                                 DataSpace.dim(isl::dim::set).release() + i)) {
      // Remove dimension
      PackedSizes.erase(PackedSizes.begin() + i);
      Layout4 = Layout4.project_out(isl::dim::out, i, 1);
      continue;
    }

    i += 1;
  }

#if 0
   // Remove dimensions of size 1.
   for (int i = PackedSizes.size() - 1; i >= 0; i -= 1) {
     assert(PackedSizes[i] > 0);
     if (PackedSizes[i] > 1)
       continue;


   }
#endif

  // Set the tuple again that might have been lost by project_out.
  Layout4 = Layout4.set_tuple_id(isl::dim::out, PackedId);

  // { [PrefixSched[] -> Data[]] -> Packed[] }
  auto Layout5 = Layout4.curry().coalesce();

  return {Layout5, PackedSizes};
}

/// @param OrigToPackedIndexMap  { PrefixSched[] -> [Data[] -> PackedData[]] }
/// @param InnerInstances        { PrefixSched[] -> Domain[] }
static void
collectRedirects(isl::schedule_node Node, isl::map OrigToPackedIndexMap,
                 isl::union_map InnerInstances,
                 DenseMap<polly::MemoryAccess *, isl::map> &AccessesToUpdate) {
  auto PrefixSpac = OrigToPackedIndexMap.get_space().domain();
  auto OrigToPackedSpace = OrigToPackedIndexMap.get_space().range().unwrap();
  auto OrigSpace = OrigToPackedSpace.domain();
  auto PackedSpace = OrigToPackedSpace.range();

  auto OrigSAI = reinterpret_cast<ScopArrayInfo *>(
      OrigSpace.get_tuple_id(isl::dim::set).get_user());

  if (isl_schedule_node_get_type(Node.get()) == isl_schedule_node_leaf) {
    auto UDomain = Node.get_domain();
    for (auto Domain : UDomain.get_set_list()) {
      auto Space = Domain.get_space();
      auto Id = Domain.get_tuple_id();
      auto Stmt = reinterpret_cast<ScopStmt *>(Id.get_user());
      // auto Domain = Stmt->getDomain();
      // assert(Stmt->getDomain().is_subset(Domain) && "Have to copy statement
      // if not transforming all instances");
      auto DomainSpace = Domain.get_space();

      // { Domain[] -> [Data[] -> PackedData[]] }
      auto PrefixDomainSpace =
          DomainSpace.map_from_domain_and_range(OrigToPackedSpace.wrap());
      auto DomainOrigToPackedUMap =
          isl::union_map(OrigToPackedIndexMap)
              .apply_domain(InnerInstances.intersect_range(Domain));
      auto DomainOrigToPackedMap =
          singleton(DomainOrigToPackedUMap, PrefixDomainSpace);

      for (auto *MemAcc : *Stmt) {
        if (MemAcc->getLatestScopArrayInfo() != OrigSAI)
          continue;

        // { Domain[] -> Data[] }
        auto OrigAccRel = MemAcc->getLatestAccessRelation();
        auto OrigAccDom = OrigAccRel.domain().intersect(Domain);

        // { Domain[] -> PackedData[] }
        auto PackedAccRel = OrigAccRel.domain_map()
                                .apply_domain(DomainOrigToPackedMap.uncurry())
                                .reverse();

        // assert(OrigAccDom.is_subset(PackedAccRel.domain()) && "There must be
        // a packed access for everything that the original access accessed");
        // MemAcc->setNewAccessRelation(PackedAccRel);
        auto &UpdatedRedirect = AccessesToUpdate[MemAcc];
        if (!UpdatedRedirect.is_null())
          UpdatedRedirect = UpdatedRedirect.unite(PackedAccRel);
        else
          UpdatedRedirect = PackedAccRel;
      }
    }
  }

  auto n = Node.n_children();
  for (auto i = 0; i < n; i += 1) {
    auto Child = Node.child(i);
    collectRedirects(Child, OrigToPackedIndexMap, InnerInstances,
                     AccessesToUpdate);
  }
}

template <typename Derived, typename... Args>
struct ExtensionNodeRewriter
    : public RecursiveScheduleTreeVisitor<
          Derived, std::pair<isl::schedule, isl::union_map>,
          const isl::union_set &, Args...> {
  using BaseTy =
      RecursiveScheduleTreeVisitor<Derived,
                                   std::pair<isl::schedule, isl::union_map>,
                                   const isl::union_set &, Args...>;
  BaseTy &getBase() { return *this; }
  const BaseTy &getBase() const { return *this; }
  Derived &getDerived() { return *static_cast<Derived *>(this); }
  const Derived &getDerived() const {
    return *static_cast<const Derived *>(this);
  }

  std::pair<isl::schedule, isl::union_map> visit(const isl::schedule_node &Node,
                                                 const isl::union_set &Domain,
                                                 Args... args) {
    return getBase().visit(Node, Domain, args...);
  }

  std::pair<isl::schedule, isl::union_map> visit(const isl::schedule &Schedule,
                                                 const isl::union_set &Domain,
                                                 Args... args) {
    return getBase().visit(Schedule, Domain, args...);
  }

  isl::schedule visit(const isl::schedule &Schedule, Args... args) {

    auto Domain = Schedule.get_domain();
    // auto Extensions = isl::union_map::empty(isl::space::params_alloc(Ctx,
    // 0));
    auto Result = getDerived().visit(Schedule, Domain, args...);
    assert(Result.second.is_empty() && "Must resolve all extension nodes");
    return Result.first;
  }

  std::pair<isl::schedule, isl::union_map>
  visitDomain(const isl::schedule_node &DomainNode,
              const isl::union_set &Domain, Args... args) {
    // Every isl::schedule implicitly has a domain node in its root, so no need
    // to add a new one Extension nodes can also be roots; these would be
    // converted to domain nodes then
    return getDerived().visit(DomainNode.child(0), Domain, args...);
  }

  std::pair<isl::schedule, isl::union_map>
  visitSequence(const isl::schedule_node &Sequence,
                const isl::union_set &Domain, Args... args) {
    auto NumChildren = isl_schedule_node_n_children(Sequence.get());
    assert(NumChildren >= 1);
    isl::schedule NewNode;
    auto PSpace = Domain.get_space();
    isl::union_map NewExtensions = emptyUMap(PSpace);

    for (int i = 0; i < NumChildren; i += 1) {
      auto OldChild = Sequence.child(i);
      isl::schedule NewChildNode;
      isl::union_map NewChildExtensions;
      std::tie(NewChildNode, NewChildExtensions) =
          getDerived().visit(OldChild, Domain, args...);
      int BandDims = 1;

      for (auto Ext : NewChildExtensions.get_map_list()) {
        int ExtDims = Ext.dim(isl::dim::in).release();
        assert(ExtDims >= BandDims);
        auto OuterDims = ExtDims - BandDims;

        // For ancestor nodes.
        if (OuterDims > 0) {
          auto OuterSched = Ext.project_out(isl::dim::in, OuterDims, BandDims);
          NewExtensions = NewExtensions.unite(OuterSched);
        }

        // FIXME: the extension node schedule dim should match the @p i; but
        // since the extension node is a descendant of this sequence at position
        // @p i, this should be a tautology.
        auto BandSched = Ext.project_out(isl::dim::in, 0, OuterDims).reverse();

        (void)BandSched;
      }

      if (!NewNode.is_null())
        NewNode = isl::manage(
            isl_schedule_sequence(NewNode.release(), NewChildNode.release()));
      else
        NewNode = std::move(NewChildNode);
    }
    return {NewNode, NewExtensions};
  }

  std::pair<isl::schedule, isl::union_map>
  visitSet(const isl::schedule_node &Set, const isl::union_set &Domain,
           Args... args) {
    llvm_unreachable("unimplemented");
  }

  std::pair<isl::schedule, isl::union_map>
  visitMark(const isl::schedule_node &Mark, const isl::union_set &Domain,
            Args... args) {
    auto TheMark = Mark.as<isl::schedule_node_mark>().get_id();
    isl::schedule NewChildNode;
    isl::union_map NewChildExtensions;
    std::tie(NewChildNode, NewChildExtensions) =
        getDerived().visit(Mark.child(0), Domain, args...);
    return {
        NewChildNode.get_root().child(0).insert_mark(TheMark).get_schedule(),
        NewChildExtensions};
  }

  std::pair<isl::schedule, isl::union_map>
  visitLeaf(const isl::schedule_node &Leaf, const isl::union_set &Domain,
            Args... args) {
    auto Ctx = Leaf.ctx();
    auto NewChildNode = isl::schedule::from_domain(Domain);
    auto Extensions = isl::union_map::empty(Ctx);
    return {NewChildNode, Extensions};
  }

  std::pair<isl::schedule, isl::union_map>
  visitBand(const isl::schedule_node &Band, const isl::union_set &Domain,
            Args... args) {
    auto OldChild = Band.child(0);
    auto OldPartialSched =
        isl::manage(isl_schedule_node_band_get_partial_schedule(Band.get()));

    isl::schedule NewChildNode;
    isl::union_map NewChildExtensions;
    std::tie(NewChildNode, NewChildExtensions) =
        getDerived().visit(OldChild, Domain, args...);

    auto PSpace = NewChildExtensions.get_space();
    isl::union_map OuterExtensions = emptyUMap(PSpace);
    isl::union_map BandExtensions = emptyUMap(PSpace);
    auto NewPartialSched = OldPartialSched;
    isl::union_map NewPartialSchedMap = isl::union_map::from(OldPartialSched);

    // We have to add the extensions to the schedule
    auto BandDims = isl_schedule_node_band_n_member(Band.get());
    for (auto Ext : NewChildExtensions.get_map_list()) {
      auto ExtDims = Ext.dim(isl::dim::in).release();
      assert(ExtDims >= BandDims);
      auto OuterDims = ExtDims - BandDims;

      if (OuterDims > 0) {
        auto OuterSched = Ext.project_out(isl::dim::in, OuterDims, BandDims);
        OuterExtensions = OuterExtensions.unite(OuterSched);
      }

      auto BandSched = Ext.project_out(isl::dim::in, 0, OuterDims).reverse();
      BandExtensions = BandExtensions.unite(BandSched.reverse());

      auto AsPwMultiAff = isl::pw_multi_aff::from_map(BandSched);
      auto AsMultiUnionPwAff =
          isl::multi_union_pw_aff::from_union_map(BandSched);
      NewPartialSched = NewPartialSched.union_add(AsMultiUnionPwAff);

      NewPartialSchedMap = NewPartialSchedMap.unite(BandSched);
    }

    auto NewPartialSchedAsAsMultiUnionPwAff =
        isl::multi_union_pw_aff::from_union_map(NewPartialSchedMap);
    auto NewNode = NewChildNode.insert_partial_schedule(
        NewPartialSchedAsAsMultiUnionPwAff);
    return {NewNode, OuterExtensions};
  }

  std::pair<isl::schedule, isl::union_map>
  visitFilter(const isl::schedule_node &Filter, const isl::union_set &Domain,
              Args... args) {
    auto FilterDomain = Filter.as<isl::schedule_node_filter>().get_filter();
    auto NewDomain = Domain.intersect(FilterDomain);
    auto NewChild = getDerived().visit(Filter.child(0), NewDomain);

    // A filter is added implicitly if necessary when joining schedule trees
    return NewChild;
  }

  std::pair<isl::schedule, isl::union_map>
  visitExtension(const isl::schedule_node &Extension,
                 const isl::union_set &Domain, Args... args) {
    auto ExtDomain =
        Extension.as<isl::schedule_node_extension>().get_extension();
    auto NewDomain = Domain.unite(ExtDomain.range());
    auto NewChild = getDerived().visit(Extension.child(0), NewDomain);
    return {NewChild.first, NewChild.second.unite(ExtDomain)};
  }
};

class ExtensionNodeRewriterPlain
    : public ExtensionNodeRewriter<ExtensionNodeRewriterPlain> {};

struct ScheduleTreeCollectExtensionNodes
    : public RecursiveScheduleTreeVisitor<
          ScheduleTreeCollectExtensionNodes, void,
          SmallVectorImpl<isl::schedule_node> &> {
  void visitExtension(const isl::schedule_node &Extension,
                      SmallVectorImpl<isl::schedule_node> &List) {
    List.push_back(Extension);
  }
};

/// Hoist all domains from extension into the root domain node, such that there
/// are no more extension nodes (which isl does not support for some
/// operations). This assumes that domains added by to extension nodes do not
/// overlap.
static isl::schedule hoistExtensionNodes2(isl::schedule Sched) {
  auto Root = Sched.get_root();
  auto RootDomain = Sched.get_domain();
  auto ParamSpace = RootDomain.get_space();
  // ScheduleTreeCollectDomains DomainCollector(RootDomain.get_space());
  // DomainCollector.visit(Sched);
  // auto AllDomains = DomainCollector.Domains;

  // auto NewRool = isl::schedule_node::from_domain(AllDomains);

  ScheduleTreeCollectExtensionNodes ExtensionCollector;
  SmallVector<isl::schedule_node, 4> ExtNodes;
  ExtensionCollector.visit(Sched, ExtNodes);

  isl::union_set ExtDomains = emptyUSet(ParamSpace);
  isl::union_map Extensions = emptyUMap(ParamSpace);
  for (auto ExtNode : ExtNodes) {
    auto Extension = ExtNode.as<isl::schedule_node_extension>().get_extension();
    ExtDomains = ExtDomains.unite(Extension.range());
    Extensions = Extensions.unite(Extension);
  }
  auto AllDomains = ExtDomains.unite(RootDomain);
  auto NewRoot = isl::schedule_node::from_domain(AllDomains);

  ExtensionNodeRewriterPlain rewriter;
  auto NewSched = rewriter.visit(Sched);

  return NewSched;
}

static void applyDataPack(Scop &S, isl::schedule &Sched,
                          isl::schedule_node TheBand, const ScopArrayInfo *SAI,
                          bool OnHeap, StringRef &ErrorDesc, isl::set IslSize,
                          isl::map IslRedirect) {
  ErrorDesc = StringRef();
  // auto Ctx = S.getIslCtx();

  if (!IslSize.is_null())
    LLVM_DEBUG(dbgs() << "IslSize: " << IslSize << "\n");
  if (!IslRedirect.is_null())
    LLVM_DEBUG(dbgs() << "IslRedirect: " << IslRedirect << "\n");

  auto PSpace = S.getParamSpace();
  isl::union_map Accs = emptyUMap(PSpace);
  collectMemAccsDomains(TheBand, SAI, Accs, false);

  SmallVector<polly::MemoryAccess *, 16> MemAccs;
  collectSubtreeAccesses(TheBand, SAI, MemAccs);

  if (MemAccs.empty()) {
    LLVM_DEBUG(dbgs() << "#pragma clang loop pack failed: No access found");
    ErrorDesc = "No access to array in loop";
    return;
  }

  auto SchedMap = Sched.get_map();
  auto SchedSpace = getScatterSpace(SchedMap);
  auto ParamSpace = SchedSpace.params();

  auto ArraySpace = ParamSpace.set_from_params()
                        .add_dims(isl::dim::set, SAI->getNumberOfDimensions())
                        .set_tuple_id(isl::dim::set, SAI->getBasePtrId());
  auto SchedArraySpace = SchedSpace.map_from_domain_and_range(ArraySpace);

  // { Sched[] -> Data[] }
  auto AllSchedRel = isl::map::empty(SchedArraySpace);
  for (auto Acc : MemAccs) {
    auto Stmt = Acc->getStatement();
    auto Dom = Stmt->getDomain();
    auto Rel = Acc->getLatestAccessRelation();
    auto RelSched = SchedMap.apply_domain(Rel);

    auto SchedRel = RelSched.reverse();
    auto SingleSchedRel = singleton(SchedRel, SchedArraySpace);

    AllSchedRel = AllSchedRel.unite(SingleSchedRel);
  }

  bool WrittenTo = false;
  // bool ReadFrom = false;
  for (auto *Acc : MemAccs) {
    // if (Acc->isRead())
    //   ReadFrom = true;
    if (Acc->isMayWrite() || Acc->isMustWrite())
      WrittenTo = true;

    if (Acc->isAffine())
      continue;

    LLVM_DEBUG(dbgs() << "#pragma clang loop pack failed: Can only transform "
                         "affine access relations");
    ErrorDesc = "All array accesses must be affine";
    return;
  }

  CollectInnerSchedules InnerSchedCollector(ParamSpace);
  InnerSchedCollector.visit(TheBand);

  // { PostfixSched[] -> Domain[] }
  auto InnerSchedules = InnerSchedCollector.InnerSched.reverse();

  // { PrefixSched[] -> Domain[] }
  auto InnerInstances = collectParentSchedules(TheBand);

  // { PrefixSched[] -> [Data[] -> PackedData[]] }
  isl::map OrigToPackedIndexMap;
  std::vector<unsigned int> PackedSizes;
  if (!IslSize.is_null() || !IslRedirect.is_null()) {
    std::tie(OrigToPackedIndexMap, PackedSizes) = readPackingLayout(
        InnerSchedules, InnerInstances, Accs, IslSize, IslRedirect);
  } else {
    std::tie(OrigToPackedIndexMap, PackedSizes) =
        findPackingLayout(InnerSchedules, InnerInstances, Accs);
  }
  if (0) {
    isl::map OrigToPackedIndexMap2;
    std::vector<unsigned int> PackedSizes2;
    std::tie(OrigToPackedIndexMap2, PackedSizes2) =
        findPackingLayout2(InnerSchedules, InnerInstances, Accs);
  }

  LLVM_DEBUG(dbgs() << "OrigToPackedIndexMap: " << OrigToPackedIndexMap
                    << "\n");
  LLVM_DEBUG({
    dbgs() << "PackedSizes: (";
    for (size_t i = 0; i < PackedSizes.size(); i += 1) {
      if (i > 0)
        dbgs() << ", ";
      dbgs() << PackedSizes[i];
    }
    dbgs() << ")\n";
  });

  // Create packed array
  // FIXME: Unique name necessary?
  auto PackedSAI = S.createScopArrayInfo(
      SAI->getElementType(), (llvm::Twine("Packed_") + SAI->getName()).str(),
      PackedSizes);
  PackedSAI->setIsOnHeap(OnHeap);
  auto PackedId = PackedSAI->getBasePtrId();

  // Apply Packed id generated by createScopArrayInfo instead of a temporary
  // one.
  OrigToPackedIndexMap = OrigToPackedIndexMap.uncurry()
                             .set_tuple_id(isl::dim::out, PackedId)
                             .curry();

  // Create a copy-in statement
  // TODO: Only if working set is read-from
  // { [PrefixSched[] -> PackedData[]] -> Data[] }
  auto CopyInSrc = polly::reverseRange(OrigToPackedIndexMap).uncurry();

  // { [PrefixSched[] -> PackedData[]] }
  auto CopyInDomain = CopyInSrc.domain();

  // { [PrefixSched[] -> PackedData[]] -> PackedData[] }
  auto CopyInDst = CopyInDomain.unwrap().range_map();

  simplify(CopyInSrc);
  simplify(CopyInDst);
  simplify(CopyInDomain);

  auto CopyIn = S.addScopStmt(CopyInSrc, CopyInDst, CopyInDomain);

  CopyInDomain = CopyIn->getDomain();
  auto CopyInId = CopyInDomain.get_tuple_id();

  // Update all inner access-relations to access PackedSAI instead of SAI
  // TODO: Use MemAccs instead of traversing the subtree again
  //  redirectAccesses(TheBand, OrigToPackedIndexMap, InnerInstances);
  DenseMap<polly::MemoryAccess *, isl::map> Redirects;
  collectRedirects(TheBand, OrigToPackedIndexMap, InnerInstances, Redirects);
  for (auto &Redirect : Redirects) {
    auto MemAcc = Redirect.getFirst();
    auto NewAccRel = Redirect.getSecond();
    auto Stmt = MemAcc->getStatement();
    assert(Stmt->getDomain().is_subset(NewAccRel.domain()) &&
           "Have to copy statement if not transforming all instances");
    simplify(NewAccRel);
    MemAcc->setNewAccessRelation(NewAccRel);
  }

  auto Node = TheBand;

  // Insert Copy-In/Out into schedule tree
  // TODO: No need for copy-in for elements that are overwritten before read
  if (1) {
    // TODO: Copy might not be necessary every time: mapping might not depend on
    // the outer loop.
    auto ExtensionBeforeNode = isl::schedule_node::from_extension(
        CopyInDomain.unwrap().domain_map().reverse().set_tuple_id(isl::dim::out,
                                                                  CopyInId));
    Node = moveToBandMark(Node).graft_before(ExtensionBeforeNode);
  }

  if (WrittenTo) {
    // Create a copy-out statement
    auto CopyOut = S.addScopStmt(CopyInDst, CopyInSrc, CopyInDomain);

    auto CopyOutDomain = CopyOut->getDomain();
    // OrigToPackedIndexMap = cast(OrigToPackedIndexMap, );
    auto CopyOutId = CopyOutDomain.get_tuple_id();

    // TODO: Only copy-out elements that are potentially written.
    auto ExtensionBeforeAfter = isl::schedule_node::from_extension(
        CopyOutDomain.unwrap().domain_map().reverse().set_tuple_id(
            isl::dim::out, CopyOutId));
    Node = Node.graft_after(ExtensionBeforeAfter);
  }

  // TODO: Update dependencies

  auto NewSched = Node.get_schedule();
  auto SchedWithoutExtensionNodes = hoistExtensionNodes2(NewSched);
  Sched = SchedWithoutExtensionNodes;
}

static isl::schedule applyLoopUnrollAndJam(MDNode *LoopMD,
                                           isl::schedule_node BandToUnroll) {
  assert(!BandToUnroll.is_null());
  // auto Ctx = BandToUnroll.get_ctx();

  auto Factor = findOptionalIntOperand(LoopMD, "llvm.loop.unroll_and_jam.count")
                    .getValueOr(0);
  auto Full = findOptionalBoolOperand(LoopMD, "llvm.loop.unroll_and_jam.full")
                  .getValueOr(false);
  auto UnrolledID =
      findOptionalMDOperand(LoopMD,
                            "llvm.loop.unroll_and_jam.followup_outer_unrolled")
          .getValueOr(nullptr);

  BandToUnroll = moveToBandMark(BandToUnroll);
  BandToUnroll = removeMark(BandToUnroll);

  SmallVector<isl::schedule_node> IntermediateBands;
  auto BandToJam = BandToUnroll;
  auto Cur = BandToJam;
  while (true) {

    if (Cur.n_children().release() != 1)
      break;
    auto Child = Cur.first_child();
    if (isBand(Child)) {
      IntermediateBands.push_back(Cur);
      BandToJam = Child;
    }
    Cur = Child;
  }
  assert(!isSameNode(BandToJam, BandToUnroll) &&
         "unroll-and-jam requires perfect loop nest");
  IntermediateBands.pop_back();

  auto JamAttr = getBandAttr(BandToJam);
  auto JammedID =
      findOptionalMDOperand(JamAttr->Metadata,
                            "llvm.loop.unroll_and_jam.followup_inner_unrolled")
          .getValueOr(nullptr);

  SmallVector<MDNode *> IntermediateIDs;
  for (auto Band : IntermediateBands) {
    auto Attr = getBandAttr(Band);
    auto IntermediateID =
        findOptionalMDOperand(
            Attr->Metadata,
            "llvm.loop.unroll_and_jam.followup_intermediate_unrolled")
            .getValueOr(nullptr);
    IntermediateIDs.push_back(IntermediateID);
  }

  return unrollAndOrJam(BandToUnroll, BandToJam, Factor, Full, UnrolledID,
                        IntermediateIDs, JammedID);
}

static void collectAccessInstList(SmallVectorImpl<Instruction *> &Insts,
                                  DenseSet<Metadata *> InstMDs, Function &F,
                                  StringRef MetadataName = "llvm.access") {
  Insts.reserve(InstMDs.size());
  for (auto &BB : F) {
    for (auto &Inst : BB) {
      auto MD = Inst.getMetadata(MetadataName);
      if (MD)
        if (InstMDs.count(MD))
          Insts.push_back(&Inst);
    }
  }
}

static void
collectMemoryAccessList(SmallVectorImpl<polly::MemoryAccess *> &MemAccs,
                        ArrayRef<Instruction *> Insts, Scop &S) {
  // auto &R = S.getRegion();

  DenseSet<Instruction *> InstSet;
  InstSet.insert(Insts.begin(), Insts.end());

  for (auto &Stmt : S) {
    for (auto *Acc : Stmt) {
      if (InstSet.count(Acc->getAccessInstruction()))
        MemAccs.push_back(Acc);
    }
  }
}

static isl::schedule
applyArrayPacking(MDNode *LoopMD, isl::schedule_node LoopToPack, Function *F,
                  Scop *S, OptimizationRemarkEmitter *ORE, Value *CodeRegion) {
  assert(!LoopToPack.is_null());
  auto Ctx = LoopToPack.ctx();

  // TODO: Allow multiple "llvm.data.pack.array"
  auto ArraysMD = findOptionMDForLoopID(LoopMD, "llvm.data.pack.array");
  DenseSet<Metadata *> AccMDs;
  if (ArraysMD) {
    for (const auto &A : drop_begin(ArraysMD->operands(), 1))
      AccMDs.insert(A.get());
  }

  auto OnHeap = findOptionalStringOperand(LoopMD, "llvm.data.pack.allocate")
                    .getValueOr("alloca") == "malloc";
  auto IslSizeStr = findOptionalStringOperand(LoopMD, "llvm.data.pack.isl_size")
                        .getValueOr("");
  auto IslRedirectStr =
      findOptionalStringOperand(LoopMD, "llvm.data.pack.isl_redirect")
          .getValueOr("");

  isl::set IslSize;
  if (!IslSizeStr.empty())
    IslSize = isl::set(Ctx, IslSizeStr.str());
  isl::map IslRedirect;
  if (!IslRedirectStr.empty())
    IslRedirect = isl::map(Ctx, IslRedirectStr.str());

  SmallVector<Instruction *, 32> AccInsts;
  collectAccessInstList(AccInsts, AccMDs, *F);
  SmallVector<polly::MemoryAccess *, 32> MemAccs;
  collectMemoryAccessList(MemAccs, AccInsts, *S);

  SmallPtrSet<const ScopArrayInfo *, 2> SAIs;
  for (auto MemAcc : MemAccs)
    SAIs.insert(MemAcc->getLatestScopArrayInfo());

  StringRef ErrorDesc = "unknown error";
  bool AnySuccess = false;

  // TODO: Check consistency: Are all MemoryAccesses for all selected SAIs in
  // MemAccs?
  // TODO: What should happen for polly::MemoryAccess that got their SAI
  // changed?

  auto Sched = LoopToPack.get_schedule();

  if (SAIs.empty()) {
    LLVM_DEBUG(dbgs() << "No ScopArrayInfo found\n");
    ErrorDesc = "No access to array in loop";
  } else {
    for (auto *SAI : SAIs) {
      StringRef NewErrorDesc;
      applyDataPack(*S, Sched, LoopToPack, SAI, OnHeap, NewErrorDesc, IslSize,
                    IslRedirect);
      if (NewErrorDesc.empty())
        AnySuccess = true;
      else if (!ErrorDesc.empty())
        ErrorDesc = NewErrorDesc;
    }
  }

  if (!AnySuccess) {
    auto &Ctx = LoopMD->getContext();
    LLVM_DEBUG(dbgs() << "Could not apply array packing\n");

    if (ORE) {
      auto Loc = findOptionalDebugLoc(LoopMD, "llvm.data.pack.loc");
      ORE->emit(DiagnosticInfoOptimizationFailure(
                    DEBUG_TYPE, "RequestedArrayPackingFailed", Loc, CodeRegion)
                << (Twine("array not packed: ") + ErrorDesc).str());
    }

    // If illegal, revert and remove the transformation.
    auto NewLoopMD =
        makePostTransformationMetadata(Ctx, LoopMD, {"llvm.data.pack."}, {});
    auto Attr = getBandAttr(LoopToPack);
    Attr->Metadata = NewLoopMD;

    // Roll-back old schedule.
    return LoopToPack.get_schedule();
  }

  auto Mark = moveToBandMark(LoopToPack);
  if (isBandMark(Mark)) {
    auto Attr = static_cast<BandAttr *>(
        Mark.as<isl::schedule_node_mark>().get_id().get_user());

    auto NewLoopMD = makePostTransformationMetadata(
        F->getContext(), Attr->Metadata, {"llvm.data.pack."}, {});
    Attr->Metadata = NewLoopMD;
  }

  return Sched;
}

static isl::schedule
applyParallelizeThread(MDNode *LoopMD, isl::schedule_node BandToParallelize) {
  assert(!BandToParallelize.is_null());
  auto Ctx = BandToParallelize.ctx();

  BandToParallelize = moveToBandMark(BandToParallelize);
  // auto OldAttr = getBandAttr(BandToParallelize);
  BandToParallelize = removeMark(BandToParallelize);

  assert(isl_schedule_node_band_n_member(BandToParallelize.get()) == 1);
  isl::schedule_node ParallelizedBand =
      BandToParallelize.as<isl::schedule_node_band>().member_set_coincident(
          0, true);

  auto NewBandId = makeTransformLoopId(Ctx, nullptr, "threaded");
  auto NewAttr = static_cast<BandAttr *>(NewBandId.get_user());
  NewAttr->ForceThreadParallel = true;
  ParallelizedBand = insertMark(ParallelizedBand, NewBandId);

  return ParallelizedBand.get_schedule();
}

/// Apply full or partial unrolling.
static isl::schedule applyLoopUnroll(LLVMContext &LLVMCtx, MDNode *LoopMD,
                                     isl::schedule_node BandToUnroll) {
  TransformationMode UnrollMode = ::hasUnrollTransformation(LoopMD);
  if (UnrollMode & TM_Disable)
    return {};

  assert(!BandToUnroll.is_null());
  // TODO: Isl's codegen also supports unrolling by isl_ast_build via
  // isl_schedule_node_band_set_ast_build_options({ unroll[x] }) which would be
  // more efficient because the content duplication is delayed. However, the
  // unrolled loop could be input of another loop transformation which expects
  // the explicit schedule nodes. That is, we would need this explicit expansion
  // anyway and using the ISL codegen option is a compile-time optimization.
  int64_t Factor =
      getOptionalIntLoopAttribute(LoopMD, "llvm.loop.unroll.count").value_or(0);
  bool Full = getBooleanLoopAttribute(LoopMD, "llvm.loop.unroll.full");
  assert((!Full || !(Factor > 0)) &&
         "Cannot unroll fully and partially at the same time");

  if (Full)
    return applyFullUnroll(BandToUnroll);

  if (Factor > 0)
    return applyPartialUnroll(LLVMCtx, BandToUnroll, Factor);

  return applyHeuristicUnroll(LLVMCtx, BandToUnroll);
}

static isl::schedule applyLoopFission(MDNode *LoopMD,
                                      isl::schedule_node BandToFission) {
  SmallVector<uint64_t, 4> SplitPos;
  auto SplitsMD =
      findOptionMDForLoopID(LoopMD, "llvm.loop.distribute.split_at");
  if (SplitsMD) {
    for (auto &X : drop_begin(SplitsMD->operands(), 1)) {
      auto PosConst = cast<ConstantAsMetadata>(X)->getValue();
      auto Pos = cast<ConstantInt>(PosConst)->getZExtValue();
      SplitPos.push_back(Pos);
    }

    return applyFission(LoopMD, BandToFission, SplitPos);
  }

  return applyMaxFission(BandToFission);

  // Assume autofission
  //  return applyAutofission(BandToFission, D);
}

static void foreachScheduleTreeChild(
    isl::schedule_node Parent,
    llvm::function_ref<void(const isl::schedule_node &Child)> Callback) {
  if (Parent.has_children()) {
    auto C = Parent.first_child();
    while (true) {
      Callback(C);
      if (!C.has_next_sibling())
        break;
      C = C.next_sibling();
    }
  }
}

static bool isFilter(const isl::schedule_node &Node) {
  return isl_schedule_node_get_type(Node.get()) == isl_schedule_node_filter;
}

static bool isSequence(const isl::schedule_node &Node) {
  auto Kind = isl_schedule_node_get_type(Node.get());
  return Kind == isl_schedule_node_sequence;
}

static isl::schedule applyLoopFuseGroup(isl::schedule_node FuseParent,
                                        MDNode *FuseGroup) {
  assert(FuseGroup);
  assert(isSequence(FuseParent));

  MDNode *FollowupFusedId = nullptr;
  SmallVector<isl::schedule_node> ToBeFused;
  foreachScheduleTreeChild(FuseParent, [&](const isl::schedule_node &Child) {
    auto ChildBand = Child;
    if (isFilter(ChildBand))
      ChildBand = ChildBand.first_child();

    auto BandAttr = getBandAttr(ChildBand);
    if (!BandAttr)
      return;
    auto LoopMD = BandAttr->Metadata;
    if (!LoopMD)
      return;

    auto ThisFuseGroup =
        findMetadataOperand(LoopMD, "llvm.loop.fuse.fuse_group")
            .getValueOr(nullptr);
    if (ThisFuseGroup && ThisFuseGroup == FuseGroup) {
      // This loop is fused
      ToBeFused.push_back(ChildBand);
      if (!FollowupFusedId)
        FollowupFusedId = cast_or_null<MDNode>(
            findMetadataOperand(LoopMD, "llvm.loop.fuse.followup_fused")
                .getValueOr(nullptr));
    }
  });

  assert(ToBeFused.size() >= 2);
  return applyFusion(ToBeFused, FollowupFusedId);
}

static isl::schedule applyLoopFusion(MDNode *LoopMD,
                                     isl::schedule_node BandToFuse) {

  // auto FuseWith = findOptionMDForLoopID(LoopMD, "llvm.loop.fuse.fuse_with");
  auto FuseGroup = cast<MDNode>(
      findMetadataOperand(LoopMD, "llvm.loop.fuse.fuse_group").getValue());
  assert(FuseGroup);

  isl::schedule_node ParentBand = BandToFuse.parent();

  while (!isSequence(ParentBand)) {
    ParentBand = ParentBand.parent();
  }

  return applyLoopFuseGroup(ParentBand, FuseGroup);
}

// Return the properties from a LoopID. Scalar properties are ignored.
static auto getLoopMDProps(MDNode *LoopMD) {
  return map_range(
      make_filter_range(
          drop_begin(LoopMD->operands(), 1),
          [](const MDOperand &MDOp) { return isa<MDNode>(MDOp.get()); }),
      [](const MDOperand &MDOp) { return cast<MDNode>(MDOp.get()); });
}

/// Recursively visit all nodes in a schedule, loop for loop-transformations
/// metadata and apply the first encountered.
class SearchTransformVisitor final
    : public RecursiveScheduleTreeVisitor<SearchTransformVisitor> {
private:
  using BaseTy = RecursiveScheduleTreeVisitor<SearchTransformVisitor>;
  BaseTy &getBase() { return *this; }
  const BaseTy &getBase() const { return *this; }

  llvm::Function *F;
  polly::Scop *S;
  const Dependences *D;
  OptimizationRemarkEmitter *ORE;

  // Set after a transformation is applied. Recursive search must be aborted
  // once this happens to ensure that any new followup transformation is
  // transformed in innermost-first order.
  isl::schedule Result;

  /// Check wether a schedule after a transformation is legal.
  /// If yes, return the new schedule.
  /// If not, emit diagnostics and return the old schedule without the
  /// transformation metadata.
  isl::schedule
  checkDependencyViolation(isl::schedule NewSchedule, llvm::MDNode *LoopMD,
                           llvm::Value *CodeRegion,
                           const isl::schedule_node &OrigBand,
                           StringRef DebugLocAttr, StringRef TransPrefix,
                           StringRef RemarkName, StringRef TransformationName) {
    // Transformation not applied.
    if (NewSchedule.is_null())
      return NewSchedule;

    if (D->isValidSchedule(*S, NewSchedule))
      return NewSchedule;

    LLVMContext &Ctx = LoopMD->getContext();
    LLVM_DEBUG(dbgs() << "Dependency violation detected\n");

    DebugLoc TransformLoc = findTransformationDebugLoc(LoopMD, DebugLocAttr);

    if (IgnoreDepcheck) {
      LLVM_DEBUG(dbgs() << "Still accepting transformation due to "
                           "-polly-pragma-ignore-depcheck\n");
      if (ORE) {
        ORE->emit(
            OptimizationRemark(DEBUG_TYPE, RemarkName, TransformLoc, CodeRegion)
            << (Twine("Could not verify dependencies for ") +
                TransformationName +
                "; still applying because of -polly-pragma-ignore-depcheck")
                   .str());
      }
      return NewSchedule;
    }

    LLVM_DEBUG(dbgs() << "Rolling back transformation\n");

    if (ORE) {
      ORE->emit(DiagnosticInfoOptimizationFailure(DEBUG_TYPE, RemarkName,
                                                  TransformLoc, CodeRegion)
                << (Twine("not applying ") + TransformationName +
                    ": cannot ensure semantic equivalence due to possible "
                    "dependency violations")
                       .str());
    }

    // If illegal, revert and remove the transformation to not risk re-trying
    // indefintely.
    MDNode *NewLoopMD =
        makePostTransformationMetadata(Ctx, LoopMD, {TransPrefix}, {});
    BandAttr *Attr = getBandAttr(OrigBand);
    Attr->Metadata = NewLoopMD;

    // Roll back old schedule.
    return OrigBand.get_schedule();
  }

public:
  SearchTransformVisitor(llvm::Function *F, polly::Scop *S,
                         const Dependences *D, OptimizationRemarkEmitter *ORE)
      : F(F), S(S), D(D), ORE(ORE) {}

  static isl::schedule applyOneTransformation(llvm::Function *F, polly::Scop *S,
                                              const Dependences *D,
                                              OptimizationRemarkEmitter *ORE,
                                              const isl::schedule &Sched) {
    SearchTransformVisitor Transformer(F, S, D, ORE);
    Transformer.visit(Sched);
    return Transformer.Result;
  }

  void visitBand(isl::schedule_node_band Band) {
    LLVMContext &LLVMCtx = F->getContext();

    // Transform inner loops first (depth-first search).
    getBase().visitBand(Band);
    if (!Result.is_null())
      return;

    auto Mark = moveToBandMark(Band);
    if (Mark.is_null() || Mark.get() == Band.get())
      return;

    auto Attr = static_cast<BandAttr *>(
        Mark.as<isl::schedule_node_mark>().get_id().get_user());
    auto Loop = Attr->OriginalLoop;
    Value *CodeRegion = nullptr;
    if (Loop)
      CodeRegion = Loop->getHeader();
    if (!CodeRegion)
      CodeRegion = &F->getEntryBlock();

    // CodeRegion used but ORE to determine code hotness.
    // TODO: Works only for original loop; for transformed loops, should track
    // where the loop's body code comes from.
    // llvm::Loop *L = Attr->OriginalLoop;
    // Value *CodeRegion = nullptr;
    //  if (L)
    //   CodeRegion = L->getHeader();

    MDNode *LoopMD = Attr->Metadata;
    if (!LoopMD)
      return;
    auto &Ctx = LoopMD->getContext();

    // Exlicitly skip
    if (findOptionMDForLoopID(LoopMD, "llvm.loop.polly.done"))
      return;

    // Iterate over loop properties to find the first transformation.
    // FIXME: If there are more than one transformation in the LoopMD (making
    // the order of transformations ambiguous), all others are silently ignored.
    for (MDNode *MD : getLoopMDProps(LoopMD)) {
      auto *NameMD = dyn_cast<MDString>(MD->getOperand(0).get());
      if (!NameMD)
        continue;
      StringRef AttrName = NameMD->getString();

      if (AttrName == "llvm.loop.reverse.enable") {
        // TODO: Read argument (0 to disable)
        isl::schedule Reversed = applyLoopReversal(LoopMD, Band);
        Result = checkDependencyViolation(
            Reversed, LoopMD, CodeRegion, Band, "llvm.loop.reverse.loc",
            "llvm.loop.reverse.", "FailedRequestedReversal", "loop reversal");

        // TODO: Sink this out of the conditions
        if (!Result.is_null())
          return;
      } else if (AttrName == "llvm.loop.tile.enable") {
        // TODO: Read argument (0 to disable)
        auto Tiled = applyLoopTiling(LoopMD, Band);
        Result = checkDependencyViolation(
            Tiled, LoopMD, CodeRegion, Band, "llvm.loop.tile.loc",
            "llvm.loop.tile.", "FailedRequestedTiling", "loop tiling");
        if (!Result.is_null())
          return;
      } else if (AttrName == "llvm.loop.interchange.enable") {
        // TODO: Read argument (0 to disable)
        auto Interchanged = applyLoopInterchange(LoopMD, Band);
        Result = checkDependencyViolation(
            Interchanged, LoopMD, CodeRegion, Band, "llvm.loop.interchange.loc",
            "llvm.loop.interchange.", "FailedRequestedInterchange",
            "loop interchange");

        if (!Result.is_null())
          return;
      } else if (AttrName == "llvm.loop.unroll.enable" ||
                 AttrName == "llvm.loop.unroll.count" ||
                 AttrName == "llvm.loop.unroll.full") {
        Result = applyLoopUnroll(LLVMCtx, LoopMD, Band);
        if (!Result.is_null())
          return;
      } else if (AttrName == "llvm.loop.unroll_and_jam.enable") {
        // TODO: Read argument (0 to disable)
        auto UnrollAndJammed = applyLoopUnrollAndJam(LoopMD, Band);
        Result = checkDependencyViolation(
            UnrollAndJammed, LoopMD, CodeRegion, Band,
            "llvm.loop.unroll_and_jam.loc", "llvm.loop.unroll_and_jam.",
            "FailedRequestedUnrollAndJam", "unroll-and-jam");
        if (!Result.is_null())
          return;
      } else if (AttrName == "llvm.data.pack.enable") {
        // TODO: When is this transformation illegal? E.g. non-access?
        Result = applyArrayPacking(LoopMD, Band, F, S, ORE, CodeRegion);
        if (!Result.is_null())
          return;
      } else if (AttrName == "llvm.loop.parallelize_thread.enable") {
        auto IsCoincident =
            Band.as<isl::schedule_node_band>().member_get_coincident(0);
        if (!IsCoincident) {
          auto DepsAll =
              D->getDependences(Dependences::TYPE_RAW | Dependences::TYPE_WAW |
                                Dependences::TYPE_WAR | Dependences::TYPE_RED);
          auto MySchedMap = Band.first_child().get_prefix_schedule_relation();
          auto IsParallel = D->isParallel(MySchedMap.get(), DepsAll.release());
          if (!IsParallel) {
            LLVM_DEBUG(dbgs() << "Dependency violation detected\n");
            if (IgnoreDepcheck) {
              LLVM_DEBUG(dbgs()
                         << "Ignoring due to -polly-pragma-ignore-depcheck\n");
            } else {
              LLVM_DEBUG(dbgs() << "Rolling back transformation\n");

              if (ORE) {
                auto Loc = findOptionalDebugLoc(
                    LoopMD, "llvm.loop.parallelize_thread.loc");
                // Each '<<' on ORE is visible in the YAML output; to avoid
                // breaking changes, use Twine.
                ORE->emit(DiagnosticInfoOptimizationFailure(
                              DEBUG_TYPE, "FailedRequestedThreadParallelism",
                              Loc, CodeRegion)
                          << "loop not thread-parallelized: transformation "
                             "would violate dependencies");
              }

              // If illegal, revert and remove the transformation.
              auto NewLoopMD = makePostTransformationMetadata(
                  Ctx, LoopMD, {"llvm.loop.parallelize_thread."}, {});
              auto Attr = getBandAttr(Band);
              Attr->Metadata = NewLoopMD;

              // Roll back old schedule.
              Result = Band.get_schedule();
              return;
            }
          }
        }

        Result = applyParallelizeThread(LoopMD, Band);
        if (!Result.is_null())
          return;
      } else if (AttrName == "llvm.loop.distribute.enable") {
        auto Distributed = applyLoopFission(LoopMD, Band);
        Result = checkDependencyViolation(
            Distributed, LoopMD, CodeRegion, Band, "llvm.loop.distribute.loc",
            "llvm.loop.distribute.", "FailedRequestedFission",
            "loop fission/distribution");
        if (!Result.is_null())
          return;
      } else if (AttrName == "llvm.loop.fuse.enable") {
        auto Fused = applyLoopFusion(LoopMD, Band);
        Result = checkDependencyViolation(
            Fused, LoopMD, CodeRegion, Band, "llvm.loop.fuse.loc",
            "llvm.loop.fuse.", "FailedRequestedFusion", "loop fusion");
        if (!Result.is_null())
          return;
      }

      // not a loop transformation; look for next property
    }
  }

  void visitNode(isl::schedule_node Other) {
    if (!Result.is_null())
      return;
    return getBase().visitNode(Other);
  }
};

} // namespace

isl::schedule
polly::applyManualTransformations(Scop *S, isl::schedule Sched,
                                  const Dependences &D,
                                  OptimizationRemarkEmitter *ORE) {
  Function &F = S->getFunction();

  // Search the loop nest for transformations until fixpoint.
  while (true) {
    isl::schedule Result =
        SearchTransformVisitor::applyOneTransformation(&F, S, &D, ORE, Sched);
    if (Result.is_null()) {
      // No (more) transformation has been found.
      break;
    }

    // Use transformed schedule and look for more transformations.
    Sched = Result;
  }

  return Sched;
}
