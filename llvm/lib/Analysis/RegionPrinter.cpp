//===- RegionPrinter.cpp - Print regions tree pass ------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// Print out the region tree of a function using dotty/graphviz.
//===----------------------------------------------------------------------===//

#include "llvm/Analysis/RegionPrinter.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/Analysis/DOTGraphTraitsPass.h"
#include "llvm/Analysis/RegionInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/RegionIterator.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/DominanceFrontier.h"
#include "llvm/InitializePasses.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/ADT/DepthFirstIterator.h"
#include "llvm/ADT/SetVector.h"
#ifndef NDEBUG
#include "llvm/IR/LegacyPassManager.h"
#endif

using namespace llvm;

//===----------------------------------------------------------------------===//
/// onlySimpleRegion - Show only the simple regions in the RegionViewer.
static cl::opt<bool>
onlySimpleRegions("only-simple-regions",
                  cl::desc("Show only simple regions in the graphviz viewer"),
                  cl::Hidden,
                  cl::init(false));

namespace llvm {
template<>
struct DOTGraphTraits<RegionNode*> : public DefaultDOTGraphTraits {

  DOTGraphTraits (bool isSimple=false)
    : DefaultDOTGraphTraits(isSimple) {}

  std::string getNodeLabel(RegionNode *Node, RegionNode *Graph) {

    if (!Node->isSubRegion()) {
      BasicBlock *BB = Node->getNodeAs<BasicBlock>();

      if (isSimple())
        return DOTGraphTraits<DOTFuncInfo *>
          ::getSimpleNodeLabel(BB, nullptr);
      else
        return DOTGraphTraits<DOTFuncInfo *>
          ::getCompleteNodeLabel(BB, nullptr);
    }

    return "Not implemented";
  }
};

template <>
struct DOTGraphTraits<RegionInfo *> : public DOTGraphTraits<RegionNode *> {

  DOTGraphTraits (bool isSimple = false)
    : DOTGraphTraits<RegionNode*>(isSimple) {}

  static std::string getGraphName(const RegionInfo *) { return "Region Graph"; }

  std::string getNodeLabel(RegionNode *Node, RegionInfo *G) {
    return DOTGraphTraits<RegionNode *>::getNodeLabel(
        Node, reinterpret_cast<RegionNode *>(G->getTopLevelRegion()));
  }

  std::string getEdgeAttributes(RegionNode *srcNode,
                                GraphTraits<RegionInfo *>::ChildIteratorType CI,
                                RegionInfo *G) {
    RegionNode *destNode = *CI;

    if (srcNode->isSubRegion() || destNode->isSubRegion())
      return "";

    // In case of a backedge, do not use it to define the layout of the nodes.
    BasicBlock *srcBB = srcNode->getNodeAs<BasicBlock>();
    BasicBlock *destBB = destNode->getNodeAs<BasicBlock>();

    Region *R = G->getRegionFor(destBB);

    while (R && R->getParent())
      if (R->getParent()->getEntry() == destBB)
        R = R->getParent();
      else
        break;

    if (R && R->getEntry() == destBB && R->contains(srcBB))
      return "constraint=false";

    return "";
  }

  // Print the cluster of the subregions. This groups the single basic blocks
  // and adds a different background color for each group.
  static void printRegionCluster(const Region &R, GraphWriter<RegionInfo *> &GW,
                                 unsigned depth = 0) {
    raw_ostream &O = GW.getOStream();
    O.indent(2 * depth) << "subgraph cluster_" << static_cast<const void*>(&R)
      << " {\n";
    O.indent(2 * (depth + 1)) << "label = \"\";\n";

    if (!onlySimpleRegions || R.isSimple()) {
      O.indent(2 * (depth + 1)) << "style = filled;\n";
      O.indent(2 * (depth + 1)) << "color = "
        << ((R.getDepth() * 2 % 12) + 1) << "\n";

    } else {
      O.indent(2 * (depth + 1)) << "style = solid;\n";
      O.indent(2 * (depth + 1)) << "color = "
        << ((R.getDepth() * 2 % 12) + 2) << "\n";
    }

    for (const auto &RI : R)
      printRegionCluster(*RI, GW, depth + 1);

    const RegionInfo &RI = *static_cast<const RegionInfo*>(R.getRegionInfo());

    for (auto *BB : R.blocks())
      if (RI.getRegionFor(BB) == &R)
        O.indent(2 * (depth + 1)) << "Node"
          << static_cast<const void*>(RI.getTopLevelRegion()->getBBNode(BB))
          << ";\n";

    O.indent(2 * depth) << "}\n";
  }

  static void addCustomGraphFeatures(const RegionInfo *G,
                                     GraphWriter<RegionInfo *> &GW) {
    raw_ostream &O = GW.getOStream();
    O << "\tcolorscheme = \"paired12\"\n";
    printRegionCluster(*G->getTopLevelRegion(), GW, 4);
  }
};
} //end namespace llvm

namespace {

struct RegionInfoPassGraphTraits {
  static RegionInfo *getGraph(RegionInfoPass *RIP) {
    return &RIP->getRegionInfo();
  }
};

struct RegionPrinter
    : public DOTGraphTraitsPrinter<RegionInfoPass, false, RegionInfo *,
                                   RegionInfoPassGraphTraits> {
  static char ID;
  RegionPrinter()
      : DOTGraphTraitsPrinter<RegionInfoPass, false, RegionInfo *,
                              RegionInfoPassGraphTraits>("reg", ID) {
    initializeRegionPrinterPass(*PassRegistry::getPassRegistry());
  }
};
char RegionPrinter::ID = 0;

struct RegionOnlyPrinter
    : public DOTGraphTraitsPrinter<RegionInfoPass, true, RegionInfo *,
                                   RegionInfoPassGraphTraits> {
  static char ID;
  RegionOnlyPrinter()
      : DOTGraphTraitsPrinter<RegionInfoPass, true, RegionInfo *,
                              RegionInfoPassGraphTraits>("reg", ID) {
    initializeRegionOnlyPrinterPass(*PassRegistry::getPassRegistry());
  }
};
char RegionOnlyPrinter::ID = 0;

struct RegionViewer
    : public DOTGraphTraitsViewer<RegionInfoPass, false, RegionInfo *,
                                  RegionInfoPassGraphTraits> {
  static char ID;
  RegionViewer()
      : DOTGraphTraitsViewer<RegionInfoPass, false, RegionInfo *,
                             RegionInfoPassGraphTraits>("reg", ID) {
    initializeRegionViewerPass(*PassRegistry::getPassRegistry());
  }
};
char RegionViewer::ID = 0;


struct RegionViewer2 : public FunctionPass {
    static char ID;
    using AnalysisT = RegionInfo;
    using AnalysisGraphTraitsT = RegionInfoPassGraphTraits;
    using GraphT = RegionInfo *;

    RegionViewer2() : FunctionPass(ID) , Name("reg") {  
        initializeRegionViewer2Pass(*PassRegistry::getPassRegistry());
    }

    virtual bool processFunction(Function &F, AnalysisT &Analysis) {
        return true;
    }

    bool runOnFunction(Function &F) override {
       DominatorTree * DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
      //  PostDominatorTree* OrigPDT = &getAnalysis<PostDominatorTreeWrapperPass>().getPostDomTree();
        DominanceFrontier *DF = &getAnalysis<DominanceFrontierWrapperPass>().getDominanceFrontier();
       LoopInfo* LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

    //   PostDominatorTree PDT(/*Syntactical=*/true);
    //   PDT.recalculate(F);

#if 0
       for (Loop* TLL : *LI) {
           df_iterator_default_set<Loop*> Visited;
           for (Loop* L : depth_first_ext(TLL, Visited)) {
               SmallVector<BasicBlock*> ExitingBBs;
               L->getExitingBlocks(ExitingBBs);

               for (BasicBlock* Exiting : ExitingBBs) {
                 if (Exiting->size()<=1) continue;

                 // Split the branch such that if used as a loop break, no side-effect instructions would be executed
                 SplitBasicBlock();
               }
           }
       }
#endif



       PostDominatorTree SyntaxPDT(/*Syntactical=*/true);
       SyntaxPDT.recalculate(F);

       DenseMap<BasicBlock *, size_t> Costs;
       std::deque<std::pair< BasicBlock *,size_t>> Worklist;
       Worklist.emplace_back( &F.getEntryBlock(), 0u );
       while (!Worklist.empty()) {
           // llvm::stable_sort(Worklist, [](auto LHS, auto RHS) { return LHS.second < RHS.second; } );
           auto Pair = Worklist.front() ; Worklist.pop_front();
           auto BB = Pair.first;
           auto NewCost = Pair.second + BB->size();

           auto It = Costs.find(BB);
           if (It != Costs.end() && It->second < NewCost) {
               // New path is worse than old
               break;
           }
           Costs[BB] = NewCost;

           for (auto Succ: successors(BB)) 
               Worklist.emplace_back( Succ, NewCost );
                  }


       SmallVector<BasicBlock *> PDTRoots;
       llvm::append_range(PDTRoots, SyntaxPDT.roots());

       auto CostSorter= [&](BasicBlock* LHS, BasicBlock* RHS) -> bool {
           auto LIt = Costs.find(LHS);
           if (LIt == Costs.end())
               return false;
           auto RIt = Costs.find(RHS);
           if (RIt == Costs.end())
               return true;

           return   LIt->second >  RIt->second ;
       };

       llvm::stable_sort(PDTRoots,CostSorter );
       // Keep the last return as the one 'true' return;
      // PDTRoots.pop_back();

     
       SmallVector<PostDominatorTree::UpdateType> Updates;

      // do {
           bool Changed = false;


           for (BasicBlock* Root :  drop_begin(  PDTRoots,1)) {
               auto PDTNode = SyntaxPDT.getNode(Root);

               auto DTNode = DT->getNode(Root);
               while (DTNode) {
                   auto CandidBB = DTNode->getBlock();
                    if (!SyntaxPDT.dominates(PDTNode, SyntaxPDT.getNode(CandidBB) )) {
                        auto &Update = Updates.emplace_back(cfg::UpdateKind::Insert,Root, CandidBB);
                        //PostDominatorTree::UpdateType Update(cfg::UpdateKind::Insert, Root, CandidBB  );
                     
                       // Updates.emplace_back( );
                        Changed = true;
                        break;
                    }

                   DTNode = DTNode->getIDom();
               }

              // if (Changed ) break;
           }

       //    if (!Changed) break;
      // } while (true);

          SmallVector<std::pair<BasicBlock*,BasicBlock*>> ConsideredBreak;
#if 1
    // Find loops with multiple exits
           for (Loop* TLL : *LI) {
               df_iterator_default_set<Loop*> Visited;
           //    for (Loop* L : inverse_post_order_ext(TLL, Visited)) {
               for (Loop* L : depth_first_ext(TLL, Visited)) {
                   SmallVector<BasicBlock*> ExitBBs;
                   L->getUniqueExitBlocks(ExitBBs);
                //   if (ExitBBs.size()<2) continue;

                 
                // auto MainExit =   ExitBBs.pop_back_val();

                 for (BasicBlock* Exit : ExitBBs) {
                     SetVector<BasicBlock*> ExitingBBs;
                     for (auto* Exiting : predecessors(Exit)) {
                         if (L->contains(Exiting))
                             ExitingBBs.insert(Exiting);
                     }  
                     if (ExitingBBs.size()<2) continue;
                     auto ExitingBBVec = ExitingBBs.takeVector();
                     llvm::stable_sort(ExitingBBVec,CostSorter);
                     std::reverse(ExitingBBVec.begin(), ExitingBBVec.end());

                     // Only consider a BB executed unconditionally as main break
                     auto Header = L->getHeader();
                     auto Latch = L->getLoopLatch();
                     if (!Latch) continue;

                     BasicBlock *MainBreak=nullptr;
                     for (BasicBlock* Exiting : ExitingBBVec) {
                       if (! DT->dominates(Exiting, Latch)) continue;
                       MainBreak = Exiting;
                       break;
                     }

                     if (!MainBreak) continue;

#if 0
                     // Ensure that break is side-effect free
                     auto MainBreak = ExitingBBVec.front();
                     if (MainBreak->size() != 1 || isa<InvokeInst>( MainBreak->getTerminator())) 
                         continue;
#endif

                     for (BasicBlock* Exiting : reverse( ExitingBBVec)) {
                         if (Exiting == MainBreak)
                             continue;
#if 1
                         auto & UpdateRemove = Updates.emplace_back(cfg::UpdateKind::Delete, Exiting, Exit);
                         Changed = true; 
                         ConsideredBreak.emplace_back(Exiting,Exit);
#else
                         auto PDTNode = SyntaxPDT.getNode(Exiting);
                         auto DTNode = DT->getNode(Exiting);
                         while (DTNode) {
                             auto CandidBB = DTNode->getBlock();
                             if (!L->contains(CandidBB)) break;
                             if (!SyntaxPDT.dominates(PDTNode, SyntaxPDT.getNode(CandidBB))) {
                                 auto & UpdateRemove = Updates.emplace_back(cfg::UpdateKind::Delete, Exiting, Exit);
                                 auto& UpdateInsert = Updates.emplace_back(cfg::UpdateKind::Insert, Exiting, CandidBB);
                                 Changed = true; ConsideredBreak.emplace_back(Exiting,Exit);
                                 break;
                             }

                             DTNode = DTNode->getIDom();
                         }
#endif
                     }
                 }
               }
           }
#endif


     // Can only apply once, a second call would assume that `Updates` has already been applied to the CFG.
     SyntaxPDT.applyUpdates({}, Updates);

#if 0
       auto VRoot = PDT.getRootNode();
       do {
           bool Changed = false;
           auto NumRoots = VRoot->getNumChildren();

           SmallVector<DomTreeNode*> MainReturnCandidates;
           for (int i = 0; i < NumRoots ; ++i) {
               DomTreeNode* PRoot = VRoot->begin()[i];
               BasicBlock *BB  = PRoot->getBlock();
               MainReturnCandidates.push_back(PRoot);
           }

           llvm::stable_sort(MainReturnCandidates, [&](DomTreeNode* LHS, DomTreeNode* RHS) -> bool {
               auto RIt  = Costs.find(RHS->getBlock());
               if (RIt == Costs.end())
                   return false;
               auto LIt  = Costs.find(LHS->getBlock());
               if (LIt == Costs.end())
                   return true;
               return LIt->second < RIt->second;
               } );




           for (int i = 0; i < NumRoots && !Changed;++i) {
               DomTreeNode* PRoot = MainReturnCandidates[i];
               DomTreeNode* DNode = DT->getNode(PRoot->getBlock());
               while (DNode) {
                   auto BBCandidate = DNode->getBlock();
                   auto PDomCondidate = PDT.getNode(BBCandidate);
                   if (!PDT.dominates(PRoot, PDomCondidate)) {
                       PRoot->setIDom(PDomCondidate);
                       Changed = true;
                       break;
                   }

                   // Try next dominator
                   DNode = DNode->getIDom();
               }
           }
           if (!Changed) break;
       } while (true);
#endif

       // auto &Analysis = getAnalysis<AnalysisT>();
        AnalysisT Analysis(/*Syntactical=*/true);
        Analysis.releaseMemory();
        Analysis.recalculate(F, DT, &SyntaxPDT, DF, ConsideredBreak);


        if (!processFunction(F, Analysis))
            return false;

      //  GraphT Graph = AnalysisGraphTraitsT::getGraph(&Analysis);
        GraphT Graph = &Analysis;
        std::string GraphName = DOTGraphTraits<GraphT>::getGraphName(Graph);
        std::string Title = GraphName + " for '" + F.getName().str() + "' function";

        ViewGraph(Graph, Name, /*IsSimple*/false, Title);

        return false;
    }

    void getAnalysisUsage(AnalysisUsage &AU) const override {
        AU.setPreservesAll();
      //  AU.addRequired<AnalysisT>();
        AU.addRequired<DominatorTreeWrapperPass>();
      //  AU.addRequired<PostDominatorTreeWrapperPass>();
      AU.addRequired<DominanceFrontierWrapperPass>();
     AU.addRequired<LoopInfoWrapperPass>();
    }

private:
    std::string Name;
    // bool Syntactical;
};
char RegionViewer2::ID = 0;


struct RegionOnlyViewer
    : public DOTGraphTraitsViewer<RegionInfoPass, true, RegionInfo *,
                                  RegionInfoPassGraphTraits> {
  static char ID;
  RegionOnlyViewer()
      : DOTGraphTraitsViewer<RegionInfoPass, true, RegionInfo *,
                             RegionInfoPassGraphTraits>("regonly", ID) {
    initializeRegionOnlyViewerPass(*PassRegistry::getPassRegistry());
  }
};
char RegionOnlyViewer::ID = 0;

} //end anonymous namespace

INITIALIZE_PASS(RegionPrinter, "dot-regions",
                "Print regions of function to 'dot' file", true, true)

INITIALIZE_PASS(
    RegionOnlyPrinter, "dot-regions-only",
    "Print regions of function to 'dot' file (with no function bodies)", true,
    true)

INITIALIZE_PASS(RegionViewer, "view-regions", "View regions of function",
                true, true)

INITIALIZE_PASS(RegionViewer2, "view-regions2", "View regions of function2",   true, true)

INITIALIZE_PASS(RegionOnlyViewer, "view-regions-only",
                "View regions of function (with no function bodies)",
                true, true)

FunctionPass *llvm::createRegionPrinterPass() { return new RegionPrinter(); }

FunctionPass *llvm::createRegionOnlyPrinterPass() {
  return new RegionOnlyPrinter();
}

FunctionPass* llvm::createRegionViewerPass() {
  return new RegionViewer();
}

FunctionPass* llvm::createRegionOnlyViewerPass() {
  return new RegionOnlyViewer();
}

#ifndef NDEBUG
static void viewRegionInfo(RegionInfo *RI, bool ShortNames) {
  assert(RI && "Argument must be non-null");

  llvm::Function *F = RI->getTopLevelRegion()->getEntry()->getParent();
  std::string GraphName = DOTGraphTraits<RegionInfo *>::getGraphName(RI);

  llvm::ViewGraph(RI, "reg", ShortNames,
                  Twine(GraphName) + " for '" + F->getName() + "' function");
}

static void invokeFunctionPass(const Function *F, FunctionPass *ViewerPass) {
  assert(F && "Argument must be non-null");
  assert(!F->isDeclaration() && "Function must have an implementation");

  // The viewer and analysis passes do not modify anything, so we can safely
  // remove the const qualifier
  auto NonConstF = const_cast<Function *>(F);

  llvm::legacy::FunctionPassManager FPM(NonConstF->getParent());
  FPM.add(ViewerPass);
  FPM.doInitialization();
  FPM.run(*NonConstF);
  FPM.doFinalization();
}

void llvm::viewRegion(RegionInfo *RI) { viewRegionInfo(RI, false); }

void llvm::viewRegion(const Function *F) {
  invokeFunctionPass(F, createRegionViewerPass());
}

void llvm::viewRegionOnly(RegionInfo *RI) { viewRegionInfo(RI, true); }

void llvm::viewRegionOnly(const Function *F) {
  invokeFunctionPass(F, createRegionOnlyViewerPass());
}
#endif
