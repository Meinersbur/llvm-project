#ifndef LLVM_LOF_RED_H
#define LLVM_LOF_RED_H

#include "llvm/LOF/Green.h"

namespace lof {
class Red;

class red_child_iterator
    : public map_index_iterator<red_child_iterator, Red *, Red *,
                                std::ptrdiff_t, Red **, Red *> {
public:
  red_child_iterator(Red *Parent, size_t Idx)
      : map_index_iterator(Parent, Idx) {}

public:
  Red *operator*() const;
}; // class red_child_iterator
} // namespace lof

namespace llvm {
template <> struct GraphTraits<lof::Red *> {
  // Red as graph node; enumerate children
  using NodeRef = lof::Red *;
  using ChildIteratorType = lof::red_child_iterator;
  static inline ChildIteratorType child_begin(NodeRef N);
  static inline ChildIteratorType child_end(NodeRef N);

  // Red as graph representing its subtree; enumerate all nodes in subtree
  using GraphRef = lof::Red *;
  using nodes_iterator = df_iterator<NodeRef, df_iterator_default_set<NodeRef>,
                                     false, GraphTraits<NodeRef>>;
  static inline nodes_iterator nodes_begin(lof::Red *R);
  static inline nodes_iterator nodes_end(lof::Red *R);
  static NodeRef getEntryNode(lof::Red *R) { return R; }
}; // template specialization GraphTraits<lof::Red*>
} // namespace llvm

namespace lof {

struct ScalarPossibleUses {
  SmallVector<Red *, 4> Nodes;
  bool Bottom;
};

class Red {
private:
  GCommon *G;
  Red *Parent;
  int ParentIdx;

private:
  Red(GCommon *G, Red *Parent, int ParentIdx) : G(G), Parent(Parent) {
    assert(G);
    assert(!G->isStaging());
    if (Parent) {
      assert(ParentIdx >= 0);
      assert(ParentIdx < Parent->getNumChildren());
      // TODO: Check if G is one of Parent's nodes
    }
  }

public:
  static Red *createRoot(GCommon *Root) { return new Red(Root, nullptr, -1); }

public:
  bool isRoot() const { return Parent == nullptr; }
  Red *getRoot() const {
    auto Result = this;
    while (!Result->isRoot())
      Result = Result->getParent();
    return const_cast<Red *>(Result);
  }
  Red *getParent() const { return Parent; }
  size_t getParentIdx() const { return ParentIdx; }

  size_t getNumChildren() const { return G->getNumChildren(); }
  GCommon *getGreen() const { return G; }

private:
  // TODO: use llvm::TrailingObjects
  std::unique_ptr<Red *[]> Children;

public:
  Red *getChild(size_t i);
  ArrayRef<Red *> getChildren();
  auto children() {
    return make_range(red_child_iterator(this, 0),
                      red_child_iterator(this, getNumChildren()));
  }
  auto childen_ref() { return llvm::make_pointee_range(children()); }

public:
  bool isContainer() const { return G->isContainer(); }
  bool isInstruction() const { return G->isInstruction(); }
  bool isExpr() const { return G->isExpr(); }

  bool isStmt() const { return G->isStmt(); }
  bool isLoop() const { return G->isLoop(); }
  bool isRefExpr() const { return isa<GRefExpr>(G); }

private:
  /// For GRefExpr only
  std::vector<std::pair<Red *, int>> ReachableDef;
  bool TopIsReachableDef;

  /// For instructions only, one per assignment
  std::unique_ptr<ScalarPossibleUses[]> PossibleUses;

public:
  ArrayRef<std::pair<Red *, int>> getReachablesDefs() { return ReachableDef; }

public:
  void internal_setReachableDefs(ArrayRef<std::pair<Red *, int>> ReachableDefs,
                                 bool TopIsReachableDef) {
    this->ReachableDef = ReachableDefs;
    this->TopIsReachableDef = TopIsReachableDef;
  }

  void internal_addPossibleUse(int Idx, Red *Use) {
    if (!PossibleUses) {
      PossibleUses = std::make_unique<ScalarPossibleUses[]>(
          cast<Green>(G)->getNumChildren());
    }
    PossibleUses[Idx].Nodes.push_back(Use);
  }

public:
#if !defined(NDEBUG) || defined(LLVM_ENABLE_DUMP)
  LLVM_DUMP_METHOD void dump() const;
#endif

public:
  auto dfs() { return llvm::depth_first(this); }

  template <typename SetTy> auto dfs(SetTy &Visited) {
    Visited.clear();
    return llvm::depth_first_ext(this, Visited);
  }

  std::vector<Red *> collectSubnodes(std::function<bool(Red *)> Pred);

  std::vector<Red *> collectRedExprs() {
    return collectSubnodes([](Red *R) { return R->isRefExpr(); });
  }

}; // class Red

inline Red *red_child_iterator::operator*() const {
  return Container->getChild(Idx);
}

} // namespace lof

namespace llvm {
GraphTraits<lof::Red *>::ChildIteratorType
GraphTraits<lof::Red *>::child_begin(NodeRef N) {
  return N->children().begin();
}
GraphTraits<lof::Red *>::ChildIteratorType
GraphTraits<lof::Red *>::child_end(NodeRef N) {
  return N->children().end();
}

GraphTraits<lof::Red *>::nodes_iterator
GraphTraits<lof::Red *>::nodes_begin(lof::Red *R) {
  return nodes_iterator::begin(getEntryNode(R));
}
GraphTraits<lof::Red *>::nodes_iterator
GraphTraits<lof::Red *>::nodes_end(lof::Red *R) {
  return nodes_iterator::end(getEntryNode(R));
}
} // namespace llvm

namespace lof {
class RedDumper {
private:
  llvm::raw_ostream &OS;
  const Red *Highlight;
  bool OnlyStmts;
  int Indent = 0;

  void dumpGreen(Red *R, GCommon *G, StringRef Role) {
    if (R == Highlight) {
      OS << ">>";
      if ((Indent * 2 - 2) > 0)
        OS.indent(Indent * 2 - 2);
    } else {
      OS.indent(Indent * 2);
    }
    if (!Role.empty())
      OS << Role << ": ";
    G->printLine(OS);
    OS << '\n';
  }

  void dumpNode(Red *Node, StringRef Role) {
    dumpGreen(Node, Node->getGreen(), Role);

    Indent += 1;
    if (Node->isContainer()) {
      for (auto Child : Node->children()) {
        dumpNode(Child, StringRef());
      }

#if 0
        auto G = cast<Green>(Node);
        auto NumChildren = G->getNumChildren();
        for (int i = 0; i < NumChildren; i += 1) {
          auto Cond = G->getSubCond(i);
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
#endif
    } else if (Node->isInstruction()) {
      if (!OnlyStmts) {
        auto G = cast<Green>(Node->getGreen());
        for (auto Dst : G->getAssignments()) {
          dumpGreen(nullptr, Dst, "Assign");
        }
        for (auto Arg : Node->children()) {
          dumpNode(Arg, "Arg");
        }
      }
    } else if (Node->isExpr()) {
      if (!OnlyStmts) {
        for (auto Arg : Node->children()) {
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
  RedDumper(llvm::raw_ostream &OS, bool OnlyStmts = false,
            const Red *Highlight = nullptr)
      : OS(OS), Highlight(Highlight), OnlyStmts(OnlyStmts) {}

  void dump(Red *Node) { dumpNode(Node, StringRef()); }

}; // class RedDumper

} // namespace lof

#endif /* LLVM_LOF_RED_H */
