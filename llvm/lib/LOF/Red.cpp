#include "Red.h"

using namespace lof;

ArrayRef<Red *> Red::getChildren() {
  auto NumChildren = getNumChildren();
  if (NumChildren == 0)
    return {};
  for (int i = 0; i < NumChildren; i += 1)
    getChild(i);
  return ArrayRef<Red *>(&Children[0], NumChildren);
}

Red *Red::getChild(size_t i) {
  auto NumChildren = getNumChildren();
  assert(0 <= i && i < NumChildren);
  if (!Children) {
    Children = std::make_unique<Red *[]>(NumChildren);
    //  Children.reset(new Red * [NumChildren]());
  }
  auto &Child = Children[i];
  if (!Child)
    Child = new Red(G->getChild(i), this, i);
  return Child;
}

LLVM_DUMP_METHOD void Red::dump() const {
  RedDumper Dumper(llvm::errs(), isStmt(), this);
  Dumper.dump(getRoot());
}

std::vector<Red *> Red::collectSubnodes(std::function<bool(Red *)> Pred) {
  llvm::df_iterator_default_set<Red *> Visited;
  auto Range = llvm::make_filter_range(dfs(Visited), Pred);
  std::vector<Red *> Result(Range.begin(), Range.end());
  return Result;
}
