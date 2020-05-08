#include "Red.h"

using namespace lof;

ArrayRef<Red*> Red:: getChildren() {
  auto NumChildren = getNumChildren();
  if (NumChildren == 0)
    return {};
  if (!Children) {
    Children.reset(new Red*[NumChildren]);
    for (auto P : llvm::enumerate(G->children())) {
      P.value();
      auto New = new Red(P.value(), this, P.index());
      Children.get()[P.index()] = New;
    }
  }
  return ArrayRef<Red*>(&Children[0], NumChildren);
}

