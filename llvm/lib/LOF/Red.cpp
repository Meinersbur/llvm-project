#include "Red.h"

using namespace lof;


ArrayRef<Red*> Red:: getChildren() {
  auto NumChildren = getNumChildren();
  if (NumChildren == 0)
    return {};
  for (int i = 0; i < NumChildren; i += 1)
    getChild(i);
  return ArrayRef<Red*>(&Children[0], NumChildren);
}


Red* Red::getChild(size_t i) {
  auto NumChildren = getNumChildren();
  assert(0 <= i && i < NumChildren);
  if (!Children) 
    Children.reset(new Red*[NumChildren]);
  auto& Child = Children[i];
  if (!Child) 
    Child = new Red(G->getChild(i), this, i);
  return Child;
}

