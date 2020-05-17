#include "LoopTreeTransform.h"
#include "GreenBuilder.h"

using namespace lof;


Green* GreenTreeTransform::transfromStmtOrLoop(Green* Node, bool IsLoop) {
  assert(IsLoop == Node->isLoop());

  SmallVector<Green*, 8> RecreatedChildren;
  SmallVector<GExpr*, 8> RecreatedConds;
  bool Changed = false;
  auto NumChildren = Node->getNumChildren();
  for (int i = 0; i < NumChildren;i+=1) {
    auto Child = Node->getChild(i);
    auto Cond = Node->getSubCond(i);
    auto New = getDerived().visit(Child);
    RecreatedChildren.push_back(cast<Green>(New));
    auto NewCond = getDerived().visit(Cond);
    RecreatedConds.push_back(cast<GExpr>(NewCond));
    if (New != Child || Cond != NewCond)
      Changed = true;
  }

  GExpr* NewExecCond;
  if (IsLoop) {
    auto  ExecCond = Node->getExecCond();
    NewExecCond = cast<GExpr>(getDerived().visit(ExecCond ));
    if (ExecCond != NewExecCond)
      Changed = true;
  }

  if (!Changed)
    return Node;

  GreenBuilder Builder(Ctx);
  Builder.setTransformationOf(Node);

  for (int i = 0; i < NumChildren; i += 1) {
    Builder.addStmt(RecreatedConds[i], RecreatedChildren[i] );
  }
  
  if (Node->hasComputedScalars()) {
    // Needs recomputed?
    Builder.setScalarKills(Node->getScalarKills());
    Builder.setScalarReads(Node->getScalarReads());
    Builder.setScalarWrites(Node->getScalarWrites());
  }

  if (IsLoop)
    return Builder.createLoop(NewExecCond, Node->getOrigRange().first, Node->getOrigRange().second ,  Node->getCanonicalCounter() );
  return Builder.createStmt(Node->getOrigRange().first, Node->getOrigRange().second);
}
