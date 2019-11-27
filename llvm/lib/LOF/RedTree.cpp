#include "RedTree.h"

using namespace llvm;




void RedRoot::assignDefinitions(RedNode *Node, DenseMap<Value*,RedSet*> &PastDefinitions) {
  if (auto Set = dyn_cast<RedSet>(Node)) 
    PastDefinitions[Set->getVar()] = Set;
  
  if (auto Reg = dyn_cast<RedReg>(Node)) {
    auto Def = PastDefinitions.lookup(Reg->getVar());
    Reg->Def = Def;
  }

  // This expands the entire tree, which should not be necessary.
  for (auto Child : Node->children()) {
    assignDefinitions(Child, PastDefinitions);
  }
}




void RedRoot:: findAllDefinitions(){
  if (AllDefsFound)
    return;

  DenseMap<Value*,RedSet*> PastDefinitions;
  assignDefinitions(this,PastDefinitions);
  AllDefsFound = true;
}


bool RedRoot::hasDirectDependence(RedNode *Pred, RedNode *Succ ){
  findAllDefinitions();

    if (isa<RedSet>(Pred)  && isa<RedReg>(Succ))
      if (cast<RedReg>(Succ)->getDef() == Pred)
        return true;

    return false;
}


 RedNode *RedNode::Create(RedNode*Parent,const  GreenNode *Green) {
   switch (Green->getKind()) {
   case LoopHierarchyKind::Reg:
     return RedReg::Create(Parent,cast<GreenReg>(Green) );
   default:  
     return new RedNode(Parent, Green);
   }
}
