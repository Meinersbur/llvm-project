#include "Dep.h"
#include "Red.h"
#include "LoopTreeTransform.h"

using namespace lof;


class JohnnyDep : public Dep {
private:
  bool IsScalar = true;

  Red* Src;
  Red* Dst;

public:
  JohnnyDep(Red* Src, Red* Dst) : Src(Src), Dst(Dst) {}


  bool isScalar() const override { return IsScalar; }

  Red* getSrc() const { return Src; }
  Red* getDst() const { return Dst; }

}; // class JohnnyDep


static void collectRedInstructions(Red* R, std::vector<Red*>&Result) {
  auto G = R->getGreen();
  if (G->isInstruction()) {
    Result.push_back(R);
    return;
  }

  for (auto C : R->getChildren()) {
    collectRedInstructions(C, Result);
  }
}




/// Collect all red instructions
static std::vector<Red*> getAllRedInstructions(GCommon* G) {
  std::vector<Red*> Result;
  collectRedInstructions(G->asRedRoot(), Result);
  return Result;
}


std::vector<Dep*>lof:: getAllDependencies(Green *Root)  {
  std::vector<Dep*> Result;
  std::vector<Red*> Reds = getAllRedInstructions(Root);
  auto N = Reds.size();

  DenseMap<GSymbol*, SmallVector<Red*, 2> > Reads;
  DenseMap<GSymbol*, SmallVector<Red*, 2> > Writes;

  for (int i = 0; i < N; i++) {
    auto Src = Reds[i];
    auto GSrc =  cast<Green>( Src->getGreen());
    for (int j = 0; j < i; j++) {
      auto Dst = Reds[j];
      auto GDst = cast<Green>(Dst->getGreen());

      auto SrcWrites = GSrc->getScalarWrites();
      auto SrcReads = GSrc->getScalarReads();
      auto DstReads = GDst->getScalarReads();
      auto DstWrites = GDst->getScalarWrites();

      for (auto Def : SrcWrites) {
        Result.push_back(new JohnnyDep(Src, Dst));
      }

    }
  }

  return Result;
}


namespace {

  struct DepEncounter {
    bool EncounteredSrc = false;
    bool EncounteredDst = false;
    bool Violation = false;
  };

  // TODO: Checks only a singled dependence; must be much more efficient
  class CheckDepVisitor final : public RedRecursiveVisitor<bool, DepEncounter> {
  private:
    Red* Src;
    Red* Dst;

    bool EncounteredSrc = false;
    bool EncounteredDst = false;
    bool Passed = true;

  private:
    bool isRepresenting(Red* Orig, Red* Derived) {
      auto RepresentingG = cast<Green>( Orig->getGreen());
      assert(!RepresentingG->getTransformationOf()  && "Assuming the dependency is from the original tree without reused nodes (i.e. a tree, not a DAG)");

      auto DerivedG = cast<Green>(Derived);
      while (DerivedG->getTransformationOf()) {
        DerivedG = DerivedG->getTransformationOf();
      }

      return RepresentingG == DerivedG;
    }


  public:
    CheckDepVisitor(Red* Src, Red* Dst):Src(Src), Dst(Dst) {
      assert(Src->isInstruction());
      assert(Dst->isInstruction());
    }


    bool hasPassed() { return Passed; }

    DepEncounter previsitInstruction(Red* Node, DepEncounter& NodeParentData) override { 
        DepEncounter Result;
        Result.EncounteredSrc = isRepresenting(Src, Node);
        Result.EncounteredDst = isRepresenting(Dst, Node);
        Result.Violation = false;
        return Result;
     }




    DepEncounter previsitContainer(Red* Node, DepEncounter& NodeParentData)  override {
       if (Node->isLoop()) {
         // ...
       }

       DepEncounter Result;
       Result.EncounteredSrc = false;
       Result.EncounteredDst = false;
       Result.Violation = false;
       return Result;
     }
   


  }; // class CheckDepVisitor

} // anon namespace 




bool lof::checkDependencies(Green* NewRoot, ArrayRef<Dep*> Deps) {
      
  for (auto D : Deps) {
    auto Dep = cast<JohnnyDep>(D);
    CheckDepVisitor Checker(Dep->getSrc(), Dep->getDst());

  }


  return true;
}



