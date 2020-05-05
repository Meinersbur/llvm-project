#include "LoopTransform.h"
#include "GreenBuilder.h"


using namespace lof;


static Green* jamLoop(Green* G) {
}

static Green* interleaveInstruction(Green* G) {
}

static Green* jam(Green* G) {
  GreenBuilder Builder;
  SmallVector<Green*, 4> Seq;

  for (auto C : G->children()) {
    if (!C->isLoop()) {
      Seq.push_back(C);
      continue;
    }

    if (!Seq.empty()) {
      // unroll sequence
    }

    // jam loop
    auto Jammed = jam(C);
    Builder.addStmt( GOpExpr::createTrueExpr() ,  Jammed );
  }
  return nullptr;
}

Green* lof:: applyUnrollAndJam(Green* G, int Factor) {
  assert(Factor >= 1);
  assert(G->isLoop());

  if (Factor == 1) {
    return G;
  }

  jam(G);

  GreenBuilder Builder;

auto Result =   Builder.createLoop( GOpExpr::createTrueExpr(), nullptr, nullptr );
return Result;
}
