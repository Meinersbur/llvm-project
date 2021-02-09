#include "LoopTransform.h"
#include "GreenBuilder.h"

using namespace lof;

namespace {

struct UnrollAndJamTransformer {
  LoopContext &Ctx;
  int Factor;
  GSymbol *OrigCounter;
  SmallVector<GExpr *, 8> UnrolledCounters;

  UnrollAndJamTransformer(LoopContext &LoopCtx, int Factor)
      : Ctx(LoopCtx), Factor(Factor) {}

  void jamBuilder(Green *G, GreenBuilder &Builder) {

    SmallVector<Green *, 8> Seq;
    SmallVector<GExpr *, 8> SeqCond;

    auto UnrollSeq = [&]() {
      auto NumSeq = Seq.size();

      for (int f = 0; f < Factor; f += 1) {
        Builder.addAssignment(StringRef(), Ctx.getTrue(), OrigCounter,
                              UnrolledCounters[f]);
        for (int i = 0; i < NumSeq; i += 1) {
          auto C = Seq[i];
          auto Cond = SeqCond[i];
          Builder.addStmt(Cond, C);
        }
      }

      Seq.clear();
      SeqCond.clear();
    };

    auto NumChildren = G->getNumChildren();

    for (int i = 0; i < NumChildren; i += 1) {
      auto C = G->getSubStmt(i);
      auto Cond = G->getSubCond(i);

      if (!C->isLoop()) {
        assert(C->isInstruction());
        Seq.push_back(C);
        SeqCond.push_back(Cond);
        continue;
      }

      UnrollSeq();

      // jam loop
      auto Jammed = jam(C);
      Builder.addStmt(Cond, Jammed);
    }
    UnrollSeq();
  }

  Green *jam(Green *G) {
    GreenBuilder Builder(Ctx);

    jamBuilder(G, Builder);

    // Copy loop
    return Builder.createLoop(StringRef(), Builder.getTrue(),
                              G->getOrigRange().first, G->getOrigRange().second,
                              G->getIsFirstIteration(),
                              G->getCanonicalCounter());
  }

  Green *unroll(Green *G) {
    GreenBuilder Builder(Ctx);

    OrigCounter = G->getCanonicalCounter();
    assert(OrigCounter);
    auto NewCnt = Builder.createSymbolFromScratch("unroll.cnt", nullptr);
    auto NewIsFirst =
        Builder.createSymbolFromScratch("unroll.isfirst", nullptr);

    UnrolledCounters.set_size(Factor);
    for (int f = 0; f < Factor; f += 1) {
      auto FConst = Builder.getConst(f);
      UnrolledCounters[f] = GOpExpr::createOp(
          Operation(Operation::Add, nullptr), {NewCnt, FConst});
    }

    jamBuilder(G, Builder);
    return Builder.createLoop(StringRef(), Builder.getTrue(),
                              G->getOrigRange().first, G->getOrigRange().second,
                              NewIsFirst, NewCnt);
  }

}; //  class UnrollAndJamTransformer

} // namespace

Green *lof::applyUnrollAndJam(LoopContext &LoopCtx, Green *G, int Factor) {
  assert(Factor >= 1);
  assert(G->isLoop());

  UnrollAndJamTransformer Transformer(LoopCtx, Factor);
  return Transformer.unroll(G);
}
