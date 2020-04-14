#ifndef LLVM_LOF_LOOPTREECODEGEN_H
#define LLVM_LOF_LOOPTREECODEGEN_H

#include "Green.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"


namespace llvm {
  namespace lof {

    class GreenCodeGen {
      using BuilderTy = IRBuilder<>;
    private:
      Green* Root;
      Module* M;
      LLVMContext& LLVMCtx;

      Function* Func=nullptr;
      BuilderTy AllocaBuilder;
      DenseMap<GSymbol*, Value*> SymbolPtrs;

      void emitSequence(Green* G, BuilderTy& Builder);
      void emitLoop(Green* G, BuilderTy& Builder);
      Value*emitOperation(const  Operation& Op, ArrayRef<GExpr*> Arguments, BuilderTy& Builder, bool IsExpr);
      Value*emitExpr(GExpr* G, BuilderTy& Builder);
      void emitInstruction(Green* G, BuilderTy& Builder);
      void emitGreen(GCommon* G, BuilderTy& Builder);

      Value* getPtr(GSymbol* Sym);

    public:
      GreenCodeGen(Green *Root,Module *M, LLVMContext &C) :Root(Root), M(M), AllocaBuilder(C), LLVMCtx(C) {}

      std::tuple< Function*, std::vector<GSymbol*>, std::vector<GSymbol*> > emitAsFunction();

      void replaceOrig(Green* Orig);
    };

  } // namespace lof
} // namespace llvm
#endif /* LLVM_LOF_LOOPTREECODEGEN_H */
