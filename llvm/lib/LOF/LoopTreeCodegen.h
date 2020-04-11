#ifndef LLVM_LOF_LOOPTREECODEGEN_H
#define LLVM_LOF_LOOPTREECODEGEN_H

#include "Green.h"
#include "llvm/IR/IRBuilder.h"

namespace llvm {
  namespace lof {




    class GreenCodeGen {
      using BuilderTy = IRBuilder<>;
    private:
      Green* Root;
      BuilderTy AllocaBuilder;
      Module* M;
      LLVMContext& C;

      DenseMap<GSymbol*, Value*> SymbolPtrs;


      void emitSequence(Green*G, BuilderTy& Builder) {
        assert(G->isContainer());
        for (auto C :  G->children()) {
          emitGreen(C, Builder);
        }
        llvm_unreachable("unimplemented");
      }

      void emitLoop(Green*G, BuilderTy& Builder) {
        // TODO: create loop frame
        emitSequence(G, Builder);
        llvm_unreachable("unimplemented");
      }

      Value* emitOperation(const  Operation& Op, ArrayRef<GExpr*> Arguments, BuilderTy& Builder, bool IsExpr) {
        switch (Op.getKind()) {

        case Operation::LLVMInst:
          assert(!IsExpr);
          LLVM_FALLTHROUGH
        case Operation::LLVMFloating:{
            auto I = Op.getLLVMInst();
            if (auto C = dyn_cast<Constant>(I)) {
              return C;
            } else if (auto Inst = dyn_cast<Instruction>(I)) {
              auto Copy = Inst->clone();
              for (auto P :  enumerate( Arguments)) {
                auto i = P.index();
                auto GArg = P.value();
                auto NewVal = emitExpr(GArg, Builder);
                Copy->setOperand(i,NewVal );
              }
              Builder.Insert(Copy);
              return Copy;
            }
            else
              llvm_unreachable("unimplemented");
          } break;
        default:
          llvm_unreachable("unimplemented");
        }
      }

      Value* emitExpr(GExpr* G, BuilderTy& Builder) {
        if (auto Reg = dyn_cast<GRefExpr>(G)) {
          assert(SymbolPtrs.count(Reg));
          auto Ptr= SymbolPtrs.lookup(Reg);
          return Builder.CreateLoad(Ptr);
        } else if ( auto OpE = dyn_cast< GOpExpr >(G) ) {
          auto Op = OpE->getOperation();
          auto Result = emitOperation( Op, OpE->getArguments() , Builder, true);
          return Result;
        } else
          llvm_unreachable("unimplemented");
      }

      void emitInstruction(Green*G, BuilderTy& Builder) {
        assert(G->isInstruction());

        auto Op = G->getOperation();
        auto NewInst=  emitOperation(Op, G->getArguments(), Builder, false);

        assert(Op.getNumOutputs() <= 1);
        assert(Op.getNumOutputs() == G->getAssignments().size());
        if (Op.getNumOutputs()) {
          auto DstSym = G->getAssignments()[0];
          auto DstPtr = SymbolPtrs.lookup(DstSym);
          assert(DstPtr);
          Builder.CreateStore( NewInst, DstPtr );
        }
      }

      void emitGreen(GCommon*G, BuilderTy& Builder) {
        if (G->isInstruction()) {
          emitInstruction(cast<Green>(G), Builder);
        } else if (G->isLoop()) {
          emitLoop(cast<Green>(G), Builder);
        }    else if (G->isContainer()) {
          emitLoop(cast<Green>(G), Builder);
        } else 
          llvm_unreachable("Not yet implemented");
      }

    public:
      GreenCodeGen(Green *Root,Module *M, LLVMContext &C) :Root(Root), M(M), AllocaBuilder(C), C(C) {}


      Function* emitAsFunction() {
        DenseSet<GSymbol*> Reads;
        DenseSet<GSymbol*> Kills;
        DenseSet<GSymbol*> Writes;
        DenseSet<GSymbol*> AllReferences;
        Root->determineScalars(Reads, Kills, Writes, AllReferences);

        DenseSet<GSymbol*> Outside;
        Outside.insert(Reads.begin(), Reads.end() );
        Outside.insert(Writes.begin(),Writes.end() );

        SmallVector<Type*, 8 > ParamsTypes;
        SmallVector<GSymbol*,8> OutsideVec(Outside.begin(), Outside.end());
        for (auto V : OutsideVec) {
          ParamsTypes.push_back(PointerType::get(V->getType(), 0));
        }

        auto Func = Function::Create(FunctionType::get( Type::getVoidTy(C), ParamsTypes, false ), GlobalValue::InternalLinkage  , "", M);

        for (auto A : llvm::zip( OutsideVec,  Func->args() )) {
          auto Sym =std:: get<0>(A);
          auto Ptr = &std:: get<1>(A);
          assert(Sym->getType() == Ptr->getType()->getPointerElementType());
          SymbolPtrs[Sym] = Ptr;
        }

        auto AllocaBB = BasicBlock::Create(C, "func.alloca", Func);
        auto EntryBB = BasicBlock::Create(C, "func.entry", Func);


        AllocaBuilder.SetInsertPoint(AllocaBB);
        for (auto R : AllReferences) {
          auto& Ptr = SymbolPtrs[R];
          if (!Ptr) {
            Ptr = AllocaBuilder.CreateAlloca(R->getType());
          }
        }

        AllocaBuilder.CreateBr(EntryBB);
        AllocaBuilder.SetInsertPoint(AllocaBB, AllocaBB->getFirstInsertionPt());

        BuilderTy Builder(EntryBB);
        Builder.SetInsertPoint(EntryBB);

        emitGreen(Root, Builder);

        auto ExitBB = BasicBlock::Create(C, "func.exit", Func);
        Builder.CreateBr(ExitBB);
        Builder.SetInsertPoint(ExitBB);
        Builder.CreateRetVoid();

        return Func;
      }
    };




  } // namespace lof
} // namespace llvm

#endif /* LLVM_LOF_LOOPTREECODEGEN_H */
