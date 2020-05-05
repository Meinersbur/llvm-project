#ifndef LLVM_LOF_GREEN_BUILDER_H
#define LLVM_LOF_GREEN_BUILDER_H

#include "Green.h"
#include "LOFUtils.h"


  namespace lof {

#if 0
    struct InputSlot {
      int ChildIdx;
      int InputIdx;
    public:
      InputSlot(int ChildIdx, int InputIdx) : ChildIdx(ChildIdx), InputIdx(InputIdx) {}
    };

    struct OutputSlot {
      int ChildIdx;
      int OutputIdx;
    public:
      OutputSlot(int ChildIdx, int OutputIdx) : ChildIdx(ChildIdx), OutputIdx(OutputIdx) {}
    };


    struct Slots {
      SmallVector<InputSlot*, 8> Inputs;
      SmallVector<OutputSlot*, 8> Outputs;
    };
#endif

    class GreenBuilder {
    private:
     // Green* Staged =  Green::createStaged();

      //DenseMap<GVal*, GUse*> LastUses;

      SmallVector <GExpr*, 8 > Conds;
      SmallVector <Green*,8> Children;

#if 0
      bool IsFloating = true;

      SmallVector<OutputSlot*, 8> InputSlots;
      SmallVector<Green*, 8> Children;
      SmallVector<Slots, 8> ChildSlots;

      SmallVector<InputSlot*, 8> OutputSlots;


      DenseMap<InputSlot*, OutputSlot*> Connections;
#endif
  
    private :
#if 0
       void registerUse(GUse *U ) {
         auto Def = U->getDef();
         auto& LastUse = LastUses[Def];
         if (LastUse) {
           LastUse->NextUse = U;
         } else {
           Def->FirstUse = U;
         }
         LastUse = U;
       }
#endif

    public:
#if 0
      static Green* buildOpExpr(ArrayRef <Green*> Subexprs, Operation::Kind K) {
        GreenBuilder Builder;

        SmallVector<GVal*, 8 > OpArgs;
        for (auto Subexpr : Subexprs) {
          SmallVector<GVal*,8> Operands;
          for (int i = 0; i < Subexpr->getNumInputs(); i += 1)
            Operands.push_back(  Builder.addArgument( Subexpr->getArgument(i)->getLLVMVal() ) );
          auto Slots = Builder.addStmt(Subexpr,Operands );
          OpArgs.append(Slots.begin(), Slots.end() );
        }

        auto OpSlots = Builder.addOperation(Operation(K, nullptr),OpArgs );

        return Builder.createStmt();
      }

      static Green* buildUnaryOpExpr(Green* Subexpr, Operation::Kind K) {
        return buildOpExpr({Subexpr}, K);
      }

      static Green* buildBinaryOpExpr(Green* LHS,Green* RHS, Operation::Kind K) {
        return buildOpExpr({LHS, RHS}, K);
      }


      static Green* buildNotExpr(Green* Subexpr) {
        return buildUnaryOpExpr(Subexpr, Operation::Negation );
      }


      // TOOD: List of operands
      static Green* buildConjunctionExpr(Green* LHS, Green* RHS) {
        return buildBinaryOpExpr( LHS,RHS, Operation::Conjuction );
      }

      static Green* buildDisjunctionExpr(Green* LHS, Green* RHS) {
        return buildBinaryOpExpr( LHS,RHS, Operation::Disjunction );
      }
#endif

#if 0
      GVal* addArgument(Value *LLVMVal) {
        auto Arg = GVal::createArgument(Staged, Staged->Arguments.size(), LLVMVal);
        Staged->Arguments.push_back( Arg );
        return Arg;
      }

      GUse* addResult(GVal *Val, Instruction *LLVMVal) {
        auto Ret = GUse::createResult( Val, Staged->Results.size(),LLVMVal  );
        Staged->Results.push_back(Ret);
        return Ret;
      }
#endif


     void addStmt(GExpr * Cond , Green* SubStmt) {
       Conds.push_back(Cond);
       Children.push_back(SubStmt);
      }
#if 0
      std::vector<GVal*> addStmt(Green* SubStmt, ArrayRef<GVal*> Operands) {
        if (SubStmt->isExpr()) {
          // TODO: These do not need to be added as child, but how do we reference its GVal then?
          int  a = 0;
        }

        auto NumInputs = SubStmt->getNumInputs();
        auto NumOutputs = SubStmt->getNumOutputs();
        auto ChildIdx = Staged->Children.size();

        if (!SubStmt->isExpr())
          Staged->IsFloating = false;

        SmallVector<GVal*, 8> Outputs;
        for (int i = 0; i < NumOutputs; i += 1) {
          auto OutVal = GVal::createOutput(Staged,  ChildIdx, i );
          Outputs.push_back(OutVal);
        }

        SmallVector<GUse*, 8> Inputs;
        for (int i = 0; i < NumInputs; i += 1) {
          auto InpUse = GUse::createInput(Operands[i], ChildIdx, i);
          Inputs.push_back(InpUse);
          registerUse(InpUse);
        }

        Staged->Children.push_back( Green::ChildMeaning( SubStmt, ChildIdx,  Inputs , Outputs ) );
        return { Outputs.begin(), Outputs.end() };
      }
#endif


     Green* addInstruction( GExpr*Cond,  Operation Op,  ArrayRef<GExpr*> Arguments,  ArrayRef<GSymbol*> Assignments,llvm:: Instruction *OrigInst) {
       auto Result = Green::createInstruction(Op,Arguments, Assignments, OrigInst );
        addStmt(Cond,Result);
        return Result;
      }


#if 0
      void connect(OutputSlot* Def, InputSlot* I) {
        assert(!Connections.count(I) && "Must not have been already connected");
        Connections[I] = Def;
      }
#endif


      Green* createStmt(llvm::Instruction *OrigBegin,llvm:: Instruction *OrigEnd) {
        return finish( GOpExpr::createTrueExpr() , false, OrigBegin,OrigEnd);
      }


      // TODO: Parameters defining number of iterations
      Green* createLoop(GExpr *ExecCond,llvm:: Instruction *OrigBegin,llvm:: Instruction *OrigEnd) {
        return finish(ExecCond,true,OrigBegin,OrigEnd);
      }


      private:
        Optional<SmallVector<GSymbol*,4>> ScalarReads =None;
        public:
      void setScalarReads(ArrayRef<GSymbol*> Reads) {
        ScalarReads .emplace( Reads.begin(), Reads.end() );
      }

           private:
             Optional<SmallVector<GSymbol*,4>> ScalarKills =None;
        public:
          void setScalarKills(ArrayRef<GSymbol*> Kills) {
            ScalarKills .emplace( Kills.begin(), Kills.end());
          }


        private:
          Optional<SmallVector<GSymbol*,4>> ScalarWrites =None;
        public:
          void setScalarWrites(ArrayRef<GSymbol*> Writes) {
            ScalarWrites .emplace( Writes.begin(), Writes.end());
          }


      private:
        Green* finish(GExpr *ExecCond, bool IsLooping, llvm::Instruction *OrigBegin, llvm::Instruction *OrigEnd) {

         return new Green(ExecCond, Children, Conds, IsLooping, OrigBegin, OrigEnd, 
           make_optional_ArrayRef<GSymbol*>(ScalarReads),
           make_optional_ArrayRef<GSymbol*>(ScalarKills),
           make_optional_ArrayRef<GSymbol*>(ScalarKills));
        }
    };


  } // namespace lof

#endif /* LLVM_LOF_GREEN_BUILDER_H */
