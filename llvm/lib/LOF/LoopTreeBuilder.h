#ifndef LLVM_LOF_LOOPTREEBUILDER_H
#define LLVM_LOF_LOOPTREEBUILDER_H

#include "Green.h"
#include "GreenBuilder.h"
#include "llvm/Analysis/LoopInfo.h"





namespace llvm {
  namespace lof {


    class GreenConverter {
      DenseMap <Value*, GSymbol*> InputsVals;
      Function* Func;
      LoopInfo* LI;



    public:
      GreenConverter(Function *Func,LoopInfo* LI) : Func(Func),  LI(LI) {}
      Green* build() {

        for (auto &Arg : Func->args()) {
          InputsVals[&Arg] = GSymbol::create(&Arg);
        }
        return buildOriginalLoop(nullptr, &Func->getEntryBlock(), GOpExpr::createTrueExpr());
      }

    private:
      GSymbol* getOrCreateSym(Value* LLVMVal) {
        assert(!isa<Constant>(LLVMVal));
        auto &Sym = InputsVals[LLVMVal];
        if (!Sym) {
          Sym = GSymbol::create( LLVMVal );
        }
        return Sym;
      }

      GExpr* getOrCreateRefExpr(Value* LLVMVal) {
        // FIXME: constants that may trap must be instructions
        // TODO: Uniquefy constant expressions
        if (auto C = dyn_cast<Constant>(LLVMVal))
          return GOpExpr::createConstExpr(C);
        return getOrCreateSym(LLVMVal);
      }

      Green* buildOriginalLoop(Loop* L, BasicBlock *Entry, GExpr *Cond) {
        std::deque < std::pair< BasicBlock*, GExpr*>> Worklist; 
        Worklist.push_back({ Entry, GOpExpr::createTrueExpr() });

        // SmallVector<GreenMeaning, 32> LvlInsts;
        //SmallVector<Dep*, 16> InputDeps;
        //int NumIntermediates = 0;
        //int NumOutputs = 0;

        //DenseMap<Instruction*,  GVal*> DefVals;
        //DenseMap<Value*, GVal*> InputsVals;
        GreenBuilder Builder;

        auto QueueSuccessor = [&,this](BasicBlock* BB, GExpr *BBCond) {
          // Backedge
          if (L && BB == L->getHeader())
            return;

          // Exiting edge
          if (L && !L->contains(BB))
            return;

          auto InLoop = LI->getLoopFor(BB);
          if (L == InLoop) {
            Worklist.push_back({ BB , GOpExpr::createTrueExpr()  });
            return;
          }

          assert(InLoop->getHeader()==BB);
          auto InnerLoop = buildOriginalLoop(InLoop, BB, BBCond);
          //GreenMeaning GMeaning(InnerLoop, {}, {});
          //LvlInsts.push_back(GMeaning);

          Builder.addStmt( BBCond,  InnerLoop);
        };


        while (!Worklist.empty()) {
          auto p = Worklist.front();
          auto BB = p.first;
          auto BBCond = p.second;
          Worklist.pop_front();

          for (auto& Inst: *BB) {
            if (Inst.isTerminator()) {
              // TODO: Detect rejoining branches
              if (auto BrInst = dyn_cast<BranchInst>(&Inst)) {
                if (BrInst->isUnconditional()) {
                  QueueSuccessor(BrInst->getSuccessor(0), BBCond);
                  break;
                }
                if (BrInst->isConditional()) {
                  auto BrCond = nullptr;//TODO
                  QueueSuccessor(BrInst->getSuccessor(0),  GOpExpr::createConjunctionExpr(BBCond, BrCond ) );
                  QueueSuccessor(BrInst->getSuccessor(1),  GOpExpr::createConjunctionExpr(BBCond, GOpExpr::createNotExpr( BrCond )) );
                  break;
                }
              }
              if (auto RetInst = dyn_cast<ReturnInst>(&Inst)) {
                assert(!L && "Dont support ret inside loops yet");
                break;
              }

              llvm_unreachable("Unsupported branch");
            }

            // auto GInst = Green::createOperation();
            //  auto GSlots = Builder.addOperation(Operation(Operation::LLVMInst, &Inst), {});

            SmallVector<GExpr*, 8> OperandVals;
            OperandVals.set_size(Inst.getNumOperands());
            for (auto &Operand : Inst.operands()) {
              assert(Operand.getUser() == &Inst);
              auto OpIdx = Operand.getOperandNo();
              auto Def = Operand.get();
              //  OutputSlot* OperandSlot;

              OperandVals[OpIdx] = getOrCreateRefExpr(Def);
            }


            auto ResultVal = GSymbol::create(&Inst);
            Builder.addInstruction(BBCond,  Operation(Operation::LLVMInst, &Inst), OperandVals , {ResultVal});
            InputsVals[&Inst] = ResultVal ;
          }
        }

        if (L) {
          //return Green::createLoop(InputDeps.size(), InputDeps, NumIntermediates, NumOutputs, LvlInsts);
          return Builder.createLoop();
        }
        //  return Green::createFunc(InputDeps.size(), InputDeps, NumIntermediates, NumOutputs,  LvlInsts);
        return Builder.createStmt();
      }



    };



  } // namespace lof
} // namespace llvm

#endif /* LLVM_LOF_LOOPTREEBUILDER_H */
