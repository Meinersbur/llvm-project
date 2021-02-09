#ifndef LLVM_LOF_GREEN_BUILDER_H
#define LLVM_LOF_GREEN_BUILDER_H

#include "LoopContext.h"
#include "llvm/LOF/Green.h"
#include "llvm/LOF/LOFUtils.h"

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

  // DenseMap<GVal*, GUse*> LastUses;

  LoopContext &Ctx;
  SmallVector<GExpr *, 8> Conds;
  SmallVector<Green *, 8> Children;

public:
  GreenBuilder() = delete;
  explicit GreenBuilder(LoopContext &Ctx) : Ctx(Ctx) {}

#if 0
      bool IsFloating = true;

      SmallVector<OutputSlot*, 8> InputSlots;
      SmallVector<Green*, 8> Children;
      SmallVector<Slots, 8> ChildSlots;

      SmallVector<InputSlot*, 8> OutputSlots;


      DenseMap<InputSlot*, OutputSlot*> Connections;
#endif

private:
#if 0
      void registerUse(GUse* U) {
        auto Def = U->getDef();
        auto& LastUse = LastUses[Def];
        if (LastUse) {
          LastUse->NextUse = U;
        }
        else {
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
          SmallVector<GVal*, 8> Operands;
          for (int i = 0; i < Subexpr->getNumInputs(); i += 1)
            Operands.push_back(Builder.addArgument(Subexpr->getArgument(i)->getLLVMVal()));
          auto Slots = Builder.addStmt(Subexpr, Operands);
          OpArgs.append(Slots.begin(), Slots.end());
        }

        auto OpSlots = Builder.addOperation(Operation(K, nullptr), OpArgs);

        return Builder.createStmt();
      }

      static Green* buildUnaryOpExpr(Green* Subexpr, Operation::Kind K) {
        return buildOpExpr({ Subexpr }, K);
      }

      static Green* buildBinaryOpExpr(Green* LHS, Green* RHS, Operation::Kind K) {
        return buildOpExpr({ LHS, RHS }, K);
      }


      static Green* buildNotExpr(Green* Subexpr) {
        return buildUnaryOpExpr(Subexpr, Operation::Negation);
      }


      // TOOD: List of operands
      static Green* buildConjunctionExpr(Green* LHS, Green* RHS) {
        return buildBinaryOpExpr(LHS, RHS, Operation::Conjuction);
      }

      static Green* buildDisjunctionExpr(Green* LHS, Green* RHS) {
        return buildBinaryOpExpr(LHS, RHS, Operation::Disjunction);
      }
#endif

#if 0
      GVal* addArgument(Value* LLVMVal) {
        auto Arg = GVal::createArgument(Staged, Staged->Arguments.size(), LLVMVal);
        Staged->Arguments.push_back(Arg);
        return Arg;
      }

      GUse* addResult(GVal* Val, Instruction* LLVMVal) {
        auto Ret = GUse::createResult(Val, Staged->Results.size(), LLVMVal);
        Staged->Results.push_back(Ret);
        return Ret;
      }
#endif

#if 0
      template<typename T> 
      static T** unconstify(T* const* Arg) {
        return reinterpret_cast<T**>(Arg);
      }
#endif

  void prepend(Green *SubStmt) {
    Conds.insert(Conds.begin(), Ctx.getTrue());
    Children.insert(Children.begin(), SubStmt);
  }

  void addStmt(GExpr *Cond, Green *SubStmt) {
    if (!Cond)
      Cond = Ctx.getTrue();
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
          auto OutVal = GVal::createOutput(Staged, ChildIdx, i);
          Outputs.push_back(OutVal);
        }

        SmallVector<GUse*, 8> Inputs;
        for (int i = 0; i < NumInputs; i += 1) {
          auto InpUse = GUse::createInput(Operands[i], ChildIdx, i);
          Inputs.push_back(InpUse);
          registerUse(InpUse);
        }

        Staged->Children.push_back(Green::ChildMeaning(SubStmt, ChildIdx, Inputs, Outputs));
        return { Outputs.begin(), Outputs.end() };
      }
#endif

  GExpr *getTrue() { return Ctx.getTrue(); }
  GExpr *getFalse() { return Ctx.getFalse(); }
  GExpr *getBool(bool Val) { return Val ? getTrue() : getFalse(); }
  GExpr *getConst(int Val) { return Ctx.getConst(Val); }

  GSymbol *createSymbolFromScratch(StringRef Name, llvm::Type *Ty) {
    // Do we want to register all symbols with LLVMContext? Maybe to
    // disambiguate names
    return GSymbol::createFromScratch(Name, Ty);
  }

  Green *addInstruction(StringRef Name, GExpr *Cond, Operation Op,
                        ArrayRef<GExpr *> Arguments,
                        ArrayRef<GSymbol *> Assignments,
                        llvm::Instruction *OrigInst) {
    auto Result = Green::createInstruction(Name, Op, Arguments, Assignments,
                                           OrigInst, nullptr);
    addStmt(Cond, Result);
    return Result;
  }

  Green *addAssignment(StringRef Name, GExpr *Cond, GSymbol *Target,
                       GExpr *Val) {
    return addInstruction(Name, Cond, Operation(Operation::Nop, nullptr), {Val},
                          {Target}, nullptr);
  }

#if 0
      void connect(OutputSlot* Def, InputSlot* I) {
        assert(!Connections.count(I) && "Must not have been already connected");
        Connections[I] = Def;
      }
#endif

  Green *createStmt(StringRef Name, llvm::Instruction *OrigBegin,
                    llvm::Instruction *OrigEnd) {
    return finish(Name, GOpExpr::createTrueExpr(), false, OrigBegin, OrigEnd,
                  nullptr, nullptr);
  }

  // TODO: Parameters defining number of iterations
  Green *createLoop(StringRef Name, GExpr *ExecCond,
                    llvm::Instruction *OrigBegin, llvm::Instruction *OrigEnd,
                    GSymbol *LoopIsFirst, GSymbol *LoopCounter) {
    if (!LoopIsFirst)
      LoopIsFirst = GSymbol::createFromScratch("isfirst", Ctx.getBoolType());
    // if (!LoopCounter)
    //  LoopCounter = GSymbol::createFromScratch("iv", Ctx.getGenericIntType());
    return finish(Name, ExecCond, true, OrigBegin, OrigEnd, LoopIsFirst,
                  LoopCounter);
  }

private:
  bool UserDefined = false;
  SmallVector<GSymbol *, 4> ScalarReads;
  SmallVector<GSymbol *, 4> ScalarKills;
  SmallVector<GSymbol *, 4> ScalarWrites;
  SmallVector<GSymbol *, 4> ScalarRecurrences;

public:
  //  void setScalarReads(ArrayRef<GSymbol*> Reads) {
  //    ScalarReads .emplace( Reads.begin(), Reads.end() );
  //  }

private:
public:
  // void setScalarKills(ArrayRef<GSymbol*> Kills) {
  //   ScalarKills .emplace( Kills.begin(), Kills.end());
  //}

private:
public:
  // void setScalarWrites(ArrayRef<GSymbol*> Writes) {
  //    ScalarWrites .emplace( Writes.begin(), Writes.end());
  // }

  void setScalarReadKillsWrites(ArrayRef<GSymbol *> Reads,
                                ArrayRef<GSymbol *> Kills,
                                ArrayRef<GSymbol *> Writes,
                                ArrayRef<GSymbol *> Recurrences) {
    UserDefined = true;
    ScalarReads.clear();
    ScalarReads.append(Reads.begin(), Reads.end());
    ScalarKills.clear();
    ScalarKills.append(Kills.begin(), Kills.end());
    ScalarWrites.clear();
    ScalarWrites.append(Writes.begin(), Writes.end());
    ScalarRecurrences.clear();
    ScalarRecurrences.append(Recurrences.begin(), Recurrences.end());
  }

private:
  Green *TransformationOf = nullptr;

public:
  // TODO: Also store information about which (relative) instance this
  // represents use (partially expanded) RedTree for this?
  void setTransformationOf(Green *Base) { TransformationOf = Base; }

private:
  Green *finish(StringRef Name, GExpr *ExecCond, bool IsLooping,
                llvm::Instruction *OrigBegin, llvm::Instruction *OrigEnd,
                GSymbol *LoopIsFirst, GSymbol *CanonicalCounter) {
    if (!UserDefined) {
      ScalarReads.clear();
      ScalarKills.clear();
      ScalarWrites.clear();
      ScalarRecurrences.clear();

      for (auto SubStmt : Children) {
        auto SubStmtScalarReads = SubStmt->getScalarReads();
        ScalarReads.append(SubStmtScalarReads.begin(),
                           SubStmtScalarReads.end());

        auto SubStmtScalarKills = SubStmt->getScalarKills();
        ScalarKills.append(SubStmtScalarKills.begin(),
                           SubStmtScalarKills.end());

        auto SubStmtScalarWrites = SubStmt->getScalarWrites();
        ScalarWrites.append(SubStmtScalarWrites.begin(),
                            SubStmtScalarWrites.end());

        // TODO: Determine recurrences
      }
    }

    return new Green(Name, ExecCond, Children, Conds, IsLooping, OrigBegin,
                     OrigEnd, ScalarReads, ScalarKills, ScalarWrites,
                     ScalarRecurrences, LoopIsFirst, CanonicalCounter,
                     TransformationOf);
  }
};

} // namespace lof

#endif /* LLVM_LOF_GREEN_BUILDER_H */
