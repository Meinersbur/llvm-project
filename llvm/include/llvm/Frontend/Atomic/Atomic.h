//===--- Atomic.h - Shared codegen of atomic operations -------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_FRONTEND_ATOMIC_ATOMIC_H
#define LLVM_FRONTEND_ATOMIC_ATOMIC_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/CodeGen/RuntimeLibcalls.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Transforms/Utils/BuildLibCalls.h"


namespace llvm {

      struct  AtomicResult {
    Value *V;
    bool VIsPtrToResult;
    Value *Success;

    static AtomicResult fromVal(Value *Val, Value *Success) {
      AtomicResult Res;
      Res.V = Val;
      Res.VIsPtrToResult = false;
      Res.Success = Success;
      return Res;
    }

    static AtomicResult fromPtr(Value *Ptr, Value *Success) {
      AtomicResult Res;
      Res.V = Ptr;
      Res.VIsPtrToResult = true  ;
      Res.Success = Success;
      return Res;
    }
  };

template <typename IRBuilderTy> struct AtomicInfo {
  IRBuilderTy *Builder;


  // For libcall ABI; try to be as close to what Clang would emit as possible.
  // TODO: Handle by BuildLibCalls.h
  Type *IntTy;  // LLVM representation of `int`
  Type *SizeTy; // LLVM representation of `size_t`
  uint64_t BitsPerByte; // Clang does not always hardcode this
  CallingConv::ID cc; 
  bool EnableNoundefAttrs;
  bool BoolHasStrictReturn; 
  bool IntIsPromotable;
//  bool AssumeConvergent;

  Type *Ty;
  uint64_t AtomicSizeInBits;
  uint64_t ValueSizeInBits;
  Align AtomicAlign;
  Align ValueAlign;
  // bool IsVolatile;
  // bool IsWeak;
  bool UseLibcall;

public:
  AtomicInfo(IRBuilderTy *Builder, 
      Type *IntTy, Type *SizeTy, uint64_t BitsPerByte, CallingConv::ID cc, bool EnableNoundefAttrs, bool BoolHasStrictReturn, bool IntIsPromotable, bool AssumeConvergent,
    Type *Ty, uint64_t AtomicSizeInBits,
             uint64_t ValueSizeInBits, Align AtomicAlign,
             Align ValueAlign, 
             // bool ISVolatile, bool IsWeak,
             bool UseLibcall)
      : Builder(Builder),
    IntTy(IntTy), SizeTy(SizeTy), BitsPerByte(BitsPerByte), cc(cc), EnableNoundefAttrs(EnableNoundefAttrs), BoolHasStrictReturn(BoolHasStrictReturn), IntIsPromotable(IntIsPromotable),
    //AssumeConvergent(AssumeConvergent),
    Ty(Ty), AtomicSizeInBits(AtomicSizeInBits),
        ValueSizeInBits(ValueSizeInBits), AtomicAlign(AtomicAlign),
        ValueAlign(ValueAlign), UseLibcall(UseLibcall) {}

  virtual ~AtomicInfo() = default;







  Align getAtomicAlignment() const { return AtomicAlign; }
  uint64_t getAtomicSizeInBits() const { return AtomicSizeInBits; }
  uint64_t getValueSizeInBits() const { return ValueSizeInBits; }
  bool shouldUseLibcall() const { return UseLibcall; }
  Type *getAtomicTy() const { return Ty; }

  virtual Value *getAtomicPointer() const = 0;
  virtual void decorateWithTBAA(Instruction *I) {}
  virtual void decorateFnDeclAttributes(AttrBuilder &FnAttr, StringRef Name) {}
  virtual void decorateCallAttributes(AttrBuilder &CallAttr, StringRef Name) {}
  virtual AllocaInst *CreateAlloca(Type *Ty,  const Twine &Name) { 
    auto Alloca = Builder->CreateAlloca(Ty); 
  Alloca->setName(Name);
  return Alloca;
  } 



  /// Is the atomic size larger than the underlying value type?
  ///
  /// Note that the absence of padding does not mean that atomic
  /// objects are completely interchangeable with non-atomic
  /// objects: we might have promoted the alignment of a type
  /// without making it bigger.
  bool hasPadding() const { return (ValueSizeInBits != AtomicSizeInBits); }

  LLVMContext &getLLVMContext() const { return Builder->getContext(); }

  static bool shouldCastToInt(Type *ValTy, bool CmpXchg) {
    if (ValTy->isFloatingPointTy())
      return ValTy->isX86_FP80Ty() || CmpXchg;
    return !ValTy->isIntegerTy() && !ValTy->isPointerTy();
  }

  Value *EmitAtomicLoadOp(AtomicOrdering AO, bool IsVolatile,
                                bool CmpXchg = false) {
    Value *Ptr = getAtomicPointer();
    Type *AtomicTy = Ty;
    if (shouldCastToInt(Ty, CmpXchg))
      AtomicTy = IntegerType::get(getLLVMContext(), AtomicSizeInBits);
    LoadInst *Load =
        Builder->CreateAlignedLoad(AtomicTy, Ptr, AtomicAlign, "atomic-load");
    Load->setAtomic(AO);

    // Other decoration.
    if (IsVolatile)
      Load->setVolatile(true);
    decorateWithTBAA(Load);
    return Load;
  }






  // TODO: Replace with emitLibCall and its helpers from from BuildLibCalls.h
   CallInst *emitLibCall(IRBuilderTy *Builder, StringRef fnName,
                                       Type *ResultType,
                                       ArrayRef<Value *> Args) {
      auto &C = Builder->getContext();




    SmallVector<Type *, 6> ParamTys;
    SmallVector<AttributeSet, 6> ArgAttrs;
    for (Value *Arg : Args) {
      ParamTys.push_back(Arg->getType());
  
      AttrBuilder AttrB(C);
      if (EnableNoundefAttrs) 
        AttrB.addAttribute( Attribute::NoUndef);
      if ((ParamTys.back() == IntTy && IntIsPromotable)
        //|| (ParamTys.back()->isIntegerTy() && ParamTys.back()->getIntegerBitWidth() <IntTy->getIntegerBitWidth() )
         ) {
            AttrB.addAttribute( Attribute::SExt);
      }
      ArgAttrs.push_back(AttributeSet::get(C, AttrB));
    }
    FunctionType *FnType = FunctionType::get(ResultType, ParamTys, false);
    Module *M = Builder->GetInsertBlock()->getModule();



    AttrBuilder fnAttrB(C);
     decorateFnDeclAttributes( fnAttrB, fnName  );
    fnAttrB.addAttribute(Attribute::NoUnwind);
    fnAttrB.addAttribute(Attribute::WillReturn);
    AttributeList fnAttrs = AttributeList::get(C, AttributeList::FunctionIndex, fnAttrB);
    FunctionCallee LibcallFn = M->getOrInsertFunction(fnName, FnType, fnAttrs);
    Function *F = cast<Function>(LibcallFn.getCallee());
    F->setCallingConv(cc);
    markRegisterParameterAttributes(F);
    

     AttrBuilder CallAttrB(C);
   decorateCallAttributes( CallAttrB, fnName  );

        AttrBuilder RetAttrB(C);
    if (F->getReturnType() ->isIntegerTy(1)) {
      // Clang switches on type-coersion to int for things that can be passed in a register, and adds zeroextend.
      RetAttrB.addAttribute(Attribute::ZExt);

      // It also sets noundef for the return value under certain conditions
      if (EnableNoundefAttrs && BoolHasStrictReturn) 
               RetAttrB.addAttribute( Attribute::NoUndef);
    }

    CallInst *CI = Builder->CreateCall(LibcallFn, Args);
    CI->setCallingConv(cc);
    CI->setAttributes(AttributeList::get(C, AttributeSet::get(C, CallAttrB), AttributeSet::get(C, RetAttrB) , ArgAttrs));



    return CI;
  }



  Value *getAtomicSizeValue() const {
    return ConstantInt::get(SizeTy,   AtomicSizeInBits / BitsPerByte);
  }



 Value *
    EmitAtomicCompareExchangeLibcall(
      Value *ExpectedPtr, Value *DesiredPtr,
      AtomicOrdering Success, AtomicOrdering Failure) {
    auto &C = getLLVMContext();



    // In Clang, the type of the builtin is derived from the concrete arguments. It shouldn't.
    Value *Args[6] = {
        getAtomicSizeValue(),
        getAtomicPointer(),
        ExpectedPtr,
        DesiredPtr,
        Constant::getIntegerValue(
           IntTy,
            APInt(IntTy->getScalarSizeInBits(),  (int)toCABI(Success), /*signed=*/true)),
       Constant::getIntegerValue(
           IntTy,
            APInt(IntTy->getScalarSizeInBits(),  (int)toCABI(Failure), /*signed=*/true)),
    };
    auto Res = emitLibCall(Builder, "__atomic_compare_exchange", IntegerType::getInt1Ty(C), Args);
   // auto PreviousVal = Builder->CreateLoad(ExpectedVal->getType(), ExpectedPtr, "atomic.previous");
   // return std::make_pair(Res, Res);
    return Res;
  }

  Value *castToAtomicIntPointer(Value *addr) const {
    return addr; // opaque pointer
  }

  Value *getAtomicAddressAsAtomicIntPointer() const {
    return castToAtomicIntPointer(getAtomicPointer());
  }



  std::pair<Value *, Value *>
  EmitAtomicCompareExchangeOp(Value *ExpectedVal, Value *DesiredVal,
                              AtomicOrdering Success,
                              AtomicOrdering Failure,
                              bool IsVolatile = false, bool IsWeak = false) {
    // Do the atomic store.
    Value *Addr = getAtomicAddressAsAtomicIntPointer();
    auto *Inst = Builder->CreateAtomicCmpXchg(Addr, ExpectedVal, DesiredVal,
                                              getAtomicAlignment(), Success,
                                              Failure, SyncScope::System);
    // Other decoration.
    Inst->setVolatile(IsVolatile);
    Inst->setWeak(IsWeak);

    // Okay, turn that back into the original value type.
    auto *PreviousVal = Builder->CreateExtractValue(Inst, /*Idxs=*/0);
    auto *SuccessFailureVal = Builder->CreateExtractValue(Inst, /*Idxs=*/1);
    return std::make_pair(PreviousVal, SuccessFailureVal);
  }


  // The callbacks are done to make this an NFC change; It should be sufficient to pass the pointer variant, and leave it to mem2reg to optimize them away.
AtomicResult
  EmitAtomicCompareExchange(
    function_ref< Value *()> MaterializeExpectedVal,
      function_ref< Value *()> MaterializeExpectedPtr,
        function_ref< Value *()> MaterializeDesiredVal,
      function_ref< Value *()> MaterializeDesiredPtr,
                            AtomicOrdering Success,
                            AtomicOrdering Failure, bool IsVolatile,
                            bool IsWeak, bool ForceLibcall=false) {

    // Check whether we should use a library call.
    if (shouldUseLibcall() || ForceLibcall) {
              auto   ExpectedPtr = MaterializeExpectedPtr();
        auto   DesiredPtr = MaterializeDesiredPtr();
      auto Res = EmitAtomicCompareExchangeLibcall(ExpectedPtr, DesiredPtr, Success, Failure);

          return AtomicResult::fromPtr(ExpectedPtr,Res );
    }



            auto   ExpectedVal = MaterializeExpectedVal();
        auto   DesiredVal = MaterializeDesiredVal();
    auto Res = EmitAtomicCompareExchangeOp(ExpectedVal, DesiredVal, Success,      Failure, IsVolatile, IsWeak);
      return AtomicResult::fromVal(Res.first,Res.second );
  }

};
} // end namespace llvm

#endif /* LLVM_FRONTEND_ATOMIC_ATOMIC_H */
