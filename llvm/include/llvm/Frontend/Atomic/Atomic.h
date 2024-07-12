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

  Type *Ty;
  uint64_t AtomicSizeInBits;
  uint64_t ValueSizeInBits;
  llvm::Align AtomicAlign;
  llvm::Align ValueAlign;
  // bool IsVolatile;
  // bool IsWeak;
  bool UseLibcall;

public:
  AtomicInfo(IRBuilderTy *Builder, 
      Type *IntTy, Type *SizeTy, uint64_t BitsPerByte, CallingConv::ID cc, bool EnableNoundefAttrs, bool BoolHasStrictReturn,
    Type *Ty, uint64_t AtomicSizeInBits,
             uint64_t ValueSizeInBits, llvm::Align AtomicAlign,
             llvm::Align ValueAlign,
             // bool ISVolatile, bool IsWeak,
             bool UseLibcall)
      : Builder(Builder),
    IntTy(IntTy), SizeTy(SizeTy), BitsPerByte(BitsPerByte), cc(cc), EnableNoundefAttrs(EnableNoundefAttrs), BoolHasStrictReturn(BoolHasStrictReturn), IntIsPromotable(IntIsPromotable),
    Ty(Ty), AtomicSizeInBits(AtomicSizeInBits),
        ValueSizeInBits(ValueSizeInBits), AtomicAlign(AtomicAlign),
        ValueAlign(ValueAlign), UseLibcall(UseLibcall) {}

  virtual ~AtomicInfo() = default;







  llvm::Align getAtomicAlignment() const { return AtomicAlign; }
  uint64_t getAtomicSizeInBits() const { return AtomicSizeInBits; }
  uint64_t getValueSizeInBits() const { return ValueSizeInBits; }
  bool shouldUseLibcall() const { return UseLibcall; }
  llvm::Type *getAtomicTy() const { return Ty; }

  virtual llvm::Value *getAtomicPointer() const = 0;
  virtual void decorateWithTBAA(Instruction *I) {}
  virtual llvm::AllocaInst *CreateAlloca(llvm::Type *Ty,
                                         const llvm::Twine &Name) = 0;

  /// Is the atomic size larger than the underlying value type?
  ///
  /// Note that the absence of padding does not mean that atomic
  /// objects are completely interchangeable with non-atomic
  /// objects: we might have promoted the alignment of a type
  /// without making it bigger.
  bool hasPadding() const { return (ValueSizeInBits != AtomicSizeInBits); }

  LLVMContext &getLLVMContext() const { return Builder->getContext(); }

  static bool shouldCastToInt(llvm::Type *ValTy, bool CmpXchg) {
    if (ValTy->isFloatingPointTy())
      return ValTy->isX86_FP80Ty() || CmpXchg;
    return !ValTy->isIntegerTy() && !ValTy->isPointerTy();
  }

  llvm::Value *EmitAtomicLoadOp(llvm::AtomicOrdering AO, bool IsVolatile,
                                bool CmpXchg = false) {
    Value *Ptr = getAtomicPointer();
    Type *AtomicTy = Ty;
    if (shouldCastToInt(Ty, CmpXchg))
      AtomicTy = llvm::IntegerType::get(getLLVMContext(), AtomicSizeInBits);
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
#if 0

  llvm::Constant *C =
      GetOrCreateLLVMFunction(Name, FTy, GlobalDecl(), /*ForVTable=*/false,
                              /*DontDefer=*/false, /*IsThunk=*/false,
                              ExtraAttrs);

  if (auto *F = dyn_cast<llvm::Function>(C)) {
    if (F->empty()) {
      // In Windows Itanium environments, try to mark runtime functions
      // dllimport. For Mingw and MSVC, don't. We don't really know if the user
      // will link their standard library statically or dynamically. Marking
      // functions imported when they are not imported can cause linker errors
      // and warnings.
      if (!Local && getTriple().isWindowsItaniumEnvironment() &&
          !getCodeGenOpts().LTOVisibilityPublicStd) {
        const FunctionDecl *FD = GetRuntimeFunctionDecl(Context, Name);
        if (!FD || FD->hasAttr<DLLImportAttr>()) {
          F->setDLLStorageClass(llvm::GlobalValue::DLLImportStorageClass);
          F->setLinkage(llvm::GlobalValue::ExternalLinkage);
        }
      }
      setDSOLocal(F);
      // FIXME: We should use CodeGenModule::SetLLVMFunctionAttributes() instead
      // of trying to approximate the attributes using the LLVM function
      // signature. This requires revising the API of CreateRuntimeFunction().
      markRegisterParameterAttributes(F);
    }
  }
    #endif 

      auto &C = Builder->getContext();




    SmallVector<Type *, 6> ParamTys;
    SmallVector<AttributeSet, 6> ArgAttrs;
    for (Value *Arg : Args) {
      ParamTys.push_back(Arg->getType());
  
      AttrBuilder AttrB(C);
      if (EnableNoundefAttrs) 
        AttrB.addAttribute( Attribute::NoUndef);
      if ((ParamTys.back() == IntTy && IntIsPromotable) ||
          (ParamTys.back()->isIntegerTy() && ParamTys.back()->getIntegerBitWidth() <IntTy->getIntegerBitWidth() )) {
            AttrB.addAttribute( llvm::Attribute::SExt);
      }
      ArgAttrs.push_back(AttributeSet::get(C, AttrB));
    }
    FunctionType *FnType = FunctionType::get(ResultType, ParamTys, false);
    Module *M = Builder->GetInsertBlock()->getModule();



    llvm::AttrBuilder fnAttrB(C);
    fnAttrB.addAttribute(llvm::Attribute::NoUnwind);
    fnAttrB.addAttribute(llvm::Attribute::WillReturn);
    llvm::AttributeList fnAttrs = AttributeList::get(C, llvm::AttributeList::FunctionIndex, fnAttrB);
    FunctionCallee LibcallFn = M->getOrInsertFunction(fnName, FnType, fnAttrs);
    Function *F = cast<Function>(LibcallFn.getCallee());
    F->setCallingConv(cc);
    markRegisterParameterAttributes(F);
    

        llvm::AttrBuilder RetAttrB(C);
    if (F->getReturnType() ->isIntegerTy(1)) {
      // Clang switches on type-coersion to int for things that can be passed in a register, and adds zeroextend.
      RetAttrB.addAttribute(Attribute::ZExt );

      // It also sets noundef for the return value under certain conditions
      if (EnableNoundefAttrs && BoolHasStrictReturn) {
               RetAttrB.addAttribute( llvm::Attribute::NoUndef);
      }
    }

    CallInst *CI = Builder->CreateCall(LibcallFn, Args);
    CI->setCallingConv(cc);
    CI->setAttributes(AttributeList::get(C, {}, AttributeSet::get(C, RetAttrB) , ArgAttrs));



    return CI;
  }



  llvm::Value *getAtomicSizeValue() const {
    return llvm::ConstantInt::get(SizeTy,   AtomicSizeInBits / BitsPerByte);
  }



 llvm::Value *
    EmitAtomicCompareExchangeLibcall(
      llvm::Value *ExpectedPtr, llvm::Value *DesiredPtr,
      llvm::AtomicOrdering Success, llvm::AtomicOrdering Failure) {
    auto &C = getLLVMContext();



    // In Clang, the type of the builtin is derived from the concrete arguments. It shouldn't.
    Value *Args[6] = {
        getAtomicSizeValue(),
        getAtomicPointer(),
        ExpectedPtr,
        DesiredPtr,
        Constant::getIntegerValue(
           IntTy,
            llvm::APInt(IntTy->getScalarSizeInBits(),  (int)llvm::toCABI(Success), /*signed=*/true)),
       Constant::getIntegerValue(
           IntTy,
            llvm::APInt(IntTy->getScalarSizeInBits(),  (int)llvm::toCABI(Failure), /*signed=*/true)),
    };
    auto Res = emitLibCall(Builder, "__atomic_compare_exchange", llvm::IntegerType::getInt1Ty(C), Args);
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



  std::pair<llvm::Value *, llvm::Value *>
  EmitAtomicCompareExchangeOp(llvm::Value *ExpectedVal, llvm::Value *DesiredVal,
                              llvm::AtomicOrdering Success,
                              llvm::AtomicOrdering Failure,
                              bool IsVolatile = false, bool IsWeak = false) {
    // Do the atomic store.
    Value *Addr = getAtomicAddressAsAtomicIntPointer();
    auto *Inst = Builder->CreateAtomicCmpXchg(Addr, ExpectedVal, DesiredVal,
                                              getAtomicAlignment(), Success,
                                              Failure, llvm::SyncScope::System);
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
    //llvm::Value *ExpectedVal, llvm::Value *DesiredVal,
                            llvm::AtomicOrdering Success,
                            llvm::AtomicOrdering Failure, bool IsVolatile,
                            bool IsWeak, bool ForceLibcall=false) {

    // Check whether we should use a library call.
    if (shouldUseLibcall() || ForceLibcall) {
              auto   ExpectedPtr = MaterializeExpectedPtr();
        auto   DesiredPtr = MaterializeDesiredPtr();
      auto Res = EmitAtomicCompareExchangeLibcall(ExpectedPtr, DesiredPtr, Success, Failure);

          return AtomicResult::fromPtr(ExpectedPtr,Res );
    }

    // If we've got a scalar value of the right size, try to avoid going
    // through memory.
    // auto *ExpectedVal = convertRValueToInt(ExpectedPtr, /*CmpXchg=*/true);
    // auto *DesiredVal = convertRValueToInt(DesiredPtr, /*CmpXchg=*/true);

            auto   ExpectedVal = MaterializeExpectedVal();
        auto   DesiredVal = MaterializeDesiredVal();
    auto Res = EmitAtomicCompareExchangeOp(ExpectedVal, DesiredVal, Success,      Failure, IsVolatile, IsWeak);
      return AtomicResult::fromVal(Res.first,Res.second );
    // return std::make_pair(ConvertToValueOrAtomic(Res.first,
    // AggValueSlot::ignored(), SourceLocation(), /*AsValue=*/false,
    // /*CmpXchg=*/true), Res.second);
  }

};
} // end namespace llvm

#endif /* LLVM_FRONTEND_ATOMIC_ATOMIC_H */
