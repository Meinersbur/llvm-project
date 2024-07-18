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
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Transforms/Utils/BuildLibCalls.h"

namespace llvm {

struct AtomicResult {
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
    Res.VIsPtrToResult = true;
    Res.Success = Success;
    return Res;
  }
};

//template <typename IRBuilderTy> 
struct AtomicInfo {
  IRBuilderBase &Builder;

  // For libcall ABI; try to be as close to what Clang would emit as possible.
  // TODO: Handle by BuildLibCalls.h
  Type *IntTy;          // LLVM representation of `int`
  Type *SizeTy;         // LLVM representation of `size_t`
  uint64_t BitsPerByte; // Clang does not always hardcode this
  CallingConv::ID cc;
  bool EnableNoundefAttrs;
  bool BoolHasStrictReturn;
  bool IntIsPromotable;

  Type *Ty;
  uint64_t AtomicSizeInBits;
  uint64_t ValueSizeInBits;
  Align AtomicAlign;
  Align ValueAlign;
  bool UseLibcall;

public:
  AtomicInfo(IRBuilderBase &Builder, 
    Type *IntTy, Type *SizeTy,
             uint64_t BitsPerByte, CallingConv::ID cc, bool EnableNoundefAttrs,
             bool BoolHasStrictReturn, bool IntIsPromotable,
             bool AssumeConvergent, Type *Ty, uint64_t AtomicSizeInBits,
             uint64_t ValueSizeInBits, Align AtomicAlign, Align ValueAlign,
             bool UseLibcall);

  virtual ~AtomicInfo() = default;


      static bool shouldCastToInt(Type *ValTy, bool CmpXchg) ;

  Align getAtomicAlignment() const { return AtomicAlign; }
  uint64_t getAtomicSizeInBits() const { return AtomicSizeInBits; }
  uint64_t getValueSizeInBits() const { return ValueSizeInBits; }
  bool shouldUseLibcall() const { return UseLibcall; }
  Type *getAtomicTy() const { return Ty; }

  virtual Value *getAtomicPointer() const = 0;
  virtual void decorateWithTBAA(Instruction *I) {}
  virtual void decorateFnDeclAttributes(AttrBuilder &FnAttr, StringRef Name) {}
  virtual void decorateCallAttributes(AttrBuilder &CallAttr, StringRef Name) {}
  virtual AllocaInst *CreateAlloca(Type *Ty, const Twine &Name) ;

  /// Is the atomic size larger than the underlying value type?
  ///
  /// Note that the absence of padding does not mean that atomic
  /// objects are completely interchangeable with non-atomic
  /// objects: we might have promoted the alignment of a type
  /// without making it bigger.
  bool hasPadding() const { return (ValueSizeInBits != AtomicSizeInBits); }

  LLVMContext &getLLVMContext() const;


  Value *EmitAtomicLoadOp(AtomicOrdering AO, bool IsVolatile,
                          bool CmpXchg = false) ;


  CallInst *emitLibCall(StringRef fnName,     Type *ResultType, ArrayRef<Value *> Args);

  Value *getAtomicSizeValue() const {
    return ConstantInt::get(SizeTy, AtomicSizeInBits / BitsPerByte);
  }

  Value *EmitAtomicCompareExchangeLibcall(Value *ExpectedPtr, Value *DesiredPtr,
                                          AtomicOrdering Success,
                                          AtomicOrdering Failure) ;

  Value *castToAtomicIntPointer(Value *addr) const {
    return addr; // opaque pointer
  }

  Value *getAtomicAddressAsAtomicIntPointer() const {
    return castToAtomicIntPointer(getAtomicPointer());
  }

  std::pair<Value *, Value *>
  EmitAtomicCompareExchangeOp(Value *ExpectedVal, Value *DesiredVal,
                              AtomicOrdering Success, AtomicOrdering Failure,
                              bool IsVolatile = false, bool IsWeak = false) ;

  // The callbacks are done to make this an NFC change; It should be sufficient
  // to pass the pointer variant, and leave it to mem2reg to optimize them away.
  AtomicResult
  EmitAtomicCompareExchange(function_ref<Value *()> MaterializeExpectedVal,
                            function_ref<Value *()> MaterializeExpectedPtr,
                            function_ref<Value *()> MaterializeDesiredVal,
                            function_ref<Value *()> MaterializeDesiredPtr,
                            AtomicOrdering Success, AtomicOrdering Failure,
                            bool IsVolatile, bool IsWeak,
                            bool ForceLibcall = false);
};
} // end namespace llvm

#endif /* LLVM_FRONTEND_ATOMIC_ATOMIC_H */
