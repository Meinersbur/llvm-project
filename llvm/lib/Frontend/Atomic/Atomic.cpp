//===--- Atomic.cpp - Shared codegen of atomic operations -----------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Frontend/Atomic/Atomic.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Frontend/Atomic/Atomic.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Operator.h"
#include "llvm/Transforms/Utils/BuildLibCalls.h"

using namespace llvm;

namespace {}

namespace llvm {
AtomicInfo::AtomicInfo(IRBuilderBase &Builder, Type *IntTy, Type *SizeTy,
                       uint64_t BitsPerByte, CallingConv::ID cc,
                       bool EnableNoundefAttrs, bool BoolHasStrictReturn,
                       bool IntIsPromotable, bool AssumeConvergent, Type *Ty,
                       uint64_t AtomicSizeInBits, uint64_t ValueSizeInBits,
                       Align AtomicAlign, Align ValueAlign,
                       bool UseLibcall)
    : Builder(Builder), IntTy(IntTy), SizeTy(SizeTy), BitsPerByte(BitsPerByte),
      cc(cc), EnableNoundefAttrs(EnableNoundefAttrs),
      BoolHasStrictReturn(BoolHasStrictReturn),
      IntIsPromotable(IntIsPromotable), Ty(Ty),
      AtomicSizeInBits(AtomicSizeInBits), ValueSizeInBits(ValueSizeInBits),
      AtomicAlign(AtomicAlign), ValueAlign(ValueAlign), UseLibcall(UseLibcall) {
}

bool AtomicInfo::shouldCastToInt(Type *ValTy, bool CmpXchg) {
  if (ValTy->isFloatingPointTy())
    return ValTy->isX86_FP80Ty() || CmpXchg;
  return !ValTy->isIntegerTy() && !ValTy->isPointerTy();
}

LLVMContext &AtomicInfo::getLLVMContext() const { return Builder.getContext(); }

AllocaInst *AtomicInfo::CreateAlloca(Type *Ty, const Twine &Name) {
  AllocaInst *Alloca = Builder.CreateAlloca(Ty);
  Alloca->setName(Name);
  return Alloca;
}

Value *AtomicInfo::EmitAtomicLoadOp(AtomicOrdering AO, bool IsVolatile,
                                    bool CmpXchg) {
  Value *Ptr = getAtomicPointer();
  Type *AtomicTy = Ty;
  if (shouldCastToInt(Ty, CmpXchg))
    AtomicTy = IntegerType::get(getLLVMContext(), AtomicSizeInBits);
  LoadInst *Load =
      Builder.CreateAlignedLoad(AtomicTy, Ptr, AtomicAlign, "atomic-load");
  Load->setAtomic(AO);

  // Other decoration.
  if (IsVolatile)
    Load->setVolatile(true);
  decorateWithTBAA(Load);
  return Load;
}

// TODO: Replace with emitLibCall and its helpers from from BuildLibCalls.h
CallInst *AtomicInfo::emitLibCall(StringRef fnName, Type *ResultType,
                                  ArrayRef<Value *> Args) {
  LLVMContext &C = Builder.getContext();

  SmallVector<Type *, 6> ParamTys;
  SmallVector<AttributeSet, 6> ArgAttrs;
  for (Value *Arg : Args) {
    ParamTys.push_back(Arg->getType());

    AttrBuilder AttrB(C);
    if (EnableNoundefAttrs)
      AttrB.addAttribute(Attribute::NoUndef);
    if ((ParamTys.back() == IntTy && IntIsPromotable)) {
      AttrB.addAttribute(Attribute::SExt);
    }
    ArgAttrs.push_back(AttributeSet::get(C, AttrB));
  }
  FunctionType *FnType = FunctionType::get(ResultType, ParamTys, false);
  Module *M = Builder.GetInsertBlock()->getModule();

  AttrBuilder fnAttrB(C);
  decorateFnDeclAttributes(fnAttrB, fnName);
  fnAttrB.addAttribute(Attribute::NoUnwind);
  fnAttrB.addAttribute(Attribute::WillReturn);
  AttributeList fnAttrs =
      AttributeList::get(C, AttributeList::FunctionIndex, fnAttrB);
  FunctionCallee LibcallFn = M->getOrInsertFunction(fnName, FnType, fnAttrs);
  Function *F = cast<Function>(LibcallFn.getCallee());
  F->setCallingConv(cc);
  markRegisterParameterAttributes(F);

  AttrBuilder CallAttrB(C);
  decorateCallAttributes(CallAttrB, fnName);

  AttrBuilder RetAttrB(C);
  if (F->getReturnType()->isIntegerTy(1)) {
    // Clang switches on type-coersion to int for things that can be passed in
    // a register, and adds zeroextend.
    RetAttrB.addAttribute(Attribute::ZExt);

    // It also sets noundef for the return value under certain conditions
    if (EnableNoundefAttrs && BoolHasStrictReturn)
      RetAttrB.addAttribute(Attribute::NoUndef);
  }

  CallInst *CI = Builder.CreateCall(LibcallFn, Args);
  CI->setCallingConv(cc);
  CI->setAttributes(AttributeList::get(C, AttributeSet::get(C, CallAttrB),
                                       AttributeSet::get(C, RetAttrB),
                                       ArgAttrs));

  return CI;
}

Value *AtomicInfo::getAtomicSizeValue() const {
  return ConstantInt::get(SizeTy, AtomicSizeInBits / BitsPerByte);
}

std::pair<Value *, Value *> AtomicInfo::EmitAtomicCompareExchangeOp(
    Value *ExpectedVal, Value *DesiredVal, AtomicOrdering Success,
    AtomicOrdering Failure, bool IsVolatile, bool IsWeak) {
  // Do the atomic store.
  Value *Addr = getAtomicAddressAsAtomicIntPointer();
  AtomicCmpXchgInst *Inst = Builder.CreateAtomicCmpXchg(
      Addr, ExpectedVal, DesiredVal, getAtomicAlignment(), Success, Failure,
      SyncScope::System);
  // Other decoration.
  Inst->setVolatile(IsVolatile);
  Inst->setWeak(IsWeak);

  // Okay, turn that back into the original value type.
  Value *PreviousVal = Builder.CreateExtractValue(Inst, /*Idxs=*/0);
  Value *SuccessFailureVal = Builder.CreateExtractValue(Inst, /*Idxs=*/1);
  return std::make_pair(PreviousVal, SuccessFailureVal);
}

Value *AtomicInfo::EmitAtomicCompareExchangeLibcall(Value *ExpectedPtr,
                                                    Value *DesiredPtr,
                                                    AtomicOrdering Success,
                                                    AtomicOrdering Failure) {
  LLVMContext &C = getLLVMContext();

  // In Clang, the type of the builtin is derived from the concrete arguments.
  // It shouldn't.
  Value *Args[6] = {
      getAtomicSizeValue(),
      getAtomicPointer(),
      ExpectedPtr,
      DesiredPtr,
      Constant::getIntegerValue(IntTy,
                                APInt(IntTy->getScalarSizeInBits(),
                                      (int)toCABI(Success), /*signed=*/true)),
      Constant::getIntegerValue(IntTy,
                                APInt(IntTy->getScalarSizeInBits(),
                                      (int)toCABI(Failure), /*signed=*/true)),
  };
  return emitLibCall("__atomic_compare_exchange", IntegerType::getInt1Ty(C),
                     Args);
}

AtomicResult AtomicInfo::EmitAtomicCompareExchange(
    function_ref<Value *()> MaterializeExpectedVal,
    function_ref<Value *()> MaterializeExpectedPtr,
    function_ref<Value *()> MaterializeDesiredVal,
    function_ref<Value *()> MaterializeDesiredPtr, AtomicOrdering Success,
    AtomicOrdering Failure, bool IsVolatile, bool IsWeak, bool ForceLibcall) {

  // Check whether we should use a library call.
  if (shouldUseLibcall() || ForceLibcall) {
    Value *ExpectedPtr = MaterializeExpectedPtr();
    Value *DesiredPtr = MaterializeDesiredPtr();
    Value *Res = EmitAtomicCompareExchangeLibcall(ExpectedPtr, DesiredPtr,
                                                  Success, Failure);

    return AtomicResult::fromPtr(ExpectedPtr, Res);
  }

  Value *ExpectedVal = MaterializeExpectedVal();
  Value *DesiredVal = MaterializeDesiredVal();
  auto [PreviousVal, SuccessFailureVal] = EmitAtomicCompareExchangeOp(
      ExpectedVal, DesiredVal, Success, Failure, IsVolatile, IsWeak);
  return AtomicResult::fromVal(PreviousVal, SuccessFailureVal);
}

} // namespace llvm
