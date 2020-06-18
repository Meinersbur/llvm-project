#ifdef IR
; RUN: opt < %s -passes=loop-vectorize -S 2>&1 | FileCheck %s

%type = type { [3 x double] }

define void @getScalarFunc(double* %A, double* %C, %type* %B) {
; CHECK-LABEL: getScalarFunc
; This check will catch also the massv version of the function.
; CHECK-NOT: call fast <{{[0-9]+}} x double> @{{.*}}atan(<{{[0-9]+}} x double> %{{[0-9]+}})
entry:
  br label %for.body

for.body:
  %i = phi i64 [ %inc, %for.body ], [ 0, %entry ]
  %dummyload2 = load double, double* %A, align 8
  %arrayidx.i24 = getelementptr inbounds %type, %type* %B, i64 %i, i32 0, i32 0
  %_15 = load double, double* %arrayidx.i24, align 8
  %call10 = tail call fast double @atan(double %_15) #0
  %inc = add i64 %i, 1
  %cmp = icmp ugt i64 1000, %inc
  br i1 %cmp, label %for.body, label %for.end

for.end:
  ret void
}

declare double @atan(double) local_unnamed_addr
declare <2 x double> @vector_atan(<2 x double>) #0
attributes #0 = { nounwind readnone "vector-function-abi-variant"="_ZGV_LLVM_N2v_atan(vector_atan)" }
#else /* IR */

#include "compiledtestboilerplate.h"
using namespace llvm;


TEST(VectorizeVFone, getScalarFunc) {
  auto M = run_opt(__FILE__, "IR", "-passes=loop-vectorize");
  auto getScalarFunc = M->getFunc("getScalarFunc");

  // Set of calls to atan
  auto Calls = getScalarFunc.call_insts();

  // Expect at least one call in the output.
  Calls.expectMinCount(1);

  // Return type must not be a vector
  ASSERT_TRUE(
    Calls.all_of([](CallInst* CI)->bool { return !isa<VectorType>(CI->getType()); })
  );

  // Argument must not be vectors
  ASSERT_TRUE(
    Calls.operands()
    .all_of([](const llvm::Use& V) ->bool {return !isa<llvm::VectorType>(V.get()->getType()); })
  );

   // Actually, there must be no vector at all
   ASSERT_TRUE(getScalarFunc.operands()
     .all_of([](const llvm::Use& U) ->bool {
      return! isa<llvm::VectorType>(U.get()->getType());
      })
    );
}

#endif /* IR */
