// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc19.0.24215 -std=c99 -ast-print %s | FileCheck --match-full-lines %s --check-prefix=PRINT
// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc19.0.24215 -std=c99 -emit-llvm -disable-llvm-passes -o - %s | FileCheck %s --check-prefix=IR
// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc19.0.24215 -std=c99 -emit-llvm -mllvm -polly-use-llvm-names -O3 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-process-unprofitable -mllvm -polly-omp-backend=LLVM -mllvm -debug-only=polly-ast -o /dev/null %s 2>&1 > /dev/null | FileCheck %s --check-prefix=AST
// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc19.0.24215 -std=c99 -emit-llvm -mllvm -polly-use-llvm-names -O3 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-process-unprofitable -mllvm -polly-omp-backend=LLVM -o - %s | FileCheck %s --check-prefix=TRANS
// RUN: %clang     -DMAIN                                   -std=c99            -mllvm -polly-use-llvm-names -O3 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-process-unprofitable -mllvm -polly-omp-backend=LLVM -fopenmp %s -o %t_pragma_pack%exeext
// RUN: %t_pragma_pack%exeext | FileCheck %s --check-prefix=RESULT

void pragma_id_unrolling_parallelize(int n, double A[n]) {
  #pragma clang loop(j) parallelize_thread
  #pragma clang loop(i) unrolling factor(2) unrolled_id(j)
  #pragma clang loop id(i)
  for (int i = 0; i < n; i += 1) {
    A[i] = 3*i + 42;
  }
}

#ifdef MAIN
#include <stdio.h>
#include <string.h>
int main() {
  double A[256];
  memset(A, 0, sizeof(A));
  pragma_id_unrolling_parallelize(256,A);
  printf("(%0.0f %0.0f)\n", A[0], A[1]);
  return 0;
}
#endif


// PRINT-LABEL: void pragma_id_unrolling_parallelize(int n, double A[n]) {
// PRINT-NEXT:    #pragma clang loop(j) parallelize_thread
// PRINT-NEXT:    #pragma clang loop(i) unrolling factor(2) unrolled_id(j)
// PRINT-NEXT:    #pragma clang loop id(i)
// PRINT-NEXT:    for (int i = 0; i < n; i += 1) {
// PRINT-NEXT:      A[i] = 3 * i + 42;
// PRINT-NEXT:    }
// PRINT-NEXT:  }


// IR-LABEL: void @pragma_id_unrolling_parallelize(
// IR:         br label %for.cond, !llvm.loop !2
//
// IR: !2 = distinct !{!2, !3, !4, !5, !6, !7}
// IR: !3 = !{!"llvm.loop.disable_nonforced"}
// IR: !4 = !{!"llvm.loop.id", !"i"}
// IR: !5 = !{!"llvm.loop.unroll.enable", i1 true}
// IR: !6 = !{!"llvm.loop.unroll.count", i3 2}
// IR: !7 = !{!"llvm.loop.unroll.followup_unrolled", !8}
// IR: !8 = distinct !{!8, !3, !9, !10}
// IR: !9 = !{!"llvm.loop.id", !"j"}
// IR: !10 = !{!"llvm.loop.parallelize_thread.enable", i1 true}


// AST: if (1
// AST:       #pragma omp parallel for
// AST:       for (int c0 = 0; c0 < n; c0 += 2) {
// AST:         Stmt_for_body(c0);
// AST:         if (n >= c0 + 2)
// AST:           Stmt_for_body(c0 + 1);
// AST:   else
// AST:     {  /* original code */ }


// TRANS-LABEL: void @pragma_id_unrolling_parallelize(
// TRANS:         @__kmpc_fork_call(ptr nonnull @.loc.dummy, i32 4, ptr nonnull @pragma_id_unrolling_parallelize_polly_subfn,
// TRANS:      }
//
// TRANS-LABEL: void @pragma_id_unrolling_parallelize_polly_subfn(
// TRANS:         store double %p_conv, ptr %uglygep, align 8, !alias.scope !2, !noalias !5
// TRANS:         store double %p_conv2, ptr %uglygep3, align 8, !alias.scope !2, !noalias !5
// TRANS:      }


// RESULT: (42 45)
