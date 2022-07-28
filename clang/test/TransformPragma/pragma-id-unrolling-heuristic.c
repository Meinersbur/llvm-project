// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc19.0.24215 -std=c99 -ast-print %s | FileCheck --match-full-lines %s --check-prefix=PRINT
// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc19.0.24215 -std=c99 -emit-llvm -disable-llvm-passes -o - %s | FileCheck %s --check-prefix=IR
// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc19.0.24215 -std=c99 -emit-llvm -mllvm -polly-use-llvm-names -O3 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-process-unprofitable -mllvm -debug-only=polly-ast -o /dev/null %s 2>&1 > /dev/null | FileCheck %s --check-prefix=AST
// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc19.0.24215 -std=c99 -emit-llvm -mllvm -polly-use-llvm-names -O3 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-process-unprofitable -o - %s | FileCheck %s --check-prefix=TRANS
// RUN: %clang     -DMAIN                                   -std=c99            -mllvm -polly-use-llvm-names -O3 -mllvm -polly -mllvm -polly-position=early -mllvm -polly-process-unprofitable %s -o %t_pragma_pack%exeext
// RUN: %t_pragma_pack%exeext | FileCheck %s --check-prefix=RESULT

void pragma_id_unrolling_heuristic(int n, double A[n]) {
  #pragma clang loop(i) unrolling
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
  pragma_id_unrolling_heuristic(256,A);
  printf("(%0.0f %0.0f)\n", A[0], A[1]);
  return 0;
}
#endif


// PRINT-LABEL: void pragma_id_unrolling_heuristic(int n, double A[n]) {
// PRINT-NEXT:    #pragma clang loop(i) unrolling
// PRINT-NEXT:    #pragma clang loop id(i)
// PRINT-NEXT:    for (int i = 0; i < n; i += 1) {
// PRINT-NEXT:      A[i] = 3 * i + 42;
// PRINT-NEXT:    }
// PRINT-NEXT:  }


// IR-LABEL: void @pragma_id_unrolling_heuristic(
// IR:         br label %for.cond, !llvm.loop !2
//
// IR: !2 = distinct !{!2, !3, !4, !5}
// IR: !3 = !{!"llvm.loop.disable_nonforced"}
// IR: !4 = !{!"llvm.loop.id", !"i"}
// IR: !5 = !{!"llvm.loop.unroll.enable", i1 true}


// AST: if (1
// AST:       // Loop with Metadata
// AST:       for (int c0 = 0; c0 < n; c0 += 1)
// AST:         Stmt_for_body(c0);
// AST:   else
// AST:     {  /* original code */ }


// Small loop, LoopUnroll should unroll at 4x
// TRANS-LABEL: void @pragma_id_unrolling_heuristic(
// TRANS:         store double %p_conv{{.*}}, ptr %uglygep{{.*}}, align 8, !alias.scope !2, !noalias !5
// TRANS:         store double %p_conv{{.*}}, ptr %uglygep{{.*}}, align 8, !alias.scope !2, !noalias !5
// TRANS:         store double %p_conv{{.*}}, ptr %uglygep{{.*}}, align 8, !alias.scope !2, !noalias !5
// TRANS:         store double %p_conv{{.*}}, ptr %uglygep{{.*}}, align 8, !alias.scope !2, !noalias !5
// TRANS:      }
// TRANS:      !{!"llvm.loop.polly.done"}


// RESULT: (42 45)
