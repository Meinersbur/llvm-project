// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc -std=c++11 -emit-llvm -o - %s | FileCheck %s

extern "C" void pragma_transform_interleave_factor(int *List, int Length) {
#pragma clang transform interleave factor(2)
  for (int i = 0; i < Length; i++) {
// CHECK: br label %{{.*}}, !llvm.loop ![[LOOP:[0-9]+]]
    List[i] = i * 2;
  }
}


// CHECK-DAG: ![[LOOP]] = distinct !{![[LOOP]], ![[VECTORIZE_DISABLE:[0-9]+]],  ![[INTERLEAVE_COUNT:[0-9]+]], ![[INTERLEAVE_ENABLE:[0-9]+]], ![[VECTORIZE_FOLLOWUP_ALL:[0-9]+]], ![[DISABLE_NONFORCED:[0-9]+]]}
// CHECK-DAG: ![[VECTORIZE_DISABLE]] = !{!"llvm.loop.vectorize.width", i32 1}
// CHECK-DAG: ![[INTERLEAVE_COUNT]] = !{!"llvm.loop.interleave.count", i32 2}
// CHECK-DAG: ![[INTERLEAVE_ENABLE]] = !{!"llvm.loop.vectorize.enable", i1 true}
// CHECK-DAG: ![[VECTORIZE_FOLLOWUP_ALL]] = !{!"llvm.loop.vectorize.followup_all", ![[LOOP_VECTORIZE_ALL:[0-9]+]]}
// CHECK-DAG: ![[DISABLE_NONFORCED]] = !{!"llvm.loop.disable_nonforced"}

// CHECK-DAG: ![[LOOP_VECTORIZE_ALL]] = distinct !{![[LOOP_VECTORIZE_ALL]], ![[DISABLE_NONFORCED:[0-9]+]]}
