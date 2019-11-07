// RUN: %clang_cc1 -triple x86_64-pc-windows-msvc -std=c++11 -fexperimental-transform-pragma -emit-llvm -o - %s | FileCheck %s

extern "C" void pragma_transform_vectorize(int *List, int Length) {
#pragma clang transform vectorize width(4)
  for (int i = 0; i < Length; i++) {
// CHECK: br label %{{.*}}, !llvm.loop ![[LOOP:[0-9]+]]
    List[i] = i * 2;
  }
}


// CHECK-DAG: ![[LOOP]] = distinct !{![[LOOP]], ![[VECTORIZE_WIDTH:[0-9]+]], ![[INTERLEAVE_DISABLE:[0-9]+]], ![[VECTORIZE_ENABLE:[0-9]+]], ![[VECTORIZE_FOLLOWUP_ALL:[0-9]+]], ![[DISABLE_NONFORCED:[0-9]+]]}
// CHECK-DAG: ![[VECTORIZE_WIDTH]] = !{!"llvm.loop.vectorize.width", i32 4}
// CHECK-DAG: ![[INTERLEAVE_DISABLE]] = !{!"llvm.loop.interleave.count", i32 1}
// CHECK-DAG: ![[VECTORIZE_ENABLE]] = !{!"llvm.loop.vectorize.enable", i1 true}
// CHECK-DAG: ![[VECTORIZE_FOLLOWUP_ALL]] = !{!"llvm.loop.vectorize.followup_all", ![[LOOP_VECTORIZE_ALL:[0-9]+]]}
// CHECK-DAG: ![[DISABLE_NONFORCED]] = !{!"llvm.loop.disable_nonforced"}

// CHECK-DAG: ![[LOOP_VECTORIZE_ALL]] = distinct !{![[LOOP_VECTORIZE_ALL]], ![[DISABLE_NONFORCED:[0-9]+]]}
