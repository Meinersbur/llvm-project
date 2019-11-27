; RUN: opt -lof -analyze -debug-pass=Executions -debug-only=lof < %s | FileCheck %s
; XFAIL: * 
;
; for (int j = 0; j < n; j += 1)
;   A[j] = j;
;
define void @func(i64 %n, i64* noalias nonnull %A) {
entry:
  br label %for

for:
  %j = phi i64 [0, %entry], [%j.inc, %inc]
  %j.cmp = icmp slt i64 %j, %n
  br i1 %j.cmp, label %body, label %exit

    body:
      %arrayidx = getelementptr inbounds i64, i64* %A, i64 %j
      store i64 %j, i64* %arrayidx, !llvm.access.group !5
      br label %inc

inc:
  %j.inc = add nuw nsw i64 %j, 1
  br label %for, !llvm.loop !14

exit:
  br label %return

return:
  ret void
}

; Access groups
!5 = distinct !{}

; LoopIDs
!14 = distinct !{!14, !{!"llvm.loop.parallel_accesses", !5}}
