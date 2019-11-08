
; RUN: opt -load LLVMPolly.so -load LLVMPolyJIT.so -O3  -polli  -polli-no-recompilation -polli-analyze -disable-output -stats < %s 2>&1 | FileCheck %s

; CHECK: 1 regions require runtime support:

; ModuleID = '/local/hdd/pjtest/pj-collect/SingleSourceBenchmarks/test-suite/SingleSource/Benchmarks/Polybench/linear-algebra/kernels/cholesky/cholesky.c.main_for.body.22.us.i.pjit.scop.prototype'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind
define weak void @main_for.body.22.us.i.pjit.scop(i64 %indvars.iv26.i, [1024 x double]* %arraydecay2, i64 %indvars.iv32.i, i32 %indvars.iv48, double* %arrayidx18.i)  {
newFuncRoot:
  br label %for.body.22.us.i

for.cond.loopexit.i.loopexit.exitStub:            ; preds = %for.cond.27.for.end.44_crit_edge.us.i
  ret void

for.body.22.us.i:                                 ; preds = %for.cond.27.for.end.44_crit_edge.us.i, %newFuncRoot
  %indvars.iv28.i = phi i64 [ %indvars.iv.next29.i, %for.cond.27.for.end.44_crit_edge.us.i ], [ %indvars.iv26.i, %newFuncRoot ]
  %arrayidx26.us.i = getelementptr inbounds [1024 x double], [1024 x double]* %arraydecay2, i64 %indvars.iv32.i, i64 %indvars.iv28.i
  %0 = load double, double* %arrayidx26.us.i, align 8, !tbaa !0
  br label %for.body.31.us.i

for.body.31.us.i:                                 ; preds = %for.body.31.us.i, %for.body.22.us.i
  %indvars.iv20.i = phi i64 [ 0, %for.body.22.us.i ], [ %indvars.iv.next21.i, %for.body.31.us.i ]
  %x.16.us.i = phi double [ %0, %for.body.22.us.i ], [ %sub41.us.i, %for.body.31.us.i ]
  %arrayidx35.us.i = getelementptr inbounds [1024 x double], [1024 x double]* %arraydecay2, i64 %indvars.iv28.i, i64 %indvars.iv20.i
  %1 = load double, double* %arrayidx35.us.i, align 8, !tbaa !0
  %arrayidx39.us.i = getelementptr inbounds [1024 x double], [1024 x double]* %arraydecay2, i64 %indvars.iv32.i, i64 %indvars.iv20.i
  %2 = load double, double* %arrayidx39.us.i, align 8, !tbaa !0
  %mul40.us.i = fmul double %1, %2
  %sub41.us.i = fsub double %x.16.us.i, %mul40.us.i
  %indvars.iv.next21.i = add nuw nsw i64 %indvars.iv20.i, 1
  %lftr.wideiv50 = trunc i64 %indvars.iv.next21.i to i32
  %exitcond51 = icmp eq i32 %lftr.wideiv50, %indvars.iv48
  br i1 %exitcond51, label %for.cond.27.for.end.44_crit_edge.us.i, label %for.body.31.us.i

for.cond.27.for.end.44_crit_edge.us.i:            ; preds = %for.body.31.us.i
  %sub41.us.i.lcssa = phi double [ %sub41.us.i, %for.body.31.us.i ]
  %3 = load double, double* %arrayidx18.i, align 8, !tbaa !0
  %mul47.us.i = fmul double %sub41.us.i.lcssa, %3
  %arrayidx51.us.i = getelementptr inbounds [1024 x double], [1024 x double]* %arraydecay2, i64 %indvars.iv28.i, i64 %indvars.iv32.i
  store double %mul47.us.i, double* %arrayidx51.us.i, align 8, !tbaa !0
  %indvars.iv.next29.i = add nuw nsw i64 %indvars.iv28.i, 1
  %lftr.wideiv52 = trunc i64 %indvars.iv.next29.i to i32
  %exitcond53 = icmp eq i32 %lftr.wideiv52, 1024
  br i1 %exitcond53, label %for.cond.loopexit.i.loopexit.exitStub, label %for.body.22.us.i
}

attributes #0 = { nounwind "polyjit-global-count"="0" "polyjit-jit-candidate" }

!0 = !{!1, !1, i64 0}
!1 = !{!"double", !2, i64 0}
!2 = !{!"omnipotent char", !3, i64 0}
!3 = !{!"Simple C/C++ TBAA"}
