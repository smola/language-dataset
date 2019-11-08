; ModuleID = '<stdin>'
source_filename = "c/05-internal-multiple-files.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define void @a() #0 !dbg !6 {
  ret void, !dbg !9
}

; Function Attrs: nounwind uwtable
define void @b() #0 !dbg !10 {
  call void @a(), !dbg !11
  ret void, !dbg !12
}

; Function Attrs: nounwind uwtable
define i32 @main(i32, i8**) #0 !dbg !13 {
  call void @llvm.dbg.value(metadata i32 %0, i64 0, metadata !20, metadata !21), !dbg !22
  call void @llvm.dbg.value(metadata i8** %1, i64 0, metadata !23, metadata !21), !dbg !24
  call void @b(), !dbg !25
  call void @b(), !dbg !28
  call void @b(), !dbg !31
  ret i32 0, !dbg !32
}

; Function Attrs: nounwind readnone
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: nounwind readnone
declare void @llvm.dbg.value(metadata, i64, metadata, metadata) #1

attributes #0 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4}
!llvm.ident = !{!5}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 3.9.0 (tags/RELEASE_390/final)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2)
!1 = !DIFile(filename: "c/05-internal-multiple-files.c", directory: "/home/nick/teaching/886/call-profiler/test")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{!"clang version 3.9.0 (tags/RELEASE_390/final)"}
!6 = distinct !DISubprogram(name: "a", scope: !1, file: !1, line: 3, type: !7, isLocal: false, isDefinition: true, scopeLine: 3, isOptimized: false, unit: !0, variables: !2)
!7 = !DISubroutineType(types: !8)
!8 = !{null}
!9 = !DILocation(line: 3, column: 11, scope: !6)
!10 = distinct !DISubprogram(name: "b", scope: !1, file: !1, line: 5, type: !7, isLocal: false, isDefinition: true, scopeLine: 5, isOptimized: false, unit: !0, variables: !2)
!11 = !DILocation(line: 6, column: 3, scope: !10)
!12 = !DILocation(line: 7, column: 1, scope: !10)
!13 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 10, type: !14, isLocal: false, isDefinition: true, scopeLine: 10, flags: DIFlagPrototyped, isOptimized: false, unit: !0, variables: !2)
!14 = !DISubroutineType(types: !15)
!15 = !{!16, !16, !17}
!16 = !DIBasicType(name: "int", size: 32, align: 32, encoding: DW_ATE_signed)
!17 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !18, size: 64, align: 64)
!18 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !19, size: 64, align: 64)
!19 = !DIBasicType(name: "char", size: 8, align: 8, encoding: DW_ATE_signed_char)
!20 = !DILocalVariable(name: "argc", arg: 1, scope: !13, file: !1, line: 10, type: !16)
!21 = !DIExpression()
!22 = !DILocation(line: 10, column: 10, scope: !13)
!23 = !DILocalVariable(name: "argv", arg: 2, scope: !13, file: !1, line: 10, type: !17)
!24 = !DILocation(line: 10, column: 23, scope: !13)
!25 = !DILocation(line: 0, column: 3, scope: !26)
!26 = !DILexicalBlockFile(scope: !13, file: !27, discriminator: 0)
!27 = !DIFile(filename: "file1.c", directory: "/home/nick/teaching/886/call-profiler/test")
!28 = !DILocation(line: 0, column: 3, scope: !29)
!29 = !DILexicalBlockFile(scope: !13, file: !30, discriminator: 0)
!30 = !DIFile(filename: "file2.c", directory: "/home/nick/teaching/886/call-profiler/test")
!31 = !DILocation(line: 65536, column: 3, scope: !29)
!32 = !DILocation(line: 65537, column: 3, scope: !29)
