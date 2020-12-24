; ModuleID = 'output.bc'
source_filename = "divide3.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @g(i32) #0 !dbg !7 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !11, metadata !DIExpression()), !dbg !12
  %2 = sub nsw i32 %0, 5, !dbg !13
  call void @llvm.dbg.value(metadata i32 %2, metadata !11, metadata !DIExpression()), !dbg !12
  %3 = sub nsw i32 %2, 3, !dbg !14
  %4 = sdiv i32 100, %3, !dbg !15
  call void @llvm.dbg.value(metadata i32 %4, metadata !16, metadata !DIExpression()), !dbg !17
  ret i32 %2, !dbg !18
}

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: noinline nounwind uwtable
define dso_local i32 @main(i32) #0 !dbg !19 {
  call void @llvm.dbg.value(metadata i32 %0, metadata !20, metadata !DIExpression()), !dbg !21
  %2 = icmp sgt i32 %0, 0, !dbg !22
  br i1 %2, label %3, label %5, !dbg !24

; <label>:3:                                      ; preds = %1
  %4 = call i32 @g(i32 %0), !dbg !25
  br label %5, !dbg !25

; <label>:5:                                      ; preds = %3, %1
  ret i32 0, !dbg !26
}

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

attributes #0 = { noinline nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5}
!llvm.ident = !{!6}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 8.0.1-9 (tags/RELEASE_801/final)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, nameTableKind: None)
!1 = !DIFile(filename: "divide3.c", directory: "/crucible/crucible-llvm/test")
!2 = !{}
!3 = !{i32 2, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{!"clang version 8.0.1-9 (tags/RELEASE_801/final)"}
!7 = distinct !DISubprogram(name: "g", scope: !1, file: !1, line: 1, type: !8, scopeLine: 1, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!8 = !DISubroutineType(types: !9)
!9 = !{!10, !10}
!10 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!11 = !DILocalVariable(name: "x", arg: 1, scope: !7, file: !1, line: 1, type: !10)
!12 = !DILocation(line: 1, column: 11, scope: !7)
!13 = !DILocation(line: 5, column: 5, scope: !7)
!14 = !DILocation(line: 11, column: 15, scope: !7)
!15 = !DILocation(line: 11, column: 12, scope: !7)
!16 = !DILocalVariable(name: "y", scope: !7, file: !1, line: 11, type: !10)
!17 = !DILocation(line: 11, column: 5, scope: !7)
!18 = !DILocation(line: 13, column: 1, scope: !7)
!19 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 28, type: !8, scopeLine: 28, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!20 = !DILocalVariable(name: "x", arg: 1, scope: !19, file: !1, line: 28, type: !10)
!21 = !DILocation(line: 28, column: 15, scope: !19)
!22 = !DILocation(line: 31, column: 5, scope: !23)
!23 = distinct !DILexicalBlock(scope: !19, file: !1, line: 31, column: 4)
!24 = !DILocation(line: 31, column: 4, scope: !19)
!25 = !DILocation(line: 33, column: 1, scope: !23)
!26 = !DILocation(line: 35, column: 1, scope: !19)
