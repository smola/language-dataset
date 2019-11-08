; ModuleID = 'F:/xilixproject/project_hog12/hls/gray2scale/solution1/.autopilot/db/a.o.2.bc'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-w64-mingw32"

@llvm_global_ctors_1 = appending global [1 x void ()*] [void ()* @_GLOBAL__I_a]
@llvm_global_ctors_0 = appending global [1 x i32] [i32 65535]
@img_2_OC_data_stream_LF_0_NF_s = internal unnamed_addr constant [23 x i8] c"img_2.data_stream[0].V\00"
@img_1_OC_data_stream_LF_0_NF_s = internal unnamed_addr constant [23 x i8] c"img_1.data_stream[0].V\00"
@img_0_OC_data_stream_LF_2_NF_s = internal unnamed_addr constant [23 x i8] c"img_0.data_stream[2].V\00"
@img_0_OC_data_stream_LF_1_NF_s = internal unnamed_addr constant [23 x i8] c"img_0.data_stream[1].V\00"
@img_0_OC_data_stream_LF_0_NF_s = internal unnamed_addr constant [23 x i8] c"img_0.data_stream[0].V\00"
@image_filter_str = internal unnamed_addr constant [13 x i8] c"image_filter\00"
@hls_KD_KD_LineBuffer_MD_2_MC_s = internal unnamed_addr constant [80 x i8] c"hls::LineBuffer<2, 1921, hls::Scalar<1, unsigned char>, 0>::LineBuffer.1.region\00"
@ap_stable_str = internal unnamed_addr constant [10 x i8] c"ap_stable\00"
@ap_fifo_str = internal unnamed_addr constant [8 x i8] c"ap_fifo\00"
@p_str1841 = private unnamed_addr constant [13 x i8] c"hls_label_17\00", align 1
@p_str1840 = private unnamed_addr constant [18 x i8] c"loop_wait_for_eol\00", align 1
@p_str1839 = private unnamed_addr constant [20 x i8] c"loop_wait_for_start\00", align 1
@p_str1837 = private unnamed_addr constant [13 x i8] c"hls_label_20\00", align 1
@p_str1824 = private unnamed_addr constant [13 x i8] c"hls_label_21\00", align 1
@p_str1823 = private unnamed_addr constant [13 x i8] c"hls_label_27\00", align 1
@p_str1820 = private unnamed_addr constant [13 x i8] c"hls_label_25\00", align 1
@p_str1817 = private unnamed_addr constant [14 x i8] c"loop_channels\00", align 1
@p_str1816 = private unnamed_addr constant [11 x i8] c"loop_width\00", align 1
@p_str1815 = private unnamed_addr constant [12 x i8] c"loop_height\00", align 1
@p_str1809 = private unnamed_addr constant [10 x i8] c"ap_stable\00", align 1
@p_str1808 = private unnamed_addr constant [24 x i8] c"-bus_bundle CONTROL_BUS\00", align 1
@p_str1807 = private unnamed_addr constant [10 x i8] c"AXI_SLAVE\00", align 1
@p_str1806 = private unnamed_addr constant [1 x i8] zeroinitializer, align 1
@p_str1805 = private unnamed_addr constant [5 x i8] c"axis\00", align 1
@p_str = internal unnamed_addr constant [1 x i8] zeroinitializer

declare i50 @llvm.part.select.i50(i50, i32, i32) nounwind readnone

declare i43 @llvm.part.select.i43(i43, i32, i32) nounwind readnone

declare i32 @llvm.part.select.i32(i32, i32, i32) nounwind readnone

declare i30 @llvm.part.select.i30(i30, i32, i32) nounwind readnone

declare i27 @llvm.part.select.i27(i27, i32, i32) nounwind readnone

declare void @llvm.dbg.value(metadata, i64, metadata) nounwind readnone

declare void @llvm.dbg.declare(metadata, metadata) nounwind readnone

define void @image_filter(i32* %INPUT_STREAM_V_data_V, i4* %INPUT_STREAM_V_keep_V, i4* %INPUT_STREAM_V_strb_V, i1* %INPUT_STREAM_V_user_V, i1* %INPUT_STREAM_V_last_V, i1* %INPUT_STREAM_V_id_V, i1* %INPUT_STREAM_V_dest_V, i8* %OUTPUT_STREAM_V_data_V, i1* %OUTPUT_STREAM_V_keep_V, i1* %OUTPUT_STREAM_V_strb_V, i1* %OUTPUT_STREAM_V_user_V, i1* %OUTPUT_STREAM_V_last_V, i1* %OUTPUT_STREAM_V_id_V, i1* %OUTPUT_STREAM_V_dest_V, i32 %rows, i32 %cols) {
Mat.exit:
  call void (...)* @_ssdm_op_SpecDataflowPipeline(i32 -1, [1 x i8]* @p_str1806) nounwind
  call void (...)* @_ssdm_op_SpecBitsMap(i32* %INPUT_STREAM_V_data_V), !map !7
  call void (...)* @_ssdm_op_SpecBitsMap(i4* %INPUT_STREAM_V_keep_V), !map !11
  call void (...)* @_ssdm_op_SpecBitsMap(i4* %INPUT_STREAM_V_strb_V), !map !15
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %INPUT_STREAM_V_user_V), !map !19
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %INPUT_STREAM_V_last_V), !map !23
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %INPUT_STREAM_V_id_V), !map !27
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %INPUT_STREAM_V_dest_V), !map !31
  call void (...)* @_ssdm_op_SpecBitsMap(i8* %OUTPUT_STREAM_V_data_V), !map !35
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %OUTPUT_STREAM_V_keep_V), !map !39
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %OUTPUT_STREAM_V_strb_V), !map !43
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %OUTPUT_STREAM_V_user_V), !map !47
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %OUTPUT_STREAM_V_last_V), !map !51
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %OUTPUT_STREAM_V_id_V), !map !55
  call void (...)* @_ssdm_op_SpecBitsMap(i1* %OUTPUT_STREAM_V_dest_V), !map !59
  call void (...)* @_ssdm_op_SpecBitsMap(i32 %rows), !map !63
  call void (...)* @_ssdm_op_SpecBitsMap(i32 %cols), !map !69
  call void (...)* @_ssdm_op_SpecTopModule([13 x i8]* @image_filter_str) nounwind
  %cols_read = call i32 @_ssdm_op_Read.ap_stable.i32(i32 %cols)
  %rows_read = call i32 @_ssdm_op_Read.ap_stable.i32(i32 %rows)
  %img_0_data_stream_0_V = alloca i8, align 1
  %empty = call i32 (...)* @_ssdm_op_SpecChannel([23 x i8]* @img_0_OC_data_stream_LF_0_NF_s, i32 1, [1 x i8]* @p_str, [1 x i8]* @p_str, i32 1, i32 1, i8* %img_0_data_stream_0_V, i8* %img_0_data_stream_0_V)
  call void (...)* @_ssdm_op_SpecInterface(i8* %img_0_data_stream_0_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  %img_0_data_stream_1_V = alloca i8, align 1
  %empty_14 = call i32 (...)* @_ssdm_op_SpecChannel([23 x i8]* @img_0_OC_data_stream_LF_1_NF_s, i32 1, [1 x i8]* @p_str, [1 x i8]* @p_str, i32 1, i32 1, i8* %img_0_data_stream_1_V, i8* %img_0_data_stream_1_V)
  call void (...)* @_ssdm_op_SpecInterface(i8* %img_0_data_stream_1_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  %img_0_data_stream_2_V = alloca i8, align 1
  %empty_15 = call i32 (...)* @_ssdm_op_SpecChannel([23 x i8]* @img_0_OC_data_stream_LF_2_NF_s, i32 1, [1 x i8]* @p_str, [1 x i8]* @p_str, i32 1, i32 1, i8* %img_0_data_stream_2_V, i8* %img_0_data_stream_2_V)
  call void (...)* @_ssdm_op_SpecInterface(i8* %img_0_data_stream_2_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  %img_1_data_stream_0_V = alloca i8, align 1
  %empty_16 = call i32 (...)* @_ssdm_op_SpecChannel([23 x i8]* @img_1_OC_data_stream_LF_0_NF_s, i32 1, [1 x i8]* @p_str, [1 x i8]* @p_str, i32 1, i32 1, i8* %img_1_data_stream_0_V, i8* %img_1_data_stream_0_V)
  call void (...)* @_ssdm_op_SpecInterface(i8* %img_1_data_stream_0_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  %img_2_data_stream_0_V = alloca i8, align 1
  %empty_17 = call i32 (...)* @_ssdm_op_SpecChannel([23 x i8]* @img_2_OC_data_stream_LF_0_NF_s, i32 1, [1 x i8]* @p_str, [1 x i8]* @p_str, i32 1, i32 1, i8* %img_2_data_stream_0_V, i8* %img_2_data_stream_0_V)
  call void (...)* @_ssdm_op_SpecInterface(i8* %img_2_data_stream_0_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i32* %INPUT_STREAM_V_data_V, i4* %INPUT_STREAM_V_keep_V, i4* %INPUT_STREAM_V_strb_V, i1* %INPUT_STREAM_V_user_V, i1* %INPUT_STREAM_V_last_V, i1* %INPUT_STREAM_V_id_V, i1* %INPUT_STREAM_V_dest_V, [5 x i8]* @p_str1805, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806) nounwind
  call void (...)* @_ssdm_op_SpecInterface(i8* %OUTPUT_STREAM_V_data_V, i1* %OUTPUT_STREAM_V_keep_V, i1* %OUTPUT_STREAM_V_strb_V, i1* %OUTPUT_STREAM_V_user_V, i1* %OUTPUT_STREAM_V_last_V, i1* %OUTPUT_STREAM_V_id_V, i1* %OUTPUT_STREAM_V_dest_V, [5 x i8]* @p_str1805, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806) nounwind
  call void (...)* @_ssdm_op_SpecIFCore(i32 %rows, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecIFCore(i32 %cols, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecIFCore(i32 0, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecInterface(i32 %rows, [10 x i8]* @p_str1809, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806) nounwind
  call void (...)* @_ssdm_op_SpecInterface(i32 %cols, [10 x i8]* @p_str1809, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806) nounwind
  %tmp = trunc i32 %rows_read to i11
  %tmp_1 = trunc i32 %cols_read to i11
  call fastcc void @image_filter_AXIvideo2Mat(i32* %INPUT_STREAM_V_data_V, i4* %INPUT_STREAM_V_keep_V, i4* %INPUT_STREAM_V_strb_V, i1* %INPUT_STREAM_V_user_V, i1* %INPUT_STREAM_V_last_V, i1* %INPUT_STREAM_V_id_V, i1* %INPUT_STREAM_V_dest_V, i11 %tmp, i11 %tmp_1, i8* %img_0_data_stream_0_V, i8* %img_0_data_stream_1_V, i8* %img_0_data_stream_2_V)
  call fastcc void @image_filter_CvtColor(i11 %tmp, i11 %tmp_1, i8* %img_0_data_stream_0_V, i8* %img_0_data_stream_1_V, i8* %img_0_data_stream_2_V, i8* %img_1_data_stream_0_V)
  call fastcc void @image_filter_Resize(i11 %tmp, i11 %tmp_1, i8* %img_1_data_stream_0_V, i8* %img_2_data_stream_0_V)
  call fastcc void @"image_filter_Mat2AXIvideo<8, 1080, 1920, 0>"(i11 %tmp, i11 %tmp_1, i8* %img_2_data_stream_0_V, i8* %OUTPUT_STREAM_V_data_V, i1* %OUTPUT_STREAM_V_keep_V, i1* %OUTPUT_STREAM_V_strb_V, i1* %OUTPUT_STREAM_V_user_V, i1* %OUTPUT_STREAM_V_last_V, i1* %OUTPUT_STREAM_V_id_V, i1* %OUTPUT_STREAM_V_dest_V)
  ret void
}

define weak void @_ssdm_op_Write.axis.volatile.i8P.i1P.i1P.i1P.i1P.i1P.i1P(i8*, i1*, i1*, i1*, i1*, i1*, i1*, i8, i1, i1, i1, i1, i1, i1) {
entry:
  store i8 %7, i8* %0
  store i1 %8, i1* %1
  store i1 %9, i1* %2
  store i1 %10, i1* %3
  store i1 %11, i1* %4
  store i1 %12, i1* %5
  store i1 %13, i1* %6
  ret void
}

define weak void @_ssdm_op_Write.ap_fifo.volatile.i8P(i8*, i8) {
entry:
  %empty = call i8 @_autotb_FifoWrite_i8(i8* %0, i8 %1)
  ret void
}

define weak void @_ssdm_op_SpecTopModule(...) {
entry:
  ret void
}

define weak i32 @_ssdm_op_SpecRegionEnd(...) {
entry:
  ret i32 0
}

define weak i32 @_ssdm_op_SpecRegionBegin(...) {
entry:
  ret i32 0
}

define weak void @_ssdm_op_SpecProtocol(...) nounwind {
entry:
  ret void
}

define weak void @_ssdm_op_SpecPipeline(...) nounwind {
entry:
  ret void
}

define weak void @_ssdm_op_SpecLoopTripCount(...) nounwind {
entry:
  ret void
}

define weak void @_ssdm_op_SpecLoopName(...) nounwind {
entry:
  ret void
}

define weak void @_ssdm_op_SpecInterface(...) nounwind {
entry:
  ret void
}

define weak void @_ssdm_op_SpecIFCore(...) {
entry:
  ret void
}

define weak void @_ssdm_op_SpecDataflowPipeline(...) nounwind {
entry:
  ret void
}

define weak i32 @_ssdm_op_SpecChannel(...) {
entry:
  ret i32 0
}

define weak void @_ssdm_op_SpecBitsMap(...) {
entry:
  ret void
}

define weak { i32, i4, i4, i1, i1, i1, i1 } @_ssdm_op_Read.axis.volatile.i32P.i4P.i4P.i1P.i1P.i1P.i1P(i32*, i4*, i4*, i1*, i1*, i1*, i1*) {
entry:
  %empty = load i32* %0
  %empty_18 = load i4* %1
  %empty_19 = load i4* %2
  %empty_20 = load i1* %3
  %empty_21 = load i1* %4
  %empty_22 = load i1* %5
  %empty_23 = load i1* %6
  %mrv_0 = insertvalue { i32, i4, i4, i1, i1, i1, i1 } undef, i32 %empty, 0
  %mrv1 = insertvalue { i32, i4, i4, i1, i1, i1, i1 } %mrv_0, i4 %empty_18, 1
  %mrv2 = insertvalue { i32, i4, i4, i1, i1, i1, i1 } %mrv1, i4 %empty_19, 2
  %mrv3 = insertvalue { i32, i4, i4, i1, i1, i1, i1 } %mrv2, i1 %empty_20, 3
  %mrv4 = insertvalue { i32, i4, i4, i1, i1, i1, i1 } %mrv3, i1 %empty_21, 4
  %mrv5 = insertvalue { i32, i4, i4, i1, i1, i1, i1 } %mrv4, i1 %empty_22, 5
  %mrv6 = insertvalue { i32, i4, i4, i1, i1, i1, i1 } %mrv5, i1 %empty_23, 6
  ret { i32, i4, i4, i1, i1, i1, i1 } %mrv6
}

define weak i32 @_ssdm_op_Read.ap_stable.i32(i32) {
entry:
  ret i32 %0
}

define weak i11 @_ssdm_op_Read.ap_stable.i11(i11) {
entry:
  ret i11 %0
}

define weak i8 @_ssdm_op_Read.ap_fifo.volatile.i8P(i8*) {
entry:
  %empty = call i8 @_autotb_FifoRead_i8(i8* %0)
  ret i8 %empty
}

define weak i11 @_ssdm_op_Read.ap_auto.i11(i11) {
entry:
  ret i11 %0
}

define weak i8 @_ssdm_op_PartSelect.i8.i50.i32.i32(i50, i32, i32) nounwind readnone {
entry:
  %empty = call i50 @llvm.part.select.i50(i50 %0, i32 %1, i32 %2)
  %empty_24 = trunc i50 %empty to i8
  ret i8 %empty_24
}

define weak i8 @_ssdm_op_PartSelect.i8.i32.i32.i32(i32, i32, i32) nounwind readnone {
entry:
  %empty = call i32 @llvm.part.select.i32(i32 %0, i32 %1, i32 %2)
  %empty_25 = trunc i32 %empty to i8
  ret i8 %empty_25
}

define weak i8 @_ssdm_op_PartSelect.i8.i30.i32.i32(i30, i32, i32) nounwind readnone {
entry:
  %empty = call i30 @llvm.part.select.i30(i30 %0, i32 %1, i32 %2)
  %empty_26 = trunc i30 %empty to i8
  ret i8 %empty_26
}

define weak i4 @_ssdm_op_PartSelect.i4.i50.i32.i32(i50, i32, i32) nounwind readnone {
entry:
  %empty = call i50 @llvm.part.select.i50(i50 %0, i32 %1, i32 %2)
  %empty_27 = trunc i50 %empty to i4
  ret i4 %empty_27
}

declare i32 @_ssdm_op_PartSelect.i32.i43.i32.i32(i43, i32, i32) nounwind readnone

declare i28 @_ssdm_op_PartSelect.i28.i43.i32.i32(i43, i32, i32) nounwind readnone

declare i27 @_ssdm_op_PartSelect.i27.i43.i32.i32(i43, i32, i32) nounwind readnone

define weak i26 @_ssdm_op_PartSelect.i26.i43.i32.i32(i43, i32, i32) nounwind readnone {
entry:
  %empty = call i43 @llvm.part.select.i43(i43 %0, i32 %1, i32 %2)
  %empty_28 = trunc i43 %empty to i26
  ret i26 %empty_28
}

define weak i20 @_ssdm_op_PartSelect.i20.i27.i32.i32(i27, i32, i32) nounwind readnone {
entry:
  %empty = call i27 @llvm.part.select.i27(i27 %0, i32 %1, i32 %2)
  %empty_29 = trunc i27 %empty to i20
  ret i20 %empty_29
}

declare i18 @_ssdm_op_PartSelect.i18.i33.i32.i32(i33, i32, i32) nounwind readnone

define weak i16 @_ssdm_op_PartSelect.i16.i32.i32.i32(i32, i32, i32) nounwind readnone {
entry:
  %empty = call i32 @llvm.part.select.i32(i32 %0, i32 %1, i32 %2)
  %empty_30 = trunc i32 %empty to i16
  ret i16 %empty_30
}

declare i12 @_ssdm_op_PartSelect.i12.i27.i32.i32(i27, i32, i32) nounwind readnone

declare i11 @_ssdm_op_PartSelect.i11.i32.i32.i32(i32, i32, i32) nounwind readnone

declare i16 @_ssdm_op_HSub(...)

declare i16 @_ssdm_op_HMul(...)

declare i16 @_ssdm_op_HDiv(...)

declare i16 @_ssdm_op_HAdd(...)

define weak i1 @_ssdm_op_BitSelect.i1.i8.i32(i8, i32) nounwind readnone {
entry:
  %empty = trunc i32 %1 to i8
  %empty_31 = shl i8 1, %empty
  %empty_32 = and i8 %0, %empty_31
  %empty_33 = icmp ne i8 %empty_32, 0
  ret i1 %empty_33
}

define weak i1 @_ssdm_op_BitSelect.i1.i50.i32(i50, i32) nounwind readnone {
entry:
  %empty = zext i32 %1 to i50
  %empty_34 = shl i50 1, %empty
  %empty_35 = and i50 %0, %empty_34
  %empty_36 = icmp ne i50 %empty_35, 0
  ret i1 %empty_36
}

define weak i1 @_ssdm_op_BitSelect.i1.i32.i32(i32, i32) nounwind readnone {
entry:
  %empty = shl i32 1, %1
  %empty_37 = and i32 %0, %empty
  %empty_38 = icmp ne i32 %empty_37, 0
  ret i1 %empty_38
}

define weak i1 @_ssdm_op_BitSelect.i1.i30.i32(i30, i32) nounwind readnone {
entry:
  %empty = trunc i32 %1 to i30
  %empty_39 = shl i30 1, %empty
  %empty_40 = and i30 %0, %empty_39
  %empty_41 = icmp ne i30 %empty_40, 0
  ret i1 %empty_41
}

define weak i1 @_ssdm_op_BitSelect.i1.i27.i32(i27, i32) nounwind readnone {
entry:
  %empty = trunc i32 %1 to i27
  %empty_42 = shl i27 1, %empty
  %empty_43 = and i27 %0, %empty_42
  %empty_44 = icmp ne i27 %empty_43, 0
  ret i1 %empty_44
}

define weak i43 @_ssdm_op_BitConcatenate.i43.i11.i32(i11, i32) nounwind readnone {
entry:
  %empty = zext i11 %0 to i43
  %empty_45 = zext i32 %1 to i43
  %empty_46 = shl i43 %empty, 32
  %empty_47 = or i43 %empty_46, %empty_45
  ret i43 %empty_47
}

define weak i32 @_ssdm_op_BitConcatenate.i32.i16.i16(i16, i16) nounwind readnone {
entry:
  %empty = zext i16 %0 to i32
  %empty_48 = zext i16 %1 to i32
  %empty_49 = shl i32 %empty, 16
  %empty_50 = or i32 %empty_49, %empty_48
  ret i32 %empty_50
}

define weak i27 @_ssdm_op_BitConcatenate.i27.i11.i16(i11, i16) nounwind readnone {
entry:
  %empty = zext i11 %0 to i27
  %empty_51 = zext i16 %1 to i27
  %empty_52 = shl i27 %empty, 16
  %empty_53 = or i27 %empty_52, %empty_51
  ret i27 %empty_53
}

define weak i26 @_ssdm_op_BitConcatenate.i26.i20.i6(i20, i6) nounwind readnone {
entry:
  %empty = zext i20 %0 to i26
  %empty_54 = zext i6 %1 to i26
  %empty_55 = shl i26 %empty, 6
  %empty_56 = or i26 %empty_55, %empty_54
  ret i26 %empty_56
}

define weak i20 @_ssdm_op_BitConcatenate.i20.i18.i2(i18, i2) nounwind readnone {
entry:
  %empty = zext i18 %0 to i20
  %empty_57 = zext i2 %1 to i20
  %empty_58 = shl i20 %empty, 2
  %empty_59 = or i20 %empty_58, %empty_57
  ret i20 %empty_59
}

declare void @_ssdm_SpecDependence(...) nounwind

declare i8 @_autotb_FifoWrite_i8(i8*, i8)

declare i8 @_autotb_FifoRead_i8(i8*)

declare void @_GLOBAL__I_a() nounwind

define internal fastcc void @image_filter_Resize_opr_linear(i11 %p_src_rows_V_read, i11 %p_src_cols_V_read, i8* %p_src_data_stream_V, i11 %p_dst_rows_V_read, i11 %p_dst_cols_V_read, i8* %p_dst_data_stream_V) {
._crit_edge:
  %row_wr = alloca i1
  %row_rd = alloca i1
  %pre_fx = alloca i16
  %pre_fy = alloca i16
  %x = alloca i16
  %win_val_0_val_1_0 = alloca i8
  %win_val_0_val_1_0_1 = alloca i8
  %win_val_1_val_1_0 = alloca i8
  %win_val_1_val_1_0_1 = alloca i8
  %tmp = alloca i8
  %p_dst_cols_V_read_1 = call i11 @_ssdm_op_Read.ap_auto.i11(i11 %p_dst_cols_V_read)
  %p_dst_rows_V_read_1 = call i11 @_ssdm_op_Read.ap_auto.i11(i11 %p_dst_rows_V_read)
  %p_src_cols_V_read_1 = call i11 @_ssdm_op_Read.ap_auto.i11(i11 %p_src_cols_V_read)
  %p_src_rows_V_read_1 = call i11 @_ssdm_op_Read.ap_auto.i11(i11 %p_src_rows_V_read)
  call void (...)* @_ssdm_op_SpecInterface(i8* %p_src_data_stream_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i8* %p_dst_data_stream_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  %k_buf_val_val_0_0 = alloca [1921 x i8], align 1
  %k_buf_val_val_1_0 = alloca [1921 x i8], align 1
  %rbegin_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([80 x i8]* @hls_KD_KD_LineBuffer_MD_2_MC_s) nounwind
  %rend_i = call i32 (...)* @_ssdm_op_SpecRegionEnd([80 x i8]* @hls_KD_KD_LineBuffer_MD_2_MC_s, i32 %rbegin_i) nounwind
  %tmp_s = call i27 @_ssdm_op_BitConcatenate.i27.i11.i16(i11 %p_dst_rows_V_read_1, i16 0)
  %tmp_5 = call i43 @_ssdm_op_BitConcatenate.i43.i11.i32(i11 %p_src_rows_V_read_1, i32 0)
  %tmp_20_cast_cast = zext i27 %tmp_s to i43
  %tmp_6 = udiv i43 %tmp_5, %tmp_20_cast_cast
  %row_rate_V = trunc i43 %tmp_6 to i32
  %tmp_11 = trunc i43 %tmp_6 to i28
  %tmp_33 = trunc i43 %tmp_6 to i27
  %tmp_7 = call i27 @_ssdm_op_BitConcatenate.i27.i11.i16(i11 %p_dst_cols_V_read_1, i16 0)
  %tmp_8 = call i43 @_ssdm_op_BitConcatenate.i43.i11.i32(i11 %p_src_cols_V_read_1, i32 0)
  %tmp_27_cast_cast = zext i27 %tmp_7 to i43
  %tmp_9 = udiv i43 %tmp_8, %tmp_27_cast_cast
  %col_rate_V = trunc i43 %tmp_9 to i32
  %tmp_47 = trunc i43 %tmp_9 to i28
  %tmp_50 = trunc i43 %tmp_9 to i27
  %p_lshr_f1_cast = call i26 @_ssdm_op_PartSelect.i26.i43.i32.i32(i43 %tmp_6, i32 1, i32 26)
  %tmp_30_cast_cast = zext i26 %p_lshr_f1_cast to i27
  %p_Val2_7 = add i27 -32768, %tmp_30_cast_cast
  %p_Val2_8 = call i20 @_ssdm_op_PartSelect.i20.i27.i32.i32(i27 %p_Val2_7, i32 6, i32 25)
  %tmp_51 = call i1 @_ssdm_op_BitSelect.i1.i27.i32(i27 %p_Val2_7, i32 5)
  %tmp_1 = zext i1 %tmp_51 to i20
  %p_Val2_16 = add i20 %tmp_1, %p_Val2_8
  %p_lshr_f_cast = call i26 @_ssdm_op_PartSelect.i26.i43.i32.i32(i43 %tmp_9, i32 1, i32 26)
  %tmp_34_cast_cast = zext i26 %p_lshr_f_cast to i27
  %p_Val2_11 = add i27 -32768, %tmp_34_cast_cast
  %p_Val2_12 = call i20 @_ssdm_op_PartSelect.i20.i27.i32.i32(i27 %p_Val2_11, i32 6, i32 25)
  %tmp_52 = call i1 @_ssdm_op_BitSelect.i1.i27.i32(i27 %p_Val2_11, i32 5)
  %tmp_2 = zext i1 %tmp_52 to i20
  %p_Val2_17 = add i20 %tmp_2, %p_Val2_12
  %tmp_10 = icmp ugt i11 %p_src_rows_V_read_1, %p_dst_rows_V_read_1
  %tmp_12 = add i11 1, %p_dst_rows_V_read_1
  %rows = select i1 %tmp_10, i11 %p_src_rows_V_read_1, i11 %tmp_12
  %tmp_13 = icmp ugt i11 %p_src_cols_V_read_1, %p_dst_cols_V_read_1
  %tmp_14 = add i11 1, %p_dst_cols_V_read_1
  %cols = select i1 %tmp_13, i11 %p_src_cols_V_read_1, i11 %tmp_14
  %sx = add i11 -1, %p_src_cols_V_read_1
  %tmp_41_cast = zext i11 %sx to i16
  %sy = add i11 -1, %p_src_rows_V_read_1
  %tmp_42_cast = zext i11 %sy to i16
  %tmp_15 = icmp sgt i28 %tmp_11, 65536
  %tmp_16 = icmp sgt i28 %tmp_47, 65536
  %tmp_17 = call i26 @_ssdm_op_BitConcatenate.i26.i20.i6(i20 %p_Val2_16, i6 0)
  %tmp_60_cast = sext i26 %tmp_17 to i32
  %tmp_18 = call i26 @_ssdm_op_BitConcatenate.i26.i20.i6(i20 %p_Val2_17, i6 0)
  %tmp_62_cast = sext i26 %tmp_18 to i32
  store i16 0, i16* %x
  store i16 -10, i16* %pre_fy
  store i16 -10, i16* %pre_fx
  store i1 false, i1* %row_rd
  store i1 false, i1* %row_wr
  br label %.loopexit5

.loopexit5:                                       ; preds = %.preheader772, %._crit_edge
  %p_Val2_14 = phi i11 [ 0, %._crit_edge ], [ %i, %.preheader772 ]
  %i_op_assign_11_cast = zext i11 %p_Val2_14 to i12
  %exitcond1 = icmp eq i11 %p_Val2_14, %rows
  call void (...)* @_ssdm_op_SpecLoopTripCount(i64 2, i64 1081, i64 0)
  %i = add i11 %p_Val2_14, 1
  br i1 %exitcond1, label %6, label %.preheader772.preheader

.preheader772.preheader:                          ; preds = %.loopexit5
  %tmp_19 = add i12 %i_op_assign_11_cast, -1
  %tmp_45_cast = sext i12 %tmp_19 to i16
  %tmp_20 = icmp eq i11 %p_Val2_14, 0
  %row_wr_2 = icmp ne i11 %p_Val2_14, 0
  %tmp_21 = call i27 @_ssdm_op_BitConcatenate.i27.i11.i16(i11 %p_Val2_14, i16 0)
  br label %.preheader772

.preheader772:                                    ; preds = %._crit_edge786, %.preheader772.preheader
  %p_Val2_15 = phi i11 [ 0, %.preheader772.preheader ], [ %j, %._crit_edge786 ]
  %win_val_0_val_1_0_2 = load i8* %win_val_0_val_1_0
  %i_op_assign_cast = zext i11 %p_Val2_15 to i12
  %exitcond = icmp eq i11 %p_Val2_15, %cols
  call void (...)* @_ssdm_op_SpecLoopTripCount(i64 2, i64 1921, i64 0)
  %j = add i11 %p_Val2_15, 1
  br i1 %exitcond, label %.loopexit5, label %0

; <label>:0                                       ; preds = %.preheader772
  %tmp_3 = call i32 (...)* @_ssdm_op_SpecRegionBegin([13 x i8]* @p_str1823)
  call void (...)* @_ssdm_op_SpecPipeline(i32 1, i32 1, i32 1, i32 0, [1 x i8]* @p_str1806) nounwind
  br i1 %tmp_15, label %_ZNK13ap_fixed_baseILi33ELi17ELb1EL9ap_q_mode5EL9ap_o_mode3ELi0EEcvsEv.exit, label %._crit_edge776

_ZNK13ap_fixed_baseILi33ELi17ELb1EL9ap_q_mode5EL9ap_o_mode3ELi0EEcvsEv.exit: ; preds = %0
  %tmp_22 = udiv i27 %tmp_21, %tmp_33
  %tmp_53 = trunc i27 %tmp_22 to i12
  br label %._crit_edge776

._crit_edge776:                                   ; preds = %_ZNK13ap_fixed_baseILi33ELi17ELb1EL9ap_q_mode5EL9ap_o_mode3ELi0EEcvsEv.exit, %0
  %dy = phi i12 [ %tmp_53, %_ZNK13ap_fixed_baseILi33ELi17ELb1EL9ap_q_mode5EL9ap_o_mode3ELi0EEcvsEv.exit ], [ %tmp_19, %0 ]
  br i1 %tmp_16, label %_ZNK13ap_fixed_baseILi33ELi17ELb1EL9ap_q_mode5EL9ap_o_mode3ELi0EEcvsEv.exit27, label %1

_ZNK13ap_fixed_baseILi33ELi17ELb1EL9ap_q_mode5EL9ap_o_mode3ELi0EEcvsEv.exit27: ; preds = %._crit_edge776
  %tmp_23 = call i27 @_ssdm_op_BitConcatenate.i27.i11.i16(i11 %p_Val2_15, i16 0)
  %tmp_24 = udiv i27 %tmp_23, %tmp_50
  %tmp_54 = trunc i27 %tmp_24 to i12
  br label %_ifconv

; <label>:1                                       ; preds = %._crit_edge776
  %tmp_25 = add i12 %i_op_assign_cast, -1
  br label %_ifconv

_ifconv:                                          ; preds = %1, %_ZNK13ap_fixed_baseILi33ELi17ELb1EL9ap_q_mode5EL9ap_o_mode3ELi0EEcvsEv.exit27
  %dx = phi i12 [ %tmp_54, %_ZNK13ap_fixed_baseILi33ELi17ELb1EL9ap_q_mode5EL9ap_o_mode3ELi0EEcvsEv.exit27 ], [ %tmp_25, %1 ]
  %row_wr_load = load i1* %row_wr
  %row_rd_load = load i1* %row_rd
  %pre_fx_load = load i16* %pre_fx
  %pre_fy_load = load i16* %pre_fy
  %x_load = load i16* %x
  %OP1_V = sext i12 %dy to i32
  %p_Val2_s = mul i32 %row_rate_V, %OP1_V
  %p_Val2_3 = add i32 %p_Val2_s, %tmp_60_cast
  %OP1_V_3 = sext i12 %dx to i32
  %p_Val2_1 = mul i32 %col_rate_V, %OP1_V_3
  %p_Val2_2 = add i32 %p_Val2_1, %tmp_62_cast
  %ret_V = call i16 @_ssdm_op_PartSelect.i16.i32.i32.i32(i32 %p_Val2_2, i32 16, i32 31)
  %tmp_55 = call i1 @_ssdm_op_BitSelect.i1.i32.i32(i32 %p_Val2_2, i32 31)
  %tmp_56 = trunc i32 %p_Val2_2 to i16
  %tmp_26 = icmp eq i16 %tmp_56, 0
  %ret_V_1 = add i16 1, %ret_V
  %p_6 = select i1 %tmp_26, i16 %ret_V, i16 %ret_V_1
  %sx_2 = select i1 %tmp_55, i16 %p_6, i16 %ret_V
  %ret_V_2 = call i16 @_ssdm_op_PartSelect.i16.i32.i32.i32(i32 %p_Val2_3, i32 16, i32 31)
  %tmp_57 = call i1 @_ssdm_op_BitSelect.i1.i32.i32(i32 %p_Val2_3, i32 31)
  %tmp_58 = trunc i32 %p_Val2_3 to i16
  %tmp_27 = icmp eq i16 %tmp_58, 0
  %ret_V_3 = add i16 1, %ret_V_2
  %p_7 = select i1 %tmp_27, i16 %ret_V_2, i16 %ret_V_3
  %sy_3 = select i1 %tmp_57, i16 %p_7, i16 %ret_V_2
  %tmp_28 = sext i32 %p_Val2_2 to i33
  %tmp_29 = call i32 @_ssdm_op_BitConcatenate.i32.i16.i16(i16 %sx_2, i16 0)
  %tmp_67_cast = sext i32 %tmp_29 to i33
  %r_V_3 = sub nsw i33 %tmp_28, %tmp_67_cast
  %tmp_30 = icmp sgt i33 %r_V_3, 0
  %tmp_59 = trunc i33 %r_V_3 to i18
  %tmp_31 = call i20 @_ssdm_op_BitConcatenate.i20.i18.i2(i18 %tmp_59, i2 0)
  %u_V = select i1 %tmp_30, i20 %tmp_31, i20 0
  %tmp_32 = sext i32 %p_Val2_3 to i33
  %tmp_34 = call i32 @_ssdm_op_BitConcatenate.i32.i16.i16(i16 %sy_3, i16 0)
  %tmp_73_cast = sext i32 %tmp_34 to i33
  %r_V_4 = sub nsw i33 %tmp_32, %tmp_73_cast
  %tmp_35 = icmp sgt i33 %r_V_4, 0
  %tmp_60 = trunc i33 %r_V_4 to i18
  %tmp_36 = call i20 @_ssdm_op_BitConcatenate.i20.i18.i2(i18 %tmp_60, i2 0)
  %v_V_2 = select i1 %tmp_35, i20 %tmp_36, i20 0
  %u1_V = sub i20 262144, %u_V
  %v1_V = sub i20 262144, %v_V_2
  %tmp_37 = icmp sgt i16 %sx_2, %tmp_41_cast
  %p_u_V = select i1 %tmp_37, i20 0, i20 %u_V
  %pre_fx_1 = select i1 %tmp_37, i16 %tmp_41_cast, i16 %sx_2
  %tmp_38 = icmp sgt i16 %sy_3, %tmp_42_cast
  %v_V = select i1 %tmp_38, i20 0, i20 %v_V_2
  %sy_4 = select i1 %tmp_38, i16 %tmp_42_cast, i16 %sy_3
  %tmp_39 = icmp eq i11 %p_Val2_15, 0
  %row_wr_1 = icmp eq i16 %sy_4, %tmp_45_cast
  %not_1 = icmp ne i16 %sy_4, %pre_fy_load
  %pre_fy_1_sy = select i1 %tmp_20, i16 %pre_fy_load, i16 %sy_4
  %x_2 = select i1 %tmp_39, i16 0, i16 %x_load
  %sel_tmp4 = and i1 %tmp_39, %tmp_15
  %sel_tmp5 = select i1 %sel_tmp4, i16 %pre_fy_load, i16 %pre_fy_1_sy
  %pre_fy_5 = select i1 %tmp_39, i16 %sel_tmp5, i16 %pre_fy_load
  %pre_fx_2 = select i1 %tmp_39, i16 -10, i16 %pre_fx_load
  %tmp1 = or i1 %not_1, %sel_tmp4
  %sel_tmp = or i1 %tmp1, %tmp_20
  %row_rd_5 = select i1 %tmp_39, i1 %sel_tmp, i1 %row_rd_load
  %row_wr_4 = select i1 %sel_tmp4, i1 %row_wr_1, i1 %row_wr_2
  %row_wr_3 = select i1 %tmp_39, i1 %row_wr_4, i1 %row_wr_load
  %tmp_40 = add i12 -1, %i_op_assign_cast
  %tmp_85_cast = sext i12 %tmp_40 to i16
  %col_wr = icmp eq i16 %pre_fx_1, %tmp_85_cast
  %not_s = icmp ne i16 %pre_fx_1, %pre_fx_2
  %pre_fx_2_sx = select i1 %tmp_39, i16 -10, i16 %pre_fx_1
  %col_wr_1 = icmp ne i11 %p_Val2_15, 0
  %pre_fx_5 = select i1 %tmp_16, i16 %pre_fx_2, i16 %pre_fx_2_sx
  %tmp2 = or i1 %not_s, %tmp_16
  %col_rd_2 = or i1 %tmp2, %tmp_39
  %col_wr_2 = select i1 %tmp_16, i1 %col_wr, i1 %col_wr_1
  br i1 %col_rd_2, label %.preheader771.0, label %._crit_edge780.pre

.preheader771.0:                                  ; preds = %_ifconv
  %tmp_41 = sext i16 %x_2 to i64
  br i1 %row_rd_5, label %2, label %.preheader770.preheader

.preheader770.preheader:                          ; preds = %.preheader771.0
  %k_buf_val_val_0_0_addr_1 = getelementptr [1921 x i8]* %k_buf_val_val_0_0, i64 0, i64 %tmp_41
  %win_val_0_val_0_0 = load i8* %k_buf_val_val_0_0_addr_1, align 1
  %k_buf_val_val_1_0_addr_1 = getelementptr [1921 x i8]* %k_buf_val_val_1_0, i64 0, i64 %tmp_41
  %win_val_1_val_0_0_1 = load i8* %k_buf_val_val_1_0_addr_1, align 1
  store i8 %win_val_0_val_0_0, i8* %win_val_0_val_1_0
  br label %.loopexit

; <label>:2                                       ; preds = %.preheader771.0
  %k_buf_val_val_1_0_addr = getelementptr [1921 x i8]* %k_buf_val_val_1_0, i64 0, i64 %tmp_41
  %k_buf_val_val_0_0_addr = getelementptr [1921 x i8]* %k_buf_val_val_0_0, i64 0, i64 %tmp_41
  %win_val_1_val_0_0 = load i8* %k_buf_val_val_0_0_addr, align 1
  store i8 %win_val_1_val_0_0, i8* %k_buf_val_val_1_0_addr, align 1
  %tmp_42 = icmp slt i16 %sy_4, %tmp_42_cast
  %tmp_43 = icmp slt i16 %pre_fx_1, %tmp_41_cast
  %or_cond = and i1 %tmp_42, %tmp_43
  br i1 %or_cond, label %3, label %._crit_edge781

; <label>:3                                       ; preds = %2
  %tmp_4 = call i32 (...)* @_ssdm_op_SpecRegionBegin([13 x i8]* @p_str1820)
  call void (...)* @_ssdm_op_SpecProtocol(i32 0, [1 x i8]* @p_str1806) nounwind
  %tmp_67 = call i8 @_ssdm_op_Read.ap_fifo.volatile.i8P(i8* %p_src_data_stream_V)
  %empty = call i32 (...)* @_ssdm_op_SpecRegionEnd([13 x i8]* @p_str1820, i32 %tmp_4)
  store i8 %tmp_67, i8* %k_buf_val_val_0_0_addr, align 1
  store i8 %tmp_67, i8* %tmp
  store i8 %tmp_67, i8* %win_val_0_val_1_0
  br label %.loopexit

._crit_edge781:                                   ; preds = %2
  br i1 %tmp_43, label %._crit_edge783, label %4

; <label>:4                                       ; preds = %._crit_edge781
  br i1 %tmp_42, label %5, label %.critedge

; <label>:5                                       ; preds = %4
  %s_val_0_load = load i8* %tmp
  store i8 %s_val_0_load, i8* %k_buf_val_val_0_0_addr, align 1
  br label %.loopexit

._crit_edge783:                                   ; preds = %._crit_edge781
  br i1 %tmp_42, label %.loopexit, label %.critedge

.critedge:                                        ; preds = %._crit_edge783, %4
  store i8 %win_val_1_val_0_0, i8* %win_val_0_val_1_0
  br label %.loopexit

.loopexit:                                        ; preds = %.critedge, %._crit_edge783, %5, %3, %.preheader770.preheader
  %win_val_val_1_0_0_2 = phi i8 [ %win_val_1_val_0_0_1, %.preheader770.preheader ], [ %win_val_1_val_0_0, %3 ], [ %win_val_1_val_0_0, %._crit_edge783 ], [ %win_val_1_val_0_0, %.critedge ], [ %win_val_1_val_0_0, %5 ]
  %win_val_1_val_1_0_load = load i8* %win_val_1_val_1_0
  %x_1 = add i16 %x_2, 1
  store i8 %win_val_1_val_1_0_load, i8* %win_val_1_val_1_0_1
  store i8 %win_val_val_1_0_0_2, i8* %win_val_1_val_1_0
  store i8 %win_val_0_val_1_0_2, i8* %win_val_0_val_1_0_1
  store i16 %x_1, i16* %x
  br label %._crit_edge780

._crit_edge780.pre:                               ; preds = %_ifconv
  store i16 %x_2, i16* %x
  br label %._crit_edge780

._crit_edge780:                                   ; preds = %._crit_edge780.pre, %.loopexit
  %brmerge_demorgan = and i1 %row_wr_3, %col_wr_2
  br i1 %brmerge_demorgan, label %.preheader.preheader_ifconv, label %._crit_edge786

.preheader.preheader_ifconv:                      ; preds = %._crit_edge780
  %win_val_0_val_1_0_load = load i8* %win_val_0_val_1_0
  %win_val_0_val_1_0_1_load = load i8* %win_val_0_val_1_0_1
  %win_val_1_val_1_0_load_1 = load i8* %win_val_1_val_1_0
  %win_val_1_val_1_0_1_load = load i8* %win_val_1_val_1_0_1
  %OP2_V = sext i20 %u1_V to i28
  %OP2_V_3_cast = sext i20 %v1_V to i47
  %OP2_V_1 = sext i20 %v1_V to i28
  %OP1_V_4 = zext i8 %win_val_1_val_1_0_1_load to i28
  %r_V = mul i28 %OP2_V, %OP1_V_4
  %OP1_V_5_cast = sext i28 %r_V to i47
  %p_Val2_20 = mul i47 %OP1_V_5_cast, %OP2_V_3_cast
  %p_Val2_49_cast = sext i47 %p_Val2_20 to i48
  %OP1_V_6 = zext i8 %win_val_1_val_1_0_load_1 to i28
  %r_V_6 = mul i28 %OP2_V_1, %OP1_V_6
  %OP1_V_7_cast = sext i28 %r_V_6 to i47
  %OP2_V_7_cast = sext i20 %p_u_V to i47
  %p_Val2_4 = mul i47 %OP1_V_7_cast, %OP2_V_7_cast
  %p_Val2_4_cast = sext i47 %p_Val2_4 to i48
  %p_Val2_23 = add i48 %p_Val2_4_cast, %p_Val2_49_cast
  %p_Val2_50_cast = sext i48 %p_Val2_23 to i49
  %OP1_V_8 = zext i8 %win_val_0_val_1_0_1_load to i28
  %r_V_7 = mul i28 %OP2_V, %OP1_V_8
  %OP1_V_9_cast = sext i28 %r_V_7 to i47
  %OP2_V_8_cast = sext i20 %v_V to i47
  %p_Val2_5 = mul i47 %OP1_V_9_cast, %OP2_V_8_cast
  %p_Val2_5_cast = sext i47 %p_Val2_5 to i48
  %tmp_44 = zext i49 %p_Val2_50_cast to i50
  %tmp_54_cast = zext i48 %p_Val2_5_cast to i49
  %OP1_V_s = zext i8 %win_val_0_val_1_0_load to i28
  %OP2_V_9 = sext i20 %p_u_V to i28
  %r_V_8 = mul i28 %OP2_V_9, %OP1_V_s
  %OP1_V_11_cast = sext i28 %r_V_8 to i47
  %p_Val2_6 = mul i47 %OP1_V_11_cast, %OP2_V_8_cast
  %p_Val2_6_cast = sext i47 %p_Val2_6 to i48
  %tmp_1398_cast_cast = zext i48 %p_Val2_6_cast to i49
  %tmp3 = add i49 %tmp_54_cast, %tmp_1398_cast_cast
  %tmp28_cast = zext i49 %tmp3 to i50
  %p_Val2_26 = add i50 %tmp28_cast, %tmp_44
  %signbit = call i1 @_ssdm_op_BitSelect.i1.i50.i32(i50 %p_Val2_26, i32 47)
  %p_Val2_27 = call i8 @_ssdm_op_PartSelect.i8.i50.i32.i32(i50 %p_Val2_26, i32 36, i32 43)
  %tmp_63 = call i1 @_ssdm_op_BitSelect.i1.i50.i32(i50 %p_Val2_26, i32 35)
  %tmp_1_i_i = zext i1 %tmp_63 to i8
  %tmp_64 = call i1 @_ssdm_op_BitSelect.i1.i50.i32(i50 %p_Val2_26, i32 43)
  %p_Val2_28 = add i8 %p_Val2_27, %tmp_1_i_i
  %tmp_65 = call i1 @_ssdm_op_BitSelect.i1.i8.i32(i8 %p_Val2_28, i32 7)
  %tmp_2_i_i = xor i1 %tmp_65, true
  %carry = and i1 %tmp_64, %tmp_2_i_i
  %p_Result_4_i_i = call i4 @_ssdm_op_PartSelect.i4.i50.i32.i32(i50 %p_Val2_26, i32 44, i32 47)
  %Range1_all_ones = icmp eq i4 %p_Result_4_i_i, -1
  %Range1_all_zeros = icmp eq i4 %p_Result_4_i_i, 0
  %deleted_zeros = select i1 %carry, i1 %Range1_all_ones, i1 %Range1_all_zeros
  %p_38_i_i_i = and i1 %carry, %Range1_all_ones
  %tmp_3_i_i = xor i1 %p_38_i_i_i, true
  %neg_src = and i1 %signbit, %tmp_3_i_i
  %p_39_demorgan_i_i_i = or i1 %deleted_zeros, %signbit
  %signbit_not = xor i1 %signbit, true
  %neg_src_not_i_i = or i1 %p_38_i_i_i, %signbit_not
  %brmerge_i_i_not_i_i = and i1 %p_39_demorgan_i_i_i, %neg_src_not_i_i
  %p_39_demorgan_i_not_i_i = xor i1 %p_39_demorgan_i_i_i, true
  %brmerge_i_i = or i1 %neg_src_not_i_i, %p_39_demorgan_i_not_i_i
  %p_mux_i_i = select i1 %brmerge_i_i_not_i_i, i8 %p_Val2_28, i8 -1
  %p_i_i = select i1 %neg_src, i8 0, i8 %p_Val2_28
  %p_Val2_s_60 = select i1 %brmerge_i_i, i8 %p_mux_i_i, i8 %p_i_i
  %tmp_45 = call i32 (...)* @_ssdm_op_SpecRegionBegin([13 x i8]* @p_str1824)
  call void (...)* @_ssdm_op_SpecProtocol(i32 0, [1 x i8]* @p_str1806) nounwind
  call void @_ssdm_op_Write.ap_fifo.volatile.i8P(i8* %p_dst_data_stream_V, i8 %p_Val2_s_60)
  %empty_61 = call i32 (...)* @_ssdm_op_SpecRegionEnd([13 x i8]* @p_str1824, i32 %tmp_45)
  br label %._crit_edge786

._crit_edge786:                                   ; preds = %.preheader.preheader_ifconv, %._crit_edge780
  %empty_62 = call i32 (...)* @_ssdm_op_SpecRegionEnd([13 x i8]* @p_str1823, i32 %tmp_3)
  store i16 %pre_fy_5, i16* %pre_fy
  store i16 %pre_fx_5, i16* %pre_fx
  store i1 %row_rd_5, i1* %row_rd
  store i1 %row_wr_3, i1* %row_wr
  br label %.preheader772

; <label>:6                                       ; preds = %.loopexit5
  ret void
}

define internal fastcc void @image_filter_Resize(i11 %rows, i11 %cols, i8* %p_src_data_stream_V, i8* %p_dst_data_stream_V) {
entry:
  call void (...)* @_ssdm_op_SpecIFCore(i11 %cols, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecIFCore(i11 %rows, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecInterface(i8* %p_dst_data_stream_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i8* %p_src_data_stream_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  %cols_read = call i11 @_ssdm_op_Read.ap_stable.i11(i11 %cols)
  %rows_read = call i11 @_ssdm_op_Read.ap_stable.i11(i11 %rows)
  call void (...)* @_ssdm_op_SpecInterface(i11 %cols, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %rows, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %cols, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %rows, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %cols, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %rows, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %cols, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %rows, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call fastcc void @image_filter_Resize_opr_linear(i11 %rows_read, i11 %cols_read, i8* %p_src_data_stream_V, i11 %rows_read, i11 %cols_read, i8* %p_dst_data_stream_V)
  ret void
}

define internal fastcc void @"image_filter_Mat2AXIvideo<8, 1080, 1920, 0>"(i11 %rows, i11 %cols, i8* %img_data_stream_V, i8* %AXI_video_strm_V_data_V, i1* %AXI_video_strm_V_keep_V, i1* %AXI_video_strm_V_strb_V, i1* %AXI_video_strm_V_user_V, i1* %AXI_video_strm_V_last_V, i1* %AXI_video_strm_V_id_V, i1* %AXI_video_strm_V_dest_V) {
entry:
  %tmp_user_V = alloca i1
  call void (...)* @_ssdm_op_SpecIFCore(i11 %cols, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecIFCore(i11 %rows, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecInterface(i8* %img_data_stream_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  %cols_read = call i11 @_ssdm_op_Read.ap_stable.i11(i11 %cols)
  %rows_read = call i11 @_ssdm_op_Read.ap_stable.i11(i11 %rows)
  call void (...)* @_ssdm_op_SpecInterface(i11 %cols, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %rows, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i8* %AXI_video_strm_V_data_V, i1* %AXI_video_strm_V_keep_V, i1* %AXI_video_strm_V_strb_V, i1* %AXI_video_strm_V_user_V, i1* %AXI_video_strm_V_last_V, i1* %AXI_video_strm_V_id_V, i1* %AXI_video_strm_V_dest_V, [5 x i8]* @p_str1805, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806) nounwind
  %r_V = add i11 %cols_read, -1
  store i1 true, i1* %tmp_user_V
  br label %0

; <label>:0                                       ; preds = %3, %entry
  %p_i = phi i11 [ 0, %entry ], [ %i_V, %3 ]
  %exitcond7_i = icmp eq i11 %p_i, %rows_read
  call void (...)* @_ssdm_op_SpecLoopTripCount(i64 0, i64 1080, i64 0)
  %i_V = add i11 %p_i, 1
  br i1 %exitcond7_i, label %"Mat2AXIvideo<8, 1080, 1920, 0>.exit", label %1

; <label>:1                                       ; preds = %0
  call void (...)* @_ssdm_op_SpecLoopName([12 x i8]* @p_str1815) nounwind
  %tmp_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([12 x i8]* @p_str1815)
  br label %2

; <label>:2                                       ; preds = %.critedge.i, %1
  %p_1_i = phi i11 [ 0, %1 ], [ %j_V, %.critedge.i ]
  %exitcond_i = icmp eq i11 %p_1_i, %cols_read
  call void (...)* @_ssdm_op_SpecLoopTripCount(i64 0, i64 1920, i64 0)
  %j_V = add i11 %p_1_i, 1
  br i1 %exitcond_i, label %3, label %.critedge.i

.critedge.i:                                      ; preds = %2
  %tmp_user_V_load = load i1* %tmp_user_V
  call void (...)* @_ssdm_op_SpecLoopName([11 x i8]* @p_str1816) nounwind
  %tmp_98_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([11 x i8]* @p_str1816)
  call void (...)* @_ssdm_op_SpecPipeline(i32 1, i32 1, i32 1, i32 0, [1 x i8]* @p_str1806) nounwind
  %axi_last_V = icmp eq i11 %p_1_i, %r_V
  %tmp_99_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([13 x i8]* @p_str1820)
  call void (...)* @_ssdm_op_SpecProtocol(i32 0, [1 x i8]* @p_str1806) nounwind
  %tmp = call i8 @_ssdm_op_Read.ap_fifo.volatile.i8P(i8* %img_data_stream_V)
  %empty = call i32 (...)* @_ssdm_op_SpecRegionEnd([13 x i8]* @p_str1820, i32 %tmp_99_i)
  call void (...)* @_ssdm_op_SpecLoopName([14 x i8]* @p_str1817) nounwind
  call void @_ssdm_op_Write.axis.volatile.i8P.i1P.i1P.i1P.i1P.i1P.i1P(i8* %AXI_video_strm_V_data_V, i1* %AXI_video_strm_V_keep_V, i1* %AXI_video_strm_V_strb_V, i1* %AXI_video_strm_V_user_V, i1* %AXI_video_strm_V_last_V, i1* %AXI_video_strm_V_id_V, i1* %AXI_video_strm_V_dest_V, i8 %tmp, i1 true, i1 undef, i1 %tmp_user_V_load, i1 %axi_last_V, i1 undef, i1 undef)
  %empty_63 = call i32 (...)* @_ssdm_op_SpecRegionEnd([11 x i8]* @p_str1816, i32 %tmp_98_i)
  store i1 false, i1* %tmp_user_V
  br label %2

; <label>:3                                       ; preds = %2
  %empty_64 = call i32 (...)* @_ssdm_op_SpecRegionEnd([12 x i8]* @p_str1815, i32 %tmp_i)
  br label %0

"Mat2AXIvideo<8, 1080, 1920, 0>.exit":            ; preds = %0
  ret void
}

define internal fastcc void @image_filter_CvtColor(i11 %rows, i11 %cols, i8* %p_src_data_stream_0_V, i8* %p_src_data_stream_1_V, i8* %p_src_data_stream_2_V, i8* %p_dst_data_stream_V) {
entry:
  call void (...)* @_ssdm_op_SpecIFCore(i11 %cols, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecIFCore(i11 %rows, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecInterface(i8* %p_dst_data_stream_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i8* %p_src_data_stream_2_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i8* %p_src_data_stream_1_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i8* %p_src_data_stream_0_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  %cols_read = call i11 @_ssdm_op_Read.ap_stable.i11(i11 %cols)
  %rows_read = call i11 @_ssdm_op_Read.ap_stable.i11(i11 %rows)
  call void (...)* @_ssdm_op_SpecInterface(i11 %cols, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %rows, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  br label %0

; <label>:0                                       ; preds = %3, %entry
  %i_i = phi i11 [ 0, %entry ], [ %i, %3 ]
  %exitcond2_i = icmp eq i11 %i_i, %rows_read
  call void (...)* @_ssdm_op_SpecLoopTripCount(i64 0, i64 1080, i64 0)
  %i = add i11 %i_i, 1
  br i1 %exitcond2_i, label %"CvtColor<HLS_RGB2GRAY, 32, 0, 1080, 1920>.exit", label %1

; <label>:1                                       ; preds = %0
  call void (...)* @_ssdm_op_SpecLoopName([12 x i8]* @p_str1815) nounwind
  %tmp_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([12 x i8]* @p_str1815)
  br label %2

; <label>:2                                       ; preds = %"operator>>.exit.i_ifconv", %1
  %j_i = phi i11 [ 0, %1 ], [ %j, %"operator>>.exit.i_ifconv" ]
  %exitcond_i = icmp eq i11 %j_i, %cols_read
  call void (...)* @_ssdm_op_SpecLoopTripCount(i64 0, i64 1920, i64 0)
  %j = add i11 %j_i, 1
  br i1 %exitcond_i, label %3, label %"operator>>.exit.i_ifconv"

"operator>>.exit.i_ifconv":                       ; preds = %2
  call void (...)* @_ssdm_op_SpecLoopName([11 x i8]* @p_str1816) nounwind
  %tmp_102_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([11 x i8]* @p_str1816)
  call void (...)* @_ssdm_op_SpecPipeline(i32 1, i32 1, i32 1, i32 0, [1 x i8]* @p_str1806) nounwind
  %tmp_103_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([13 x i8]* @p_str1837)
  call void (...)* @_ssdm_op_SpecProtocol(i32 0, [1 x i8]* @p_str1806) nounwind
  %tmp_71 = call i8 @_ssdm_op_Read.ap_fifo.volatile.i8P(i8* %p_src_data_stream_0_V)
  %tmp_72 = call i8 @_ssdm_op_Read.ap_fifo.volatile.i8P(i8* %p_src_data_stream_1_V)
  %tmp_73 = call i8 @_ssdm_op_Read.ap_fifo.volatile.i8P(i8* %p_src_data_stream_2_V)
  %empty = call i32 (...)* @_ssdm_op_SpecRegionEnd([13 x i8]* @p_str1837, i32 %tmp_103_i)
  %OP2_V_i_cast_i = zext i8 %tmp_71 to i29
  %r_V_i_i = mul i29 %OP2_V_i_cast_i, 1254096
  %OP2_V_1_i_cast_i = zext i8 %tmp_72 to i30
  %r_V_3_i_i = mul i30 %OP2_V_1_i_cast_i, 2462056
  %OP2_V_2_i_cast_i = zext i8 %tmp_73 to i28
  %r_V = mul i28 %OP2_V_2_i_cast_i, 478150
  %tmp_2_i_cast_i = zext i28 %r_V to i29
  %p_Val2_30 = add i29 %r_V_i_i, %tmp_2_i_cast_i
  %tmp_i_cast_i = zext i29 %p_Val2_30 to i30
  %r_V_5 = add i30 %r_V_3_i_i, %tmp_i_cast_i
  %p_Val2_32 = call i8 @_ssdm_op_PartSelect.i8.i30.i32.i32(i30 %r_V_5, i32 22, i32 29)
  %tmp = call i1 @_ssdm_op_BitSelect.i1.i30.i32(i30 %r_V_5, i32 21)
  %tmp_4_i_i_i_i = zext i1 %tmp to i8
  %tmp_68 = call i1 @_ssdm_op_BitSelect.i1.i30.i32(i30 %r_V_5, i32 29)
  %p_Val2_33 = add i8 %p_Val2_32, %tmp_4_i_i_i_i
  %tmp_69 = call i1 @_ssdm_op_BitSelect.i1.i8.i32(i8 %p_Val2_33, i32 7)
  %p_Result_9_i_i_i_i_not = xor i1 %tmp_68, true
  %not_carry = or i1 %tmp_69, %p_Result_9_i_i_i_i_not
  %p_Val2_s = select i1 %not_carry, i8 %p_Val2_33, i8 -1
  %tmp_107_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([13 x i8]* @p_str1824)
  call void (...)* @_ssdm_op_SpecProtocol(i32 0, [1 x i8]* @p_str1806) nounwind
  call void @_ssdm_op_Write.ap_fifo.volatile.i8P(i8* %p_dst_data_stream_V, i8 %p_Val2_s)
  %empty_65 = call i32 (...)* @_ssdm_op_SpecRegionEnd([13 x i8]* @p_str1824, i32 %tmp_107_i)
  %empty_66 = call i32 (...)* @_ssdm_op_SpecRegionEnd([11 x i8]* @p_str1816, i32 %tmp_102_i)
  br label %2

; <label>:3                                       ; preds = %2
  %empty_67 = call i32 (...)* @_ssdm_op_SpecRegionEnd([12 x i8]* @p_str1815, i32 %tmp_i)
  br label %0

"CvtColor<HLS_RGB2GRAY, 32, 0, 1080, 1920>.exit": ; preds = %0
  ret void
}

define internal fastcc void @image_filter_AXIvideo2Mat(i32* %AXI_video_strm_V_data_V, i4* %AXI_video_strm_V_keep_V, i4* %AXI_video_strm_V_strb_V, i1* %AXI_video_strm_V_user_V, i1* %AXI_video_strm_V_last_V, i1* %AXI_video_strm_V_id_V, i1* %AXI_video_strm_V_dest_V, i11 %rows, i11 %cols, i8* %img_data_stream_0_V, i8* %img_data_stream_1_V, i8* %img_data_stream_2_V) {
entry:
  call void (...)* @_ssdm_op_SpecIFCore(i11 %cols, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecIFCore(i11 %rows, [1 x i8]* @p_str1806, [10 x i8]* @p_str1807, [1 x i8]* @p_str1806, i32 -1, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [24 x i8]* @p_str1808)
  call void (...)* @_ssdm_op_SpecInterface(i8* %img_data_stream_2_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i8* %img_data_stream_1_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i8* %img_data_stream_0_V, [8 x i8]* @ap_fifo_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  %cols_read = call i11 @_ssdm_op_Read.ap_stable.i11(i11 %cols)
  %rows_read = call i11 @_ssdm_op_Read.ap_stable.i11(i11 %rows)
  call void (...)* @_ssdm_op_SpecInterface(i11 %cols, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i11 %rows, [10 x i8]* @ap_stable_str, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str, [1 x i8]* @p_str)
  call void (...)* @_ssdm_op_SpecInterface(i32* %AXI_video_strm_V_data_V, i4* %AXI_video_strm_V_keep_V, i4* %AXI_video_strm_V_strb_V, i1* %AXI_video_strm_V_user_V, i1* %AXI_video_strm_V_last_V, i1* %AXI_video_strm_V_id_V, i1* %AXI_video_strm_V_dest_V, [5 x i8]* @p_str1805, i32 0, i32 0, i32 0, i32 0, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806, [1 x i8]* @p_str1806) nounwind
  br label %._crit_edge188.i

._crit_edge188.i:                                 ; preds = %._crit_edge188.i, %entry
  call void (...)* @_ssdm_op_SpecLoopName([20 x i8]* @p_str1839) nounwind
  %tmp_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([20 x i8]* @p_str1839)
  call void (...)* @_ssdm_op_SpecPipeline(i32 1, i32 1, i32 1, i32 0, [1 x i8]* @p_str1806) nounwind
  call void (...)* @_ssdm_op_SpecLoopTripCount(i32 0, i32 0, i32 0, [1 x i8]* @p_str1806) nounwind
  %empty = call { i32, i4, i4, i1, i1, i1, i1 } @_ssdm_op_Read.axis.volatile.i32P.i4P.i4P.i1P.i1P.i1P.i1P(i32* %AXI_video_strm_V_data_V, i4* %AXI_video_strm_V_keep_V, i4* %AXI_video_strm_V_strb_V, i1* %AXI_video_strm_V_user_V, i1* %AXI_video_strm_V_last_V, i1* %AXI_video_strm_V_id_V, i1* %AXI_video_strm_V_dest_V)
  %tmp_data_V = extractvalue { i32, i4, i4, i1, i1, i1, i1 } %empty, 0
  %tmp_user_V = extractvalue { i32, i4, i4, i1, i1, i1, i1 } %empty, 3
  %tmp_last_V = extractvalue { i32, i4, i4, i1, i1, i1, i1 } %empty, 4
  %empty_68 = call i32 (...)* @_ssdm_op_SpecRegionEnd([20 x i8]* @p_str1839, i32 %tmp_i)
  br i1 %tmp_user_V, label %.preheader187.i.preheader, label %._crit_edge188.i

.preheader187.i.preheader:                        ; preds = %._crit_edge188.i
  %sof_1_i = alloca i1
  store i1 true, i1* %sof_1_i
  br label %.preheader187.i

.preheader187.i:                                  ; preds = %.preheader187.i.preheader, %5
  %axi_last_V1_i = phi i1 [ %axi_last_V_3_i, %5 ], [ %tmp_last_V, %.preheader187.i.preheader ]
  %axi_data_V1_i = phi i32 [ %axi_data_V_3_i, %5 ], [ %tmp_data_V, %.preheader187.i.preheader ]
  %p_i = phi i11 [ %i_V, %5 ], [ 0, %.preheader187.i.preheader ]
  %exitcond8_i = icmp eq i11 %p_i, %rows_read
  call void (...)* @_ssdm_op_SpecLoopTripCount(i64 0, i64 1080, i64 0)
  %i_V = add i11 %p_i, 1
  br i1 %exitcond8_i, label %"AXIvideo2Mat<32, 1080, 1920, 32>.exit", label %0

; <label>:0                                       ; preds = %.preheader187.i
  call void (...)* @_ssdm_op_SpecLoopName([12 x i8]* @p_str1815) nounwind
  %tmp_110_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([12 x i8]* @p_str1815)
  br label %1

; <label>:1                                       ; preds = %._crit_edge189.i, %0
  %eol = phi i1 [ %axi_last_V1_i, %0 ], [ %axi_last_V_2_i, %._crit_edge189.i ]
  %axi_data_V_1_i = phi i32 [ %axi_data_V1_i, %0 ], [ %p_Val2_s, %._crit_edge189.i ]
  %p_2_i = phi i11 [ 0, %0 ], [ %j_V, %._crit_edge189.i ]
  %eol_i = phi i1 [ false, %0 ], [ %axi_last_V_2_i, %._crit_edge189.i ]
  %exitcond9_i = icmp eq i11 %p_2_i, %cols_read
  call void (...)* @_ssdm_op_SpecLoopTripCount(i64 0, i64 1920, i64 0)
  %j_V = add i11 %p_2_i, 1
  br i1 %exitcond9_i, label %.preheader.i, label %2

; <label>:2                                       ; preds = %1
  %sof_1_i_load = load i1* %sof_1_i
  call void (...)* @_ssdm_op_SpecLoopName([11 x i8]* @p_str1816) nounwind
  %tmp_111_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([11 x i8]* @p_str1816)
  call void (...)* @_ssdm_op_SpecPipeline(i32 1, i32 1, i32 1, i32 0, [1 x i8]* @p_str1806) nounwind
  %brmerge_i = or i1 %sof_1_i_load, %eol_i
  br i1 %brmerge_i, label %._crit_edge189.i, label %3

; <label>:3                                       ; preds = %2
  %empty_69 = call { i32, i4, i4, i1, i1, i1, i1 } @_ssdm_op_Read.axis.volatile.i32P.i4P.i4P.i1P.i1P.i1P.i1P(i32* %AXI_video_strm_V_data_V, i4* %AXI_video_strm_V_keep_V, i4* %AXI_video_strm_V_strb_V, i1* %AXI_video_strm_V_user_V, i1* %AXI_video_strm_V_last_V, i1* %AXI_video_strm_V_id_V, i1* %AXI_video_strm_V_dest_V)
  %tmp_data_V_1 = extractvalue { i32, i4, i4, i1, i1, i1, i1 } %empty_69, 0
  %tmp_last_V_1 = extractvalue { i32, i4, i4, i1, i1, i1, i1 } %empty_69, 4
  br label %._crit_edge189.i

._crit_edge189.i:                                 ; preds = %3, %2
  %axi_last_V_2_i = phi i1 [ %tmp_last_V_1, %3 ], [ %eol, %2 ]
  %p_Val2_s = phi i32 [ %tmp_data_V_1, %3 ], [ %axi_data_V_1_i, %2 ]
  %tmp = trunc i32 %p_Val2_s to i8
  %tmp_10 = call i8 @_ssdm_op_PartSelect.i8.i32.i32.i32(i32 %p_Val2_s, i32 8, i32 15)
  %tmp_11 = call i8 @_ssdm_op_PartSelect.i8.i32.i32.i32(i32 %p_Val2_s, i32 16, i32 23)
  %tmp_116_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([13 x i8]* @p_str1841)
  call void (...)* @_ssdm_op_SpecProtocol(i32 0, [1 x i8]* @p_str1806) nounwind
  call void @_ssdm_op_Write.ap_fifo.volatile.i8P(i8* %img_data_stream_0_V, i8 %tmp)
  call void @_ssdm_op_Write.ap_fifo.volatile.i8P(i8* %img_data_stream_1_V, i8 %tmp_10)
  call void @_ssdm_op_Write.ap_fifo.volatile.i8P(i8* %img_data_stream_2_V, i8 %tmp_11)
  %empty_70 = call i32 (...)* @_ssdm_op_SpecRegionEnd([13 x i8]* @p_str1841, i32 %tmp_116_i)
  %empty_71 = call i32 (...)* @_ssdm_op_SpecRegionEnd([11 x i8]* @p_str1816, i32 %tmp_111_i)
  store i1 false, i1* %sof_1_i
  br label %1

.preheader.i:                                     ; preds = %1, %4
  %axi_last_V_3_i = phi i1 [ %tmp_last_V_2, %4 ], [ %eol, %1 ]
  %axi_data_V_3_i = phi i32 [ %tmp_data_V_2, %4 ], [ %axi_data_V_1_i, %1 ]
  %eol_2_i = phi i1 [ %tmp_last_V_2, %4 ], [ %eol_i, %1 ]
  br i1 %eol_2_i, label %5, label %4

; <label>:4                                       ; preds = %.preheader.i
  call void (...)* @_ssdm_op_SpecLoopName([18 x i8]* @p_str1840) nounwind
  %tmp_112_i = call i32 (...)* @_ssdm_op_SpecRegionBegin([18 x i8]* @p_str1840)
  call void (...)* @_ssdm_op_SpecPipeline(i32 1, i32 1, i32 1, i32 0, [1 x i8]* @p_str1806) nounwind
  call void (...)* @_ssdm_op_SpecLoopTripCount(i32 0, i32 0, i32 0, [1 x i8]* @p_str1806) nounwind
  %empty_72 = call { i32, i4, i4, i1, i1, i1, i1 } @_ssdm_op_Read.axis.volatile.i32P.i4P.i4P.i1P.i1P.i1P.i1P(i32* %AXI_video_strm_V_data_V, i4* %AXI_video_strm_V_keep_V, i4* %AXI_video_strm_V_strb_V, i1* %AXI_video_strm_V_user_V, i1* %AXI_video_strm_V_last_V, i1* %AXI_video_strm_V_id_V, i1* %AXI_video_strm_V_dest_V)
  %tmp_data_V_2 = extractvalue { i32, i4, i4, i1, i1, i1, i1 } %empty_72, 0
  %tmp_last_V_2 = extractvalue { i32, i4, i4, i1, i1, i1, i1 } %empty_72, 4
  %empty_73 = call i32 (...)* @_ssdm_op_SpecRegionEnd([18 x i8]* @p_str1840, i32 %tmp_112_i)
  br label %.preheader.i

; <label>:5                                       ; preds = %.preheader.i
  %empty_74 = call i32 (...)* @_ssdm_op_SpecRegionEnd([12 x i8]* @p_str1815, i32 %tmp_110_i)
  br label %.preheader187.i

"AXIvideo2Mat<32, 1080, 1920, 32>.exit":          ; preds = %.preheader187.i
  ret void
}

!hls.encrypted.func = !{}
!llvm.map.gv = !{!0}

!0 = metadata !{metadata !1, [1 x i32]* @llvm_global_ctors_0}
!1 = metadata !{metadata !2}
!2 = metadata !{i32 0, i32 31, metadata !3}
!3 = metadata !{metadata !4}
!4 = metadata !{metadata !"llvm.global_ctors.0", metadata !5, metadata !"", i32 0, i32 31}
!5 = metadata !{metadata !6}
!6 = metadata !{i32 0, i32 0, i32 1}
!7 = metadata !{metadata !8}
!8 = metadata !{i32 0, i32 31, metadata !9}
!9 = metadata !{metadata !10}
!10 = metadata !{metadata !"INPUT_STREAM.V.data.V", metadata !5, metadata !"uint32", i32 0, i32 31}
!11 = metadata !{metadata !12}
!12 = metadata !{i32 0, i32 3, metadata !13}
!13 = metadata !{metadata !14}
!14 = metadata !{metadata !"INPUT_STREAM.V.keep.V", metadata !5, metadata !"uint4", i32 0, i32 3}
!15 = metadata !{metadata !16}
!16 = metadata !{i32 0, i32 3, metadata !17}
!17 = metadata !{metadata !18}
!18 = metadata !{metadata !"INPUT_STREAM.V.strb.V", metadata !5, metadata !"uint4", i32 0, i32 3}
!19 = metadata !{metadata !20}
!20 = metadata !{i32 0, i32 0, metadata !21}
!21 = metadata !{metadata !22}
!22 = metadata !{metadata !"INPUT_STREAM.V.user.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!23 = metadata !{metadata !24}
!24 = metadata !{i32 0, i32 0, metadata !25}
!25 = metadata !{metadata !26}
!26 = metadata !{metadata !"INPUT_STREAM.V.last.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!27 = metadata !{metadata !28}
!28 = metadata !{i32 0, i32 0, metadata !29}
!29 = metadata !{metadata !30}
!30 = metadata !{metadata !"INPUT_STREAM.V.id.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!31 = metadata !{metadata !32}
!32 = metadata !{i32 0, i32 0, metadata !33}
!33 = metadata !{metadata !34}
!34 = metadata !{metadata !"INPUT_STREAM.V.dest.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!35 = metadata !{metadata !36}
!36 = metadata !{i32 0, i32 7, metadata !37}
!37 = metadata !{metadata !38}
!38 = metadata !{metadata !"OUTPUT_STREAM.V.data.V", metadata !5, metadata !"uint8", i32 0, i32 7}
!39 = metadata !{metadata !40}
!40 = metadata !{i32 0, i32 0, metadata !41}
!41 = metadata !{metadata !42}
!42 = metadata !{metadata !"OUTPUT_STREAM.V.keep.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!43 = metadata !{metadata !44}
!44 = metadata !{i32 0, i32 0, metadata !45}
!45 = metadata !{metadata !46}
!46 = metadata !{metadata !"OUTPUT_STREAM.V.strb.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!47 = metadata !{metadata !48}
!48 = metadata !{i32 0, i32 0, metadata !49}
!49 = metadata !{metadata !50}
!50 = metadata !{metadata !"OUTPUT_STREAM.V.user.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!51 = metadata !{metadata !52}
!52 = metadata !{i32 0, i32 0, metadata !53}
!53 = metadata !{metadata !54}
!54 = metadata !{metadata !"OUTPUT_STREAM.V.last.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!55 = metadata !{metadata !56}
!56 = metadata !{i32 0, i32 0, metadata !57}
!57 = metadata !{metadata !58}
!58 = metadata !{metadata !"OUTPUT_STREAM.V.id.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!59 = metadata !{metadata !60}
!60 = metadata !{i32 0, i32 0, metadata !61}
!61 = metadata !{metadata !62}
!62 = metadata !{metadata !"OUTPUT_STREAM.V.dest.V", metadata !5, metadata !"uint1", i32 0, i32 0}
!63 = metadata !{metadata !64}
!64 = metadata !{i32 0, i32 31, metadata !65}
!65 = metadata !{metadata !66}
!66 = metadata !{metadata !"rows", metadata !67, metadata !"int", i32 0, i32 31}
!67 = metadata !{metadata !68}
!68 = metadata !{i32 0, i32 0, i32 0}
!69 = metadata !{metadata !70}
!70 = metadata !{i32 0, i32 31, metadata !71}
!71 = metadata !{metadata !72}
!72 = metadata !{metadata !"cols", metadata !67, metadata !"int", i32 0, i32 31}
