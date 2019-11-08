$PBExportHeader$uo_23.sru
forward
global type uo_23 from uo_3
end type
end forward

global type uo_23 from uo_3
end type
global uo_23 uo_23

on uo_23.create
call super::create
end on

on uo_23.destroy
call super::destroy
end on

type cb_4 from uo_3`cb_4 within uo_23
end type

type ole_1 from uo_3`ole_1 within uo_23
end type

type ip_1 from uo_3`ip_1 within uo_23
end type

type ie_1 from uo_3`ie_1 within uo_23
end type

type am_1 from uo_3`am_1 within uo_23
end type

type dp_1 from uo_3`dp_1 within uo_23
end type

type mc_1 from uo_3`mc_1 within uo_23
end type

type gr_1 from uo_3`gr_1 within uo_23
end type

type dw_1 from uo_3`dw_1 within uo_23
end type

type tab_1 from uo_3`tab_1 within uo_23
end type

on tab_1.create
call super::create
this.Control[]={this.tabpage_1,&
this.tabpage_2}
end on

on tab_1.destroy
call super::destroy
end on

type tabpage_1 from uo_3`tabpage_1 within tab_1
end type

type cb_2 from uo_3`cb_2 within tabpage_1
integer x = 69
integer y = 96
string text = "a"
end type

event cb_2::clicked;call super::clicked;MessageBox ( "Tip", "This is a" )
end event

type tabpage_2 from uo_3`tabpage_2 within tab_1
end type

type cb_3 from uo_3`cb_3 within tabpage_2
integer x = 128
integer y = 108
string text = "b"
end type

event cb_3::clicked;call super::clicked;MessageBox ( "Tip", "This is b" )
end event

type tv_1 from uo_3`tv_1 within uo_23
end type

type lv_1 from uo_3`lv_1 within uo_23
end type

type plb_1 from uo_3`plb_1 within uo_23
end type

type lb_1 from uo_3`lb_1 within uo_23
end type

type ddplb_1 from uo_3`ddplb_1 within uo_23
end type

type ddlb_1 from uo_3`ddlb_1 within uo_23
end type

type vpb_1 from uo_3`vpb_1 within uo_23
end type

type hpb_1 from uo_3`hpb_1 within uo_23
end type

type vtb_1 from uo_3`vtb_1 within uo_23
end type

type htb_1 from uo_3`htb_1 within uo_23
end type

type vsb_1 from uo_3`vsb_1 within uo_23
end type

type hsb_1 from uo_3`hsb_1 within uo_23
end type

type rte_1 from uo_3`rte_1 within uo_23
end type

type mle_1 from uo_3`mle_1 within uo_23
end type

type em_1 from uo_3`em_1 within uo_23
end type

type sle_1 from uo_3`sle_1 within uo_23
end type

type phl_1 from uo_3`phl_1 within uo_23
end type

type p_1 from uo_3`p_1 within uo_23
end type

type shl_1 from uo_3`shl_1 within uo_23
end type

type st_1 from uo_3`st_1 within uo_23
end type

type rb_1 from uo_3`rb_1 within uo_23
end type

type cbx_1 from uo_3`cbx_1 within uo_23
end type

type pb_1 from uo_3`pb_1 within uo_23
end type

type cb_1 from uo_3`cb_1 within uo_23
end type

type gb_1 from uo_3`gb_1 within uo_23
end type

type ln_1 from uo_3`ln_1 within uo_23
end type

type ov_1 from uo_3`ov_1 within uo_23
end type

type r_1 from uo_3`r_1 within uo_23
end type

type rr_1 from uo_3`rr_1 within uo_23
end type

