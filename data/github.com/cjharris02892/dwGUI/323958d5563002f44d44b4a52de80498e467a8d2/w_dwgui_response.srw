HA$PBExportHeader$w_dwgui_response.srw
forward
global type w_dwgui_response from w_dwgui
end type
end forward

global type w_dwgui_response from w_dwgui
integer width = 2747
integer height = 1624
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
end type
global w_dwgui_response w_dwgui_response

on w_dwgui_response.create
int iCurrent
call super::create
end on

on w_dwgui_response.destroy
call super::destroy
end on

type dw_options from w_dwgui`dw_options within w_dwgui_response
end type

type uo_toolbar from w_dwgui`uo_toolbar within w_dwgui_response
end type

