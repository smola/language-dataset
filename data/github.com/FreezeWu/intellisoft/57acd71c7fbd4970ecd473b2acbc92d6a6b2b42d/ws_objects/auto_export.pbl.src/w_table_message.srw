$PBExportHeader$w_table_message.srw
forward
global type w_table_message from window
end type
type st_1 from statictext within w_table_message
end type
type st_2 from statictext within w_table_message
end type
type cb_rep from commandbutton within w_table_message
end type
type cb_add from commandbutton within w_table_message
end type
type cb_1 from commandbutton within w_table_message
end type
end forward

global type w_table_message from window
integer x = 1056
integer y = 484
integer width = 2107
integer height = 672
boolean titlebar = true
string title = "Untitled"
boolean controlmenu = true
windowtype windowtype = response!
long backcolor = 33551856
st_1 st_1
st_2 st_2
cb_rep cb_rep
cb_add cb_add
cb_1 cb_1
end type
global w_table_message w_table_message

type variables
integer ii_mess
end variables

on w_table_message.create
this.st_1=create st_1
this.st_2=create st_2
this.cb_rep=create cb_rep
this.cb_add=create cb_add
this.cb_1=create cb_1
this.Control[]={this.st_1,&
this.st_2,&
this.cb_rep,&
this.cb_add,&
this.cb_1}
end on

on w_table_message.destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.cb_rep)
destroy(this.cb_add)
destroy(this.cb_1)
end on

event open;ii_mess = 0
string s

s = message.stringparm

st_1.text = "The " + s + " table exists."


end event

type st_1 from statictext within w_table_message
integer x = 91
integer y = 104
integer width = 1915
integer height = 76
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 33551856
boolean enabled = false
string text = "Table exists"
alignment alignment = center!
boolean focusrectangle = false
end type

type st_2 from statictext within w_table_message
integer x = 87
integer y = 260
integer width = 1920
integer height = 76
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16711680
long backcolor = 33551856
boolean enabled = false
string text = "Do you wish to replace with the new data or add new data as additional?"
boolean focusrectangle = false
end type

type cb_rep from commandbutton within w_table_message
integer x = 704
integer y = 408
integer width = 288
integer height = 108
integer taborder = 20
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Replace "
end type

event clicked;integer res

//res = messagebox("Replace","Are You sure?", question!,yesno!,1)
res = messagebox("Replace","Are You sure you want to replace it?", question!,yesno!,1)

if res = 1 then
	ii_mess = 1
	closewithreturn(parent,ii_mess)
end if
end event

type cb_add from commandbutton within w_table_message
integer x = 1083
integer y = 408
integer width = 256
integer height = 108
integer taborder = 10
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Add"
end type

event clicked;integer res

//res = messagebox("Append","Are You sure?", question!,yesno!,1)
res = messagebox("Append","Are You sure you want to replace it?", question!,yesno!,1)

if res = 1 then
	ii_mess = 2
	closewithreturn(parent,ii_mess)
end if
end event

type cb_1 from commandbutton within w_table_message
integer x = 1687
integer y = 424
integer width = 247
integer height = 76
integer taborder = 30
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "Cancel"
end type

event clicked;close(parent)
end event

