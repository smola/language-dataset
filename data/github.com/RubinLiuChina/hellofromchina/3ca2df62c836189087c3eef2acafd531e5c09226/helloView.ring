Load "stdlib.ring"
Load "guilib.ring"

import System.GUI

if IsMainSourceFile() { 
	new App {
		StyleFusion()
		new helloView { win.show() } 
		exec()
	}
}

class helloView from WindowsViewParent
	win = new MainWindow() { 
		move(0,3)
		resize(1129,717)
		setWindowTitle("Hello")
		setstylesheet("background-color:;") 
		Image1 = new label(win) {
			move(8,162)
			resize(861,501)
			setstylesheet("color:black;background-color:;")
			oFont = new qfont("",0,0,0)
			oFont.fromstring("Arial")
			setfont(oFont)
			oFont.delete()
			setPixMap(New qPixMap("china.jpg"))
			
		}
		Label1 = new label(win) {
			move(307,5)
			resize(842,74)
			setstylesheet("color:black;background-color:;")
			oFont = new qfont("",0,0,0)
			oFont.fromstring("MS Shell Dlg 2,36,-1,5,50,0,0,0,0,0")
			setfont(oFont)
			oFont.delete()
			setText("Hello, from China!")
			setAlignment(Qt_AlignLeft |  Qt_AlignVCenter)
		}
		Label2 = new label(win) {
			move(79,69)
			resize(973,85)
			setstylesheet("color:black;background-color:;")
			oFont = new qfont("",0,0,0)
			oFont.fromstring("MS Shell Dlg 2,24,-1,5,50,0,0,0,0,0")
			setfont(oFont)
			oFont.delete()
			setText("最近好吗？(zuì jìn hào mǎ) “How are you these days?”")
			setAlignment(Qt_AlignLeft |  Qt_AlignVCenter)
		}
		Button1 = new pushbutton(win) {
			move(887,163)
			resize(219,41)
			setstylesheet("color:black;background-color:;")
			oFont = new qfont("",0,0,0)
			oFont.fromstring("MS Shell Dlg 2,16,-1,5,50,0,0,0,0,0")
			setfont(oFont)
			oFont.delete()
			setText("Hello")
			setClickEvent(Method(:hello))
			setBtnImage(Button1,"")
			
		}
		Button5 = new pushbutton(win) {
			move(887,213)
			resize(219,41)
			setstylesheet("color:black;background-color:;")
			oFont = new qfont("",0,0,0)
			oFont.fromstring("MS Shell Dlg 2,16,-1,5,50,0,0,0,0,0")
			setfont(oFont)
			oFont.delete()
			setText("Close")
			setClickEvent(Method(:close))
			setBtnImage(Button5,"")
			
		}
	}

# End of the Generated Source Code File...