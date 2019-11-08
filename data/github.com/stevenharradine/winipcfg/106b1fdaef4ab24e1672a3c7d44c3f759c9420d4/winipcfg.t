import GUI in "%oot/lib/GUI"
setscreen ("graphics:400;200, position:center;center, title:IP Configuration, nobuttonbar")
var FontID := Font.New ("Ariel:9")
var IPAddrFrame := GUI.CreateFrame (150, 155, 375, 170, GUI.INDENT)
var compNameFrame := GUI.CreateFrame (150, 135, 375, 150, GUI.INDENT)
var defaultGatewayFrame := GUI.CreateFrame (150, 115, 375, 130, GUI.INDENT)
var IPAddr := Net.LocalAddress
var compName := Net.LocalName
var defaultGateway : string
var dot : int := length (IPAddr)

procedure text
    drawbox (10, 100, maxx - 10, maxy - 10, black)
    drawline (15, maxy - 10, 175, maxy - 10, 30)
    drawbox (10, 10, maxx - 10, 90, black)
    Font.Draw ("Ethernet Adapter Information", 17, maxy - 14, FontID, black)
    Font.Draw ("IP Address", 80, 159, FontID, black)
    Font.Draw (IPAddr, 175, 159, FontID, black)
    Font.Draw ("Computer Name", 48, 139, FontID, black)
    Font.Draw (compName, 175, 139, FontID, black)
    Font.Draw ("Default Gateway", 50, 119, FontID, black)
    Font.Draw (defaultGateway, 175, 119, FontID, black)
end text

procedure reset
    IPAddr := Net.LocalAddress
    compName := Net.LocalName
    dot := length (IPAddr)
    loop
	exit when IPAddr (dot) = "."
	dot -= 1
    end loop
    dot := length (IPAddr) - dot
    if IPAddr (*) = "0" and IPAddr (* -1) = "." then
	defaultGateway := "Error"
    else
	defaultGateway := IPAddr (1 .. * -dot) + "1"
    end if
    cls
    GUI.Refresh
    text
end reset

procedure releasePro
    var exec := Sys.Exec ("release.bat")
end releasePro
procedure renewPro
    var exec2 := Sys.Exec ("renew.bat")
end renewPro

var quitBtn : int := GUI.CreateButton (30, 60, 0, "       OK       ", GUI.Quit)
var releaseBtn : int := GUI.CreateButton (160, 60, 0, "  Release  ", releasePro)
var renewBtn : int := GUI.CreateButton (280, 60, 0, "      Renew     ", renewPro)
var releaseAllBtn : int := GUI.CreateButton (30, 25, 0, "Release All", GUI.Quit)
var renewAllBtn : int := GUI.CreateButton (160, 25, 0, "Renew All", GUI.Quit)
var moreInfoBtn : int := GUI.CreateButton (280, 25, 0, "     Refresh    ", reset)
GUI.Disable (releaseAllBtn)
GUI.Disable (renewAllBtn)

loop
    exit when IPAddr (dot) = "."
    dot -= 1
end loop
dot := length (IPAddr) - dot
if IPAddr (*) = "0" and IPAddr (* -1) = "." then
    defaultGateway := "Error"
else
    defaultGateway := IPAddr (1 .. * -dot) + "1"
end if

colorback (30)
cls
GUI.Refresh

text

loop
    exit when GUI.ProcessEvent
end loop
