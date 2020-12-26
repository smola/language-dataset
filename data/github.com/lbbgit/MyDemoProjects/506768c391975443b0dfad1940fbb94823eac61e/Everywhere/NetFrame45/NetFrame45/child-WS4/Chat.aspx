<%@ Page Language="C#" AutoEventWireup="true" CodeFile="Chat.aspx.cs" Inherits="Chat" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>聊天主页</title>
    <script type="text/javascript" src="Ajax.js"></script>
    <script type="text/javascript"> 
      Get();
     window.setInterval("Get()",2012);    
    

    </script>
</head>
<body style="text-align: center" >

    <form id="form1" runat="server">
        <table border="0" cellpadding="0" cellspacing="0" style="width: 765px; height: 262px" >
            <tr>
                <td align="center" valign="top" style="height: 177px; width: 857px;">
                    <table border="1" cellpadding="0" cellspacing="0" style="width: 797px; height: 488px">
                        <tr>
                            <td style="height: 23px; text-align: left; width: 756px;" valign="top" colspan="2">
                                <span style="font-size: 20px">
                                <tt>聊天室主页</tt> </span>&nbsp;
                                <span style="font-size: 9pt">当前在线人数：<%=Application["Count"].ToString()%></span></td>
                        </tr>
                        <tr>
                            <td style="text-align: left; height: 485px; width: 756px;" valign="top" id="Chat" colspan="2">
                                &nbsp; &nbsp; &nbsp;
                                <br />
                                &nbsp; &nbsp; 信息加载中...
                               </td>
                        </tr>
                    </table>
                </td>
            </tr>
            <tr>
                <td style="height: 47px; width: 857px;" >
                    <input id="msg" type="text"  style=" border:solid 1px; border-color:#cccccc; width: 399px; height: 21px;" /><input id="Button1"  style=" border:solid 1px; width: 49px; height: 24px;" type="button" value="发言" onclick="Send()" /><asp:LinkButton
                        ID="LinkButton1" runat="server" OnClick="LinkButton1_Click" Height="22px" Width="54px">退出</asp:LinkButton><tt id="info"></tt></td>
            </tr>
        </table>
                   
    </form>
</body>
</html>
