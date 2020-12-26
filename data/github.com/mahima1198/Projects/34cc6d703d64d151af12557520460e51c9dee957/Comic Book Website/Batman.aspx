<%@ Page Language="C#" AutoEventWireup="true" CodeFile="Batman.aspx.cs" Inherits="Batman" MasterPageFile="~/MasterPage.master" %>
<asp:Content ID="Content2" ContentPlaceHolderID="head" Runat="Server">
</asp:Content>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" runat="Server">
 

    
    <table style="width:100%; margin-top:50px; " cellspacing="25" >
    
        <tr>
            <td><a href="cm.aspx"> <img src="batcomic1.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
            <td><a href="cm.aspx"> <img src="batcomic2.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
            <td><a href="cm.aspx"> <img src="batcomic3.png" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
            <td><a href="cm.aspx"> <img src="batcomic4.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
         
        </tr>
        <tr>
            <td><a href="cm.aspx"> <img src="batcomic5.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
            <td><a href="cm.aspx"> <img src="batcomic6.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
            <td><a href="cm.aspx"> <img src="batcomic7.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
            <td><a href="cm.aspx"> <img src="batcomic8.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
         
        </tr>
        <tr>
            <td><a href="cm.aspx"> <img src="batcomic9.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
            <td><a href="cm.aspx"> <img src="batcomic10.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
            <td><a href="cm.aspx"> <img src="batcomic1.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
            <td><a href="cm.aspx"> <img src="batcomic2.jpg" alt="Mountain View" style="height:240px; width:180px;  "></a></td>
         
        </tr>
          
    </table>
        <table style="width:100%; margin-top:50px; " cellspacing="25" >  
        <tr>
            <td><asp:Button ID="writecomment" runat="server" Text="WRITE A COMMENT" Width="244px" OnClick="writecomment_Click"  /></td>
              <td><asp:Button ID="viewcomment" runat="server" Text="VIEW ALL COMMENTS" Width="244px" PostBackUrl="~/ViewCommentb.aspx" /></td>
        </tr>

    </table>
     
          
</asp:Content>


