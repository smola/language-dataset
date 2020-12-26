<%@ Control Language="C#" AutoEventWireup="true" CodeFile="AddLinkRecord.ascx.cs" Inherits="controls_AddLinkRecord" %>
<table style="width: 703px">
    <tr>
        <td style="width: 144px">
            姓名</td>
        <td style="width: 210px">
            <asp:TextBox ID="txtname" runat="server" Width="210px"></asp:TextBox></td>
        <td style="width: 123px">
            客户名称</td>
        <td>
            <asp:TextBox ID="txtusername" runat="server" Width="196px"></asp:TextBox></td>
    </tr>
    <tr>
        <td style="width: 144px">
            备注</td>
        <td style="width: 210px">
            <asp:TextBox ID="txtnote" runat="server" Rows="10" TextMode="MultiLine" Width="209px"></asp:TextBox></td>
        <td style="width: 123px">
            日期</td>
        <td>
            <asp:Calendar ID="Calendar1" runat="server" Width="183px"></asp:Calendar>
        </td>
    </tr>
    <tr>
        <td style="width: 144px">
        </td>
        <td style="width: 210px">
            <asp:Button ID="Button1" runat="server" OnClick="Button1_Click" Text="添加" Width="87px" /></td>
        <td style="width: 123px">
            <asp:Label ID="Label1" runat="server" Width="101px"></asp:Label></td>
        <td>
        </td>
    </tr>
</table>
<asp:RequiredFieldValidator ID="RequiredFieldValidator1" runat="server" ControlToValidate="txtname"
    ErrorMessage="姓名必须填写"></asp:RequiredFieldValidator>
<asp:RequiredFieldValidator ID="RequiredFieldValidator2" runat="server" ControlToValidate="txtusername"
    ErrorMessage="客户名称必须填写"></asp:RequiredFieldValidator>
