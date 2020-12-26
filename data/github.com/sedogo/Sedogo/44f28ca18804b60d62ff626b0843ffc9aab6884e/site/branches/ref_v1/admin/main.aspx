<%@ Page Language="C#" MasterPageFile="~/admin/AdminMaster.master" AutoEventWireup="true" 
    CodeFile="main.aspx.cs" Inherits="admin_main" Theme="AdminTheme" %>
<%@ OutputCache Location="None" VaryByParam="None" %>

<asp:Content ID="Content1" ContentPlaceHolderID="head" Runat="Server">
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

    <asp:LinkButton ID="logoutLink" runat="server" OnClick="logoutLink_click" />
    
    Logged in as: <asp:Label ID="loggedInAsLabel" runat="server" /><br />&nbsp;<br />
    
    Admin menu<br />&nbsp;<br />
    
    <asp:LinkButton ID="administratorsLink" runat="server" Text="Administrators" OnClick="administratorsLink_click" /><br />&nbsp;<br />
    <asp:LinkButton ID="usersLink" runat="server" Text="Users" OnClick="usersLink_click" /><br />&nbsp;<br />
    <asp:LinkButton ID="timezonesLink" runat="server" Text="Timezones" OnClick="timezonesLink_click" /><br />&nbsp;<br />
    <asp:LinkButton ID="popularGoalsLink" runat="server" Text="Popular goals" OnClick="popularGoalsLink_click" /><br />&nbsp;<br />
    <asp:LinkButton ID="allGoalsLink" runat="server" Text="All goals" OnClick="allGoalsLink_click" /><br />&nbsp;<br />
    <asp:LinkButton ID="defaultPageTimelineLink" runat="server" Text="Default page timeline" OnClick="defaultPageTimelineLink_click" /><br />&nbsp;<br />
    <asp:LinkButton ID="homePageContentLink" runat="server" Text="Home page content" OnClick="homePageContentLink_click" /><br />&nbsp;<br />
    <asp:LinkButton ID="broadcastEmailLink" runat="server" Text="Send all user email" OnClick="broadcastEmailLink_click" /><br />&nbsp;<br />

</asp:Content>

