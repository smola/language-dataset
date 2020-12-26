<%@ Page Title="" Language="C#" MasterPageFile="~/panel_panel/admin_master.master" AutoEventWireup="true" CodeFile="create-plan.aspx.cs" Inherits="nyksi_panel_Upload_Main_Product" %>
<%@ Register Assembly="AjaxControlToolkit" Namespace="AjaxControlToolkit.HTMLEditor" TagPrefix="cc1" %>
<asp:Content ID="Content1" ContentPlaceHolderID="head" Runat="Server">
     <script language="javascript" type="text/javascript">
      <!--
    function isNumberKey(evt) {
        var charCode = (evt.which) ? evt.which : event.keyCode
        if (charCode > 31 && (charCode < 48 || charCode > 57))
            return false;

        return true;
    }
    //-->
   </script>
     <script type="text/Javascript">
         function checkDec(el) {
             var ex = /^[0-9]+\.?[0-9]*$/;
             if (ex.test(el.value) == false) {
                 el.value = el.value.substring(0, el.value.length - 1);
             }
         }
</script>


</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">
     <div class="row-fluid">
               <div class="span12">
                   <!-- BEGIN THEME CUSTOMIZER-->
                   <div id="theme-change" class="hidden-phone">
                       <i class="icon-cogs"></i>
                        <span class="settings">
                            <span class="text">Theme:</span>
                            <span class="colors">
                                <span class="color-default" data-style="default"></span>
                                <span class="color-gray" data-style="gray"></span>
                                <span class="color-purple" data-style="purple"></span>
                                <span class="color-navy-blue" data-style="navy-blue"></span>
                            </span>
                        </span>
                   </div>
                   <!-- END THEME CUSTOMIZER-->
                  <h3 class="page-title">
                     Create Portfolio
                    <%-- <small>form components and widgets</small>--%>
                  </h3>
                   <ul class="breadcrumb">
                       <li><a href="Default.aspx"><i class="icon-home"></i></a><span class="divider">&nbsp;</span></li>
                       <li><a href="#">Portfolio</a><span class="divider">&nbsp;</span></li>
                       <li><a href="#"> Create Portfolio</a><span class="divider-last">&nbsp;</span></li>
                   </ul>
               </div>
            </div>

    <div class="row-fluid">
               <div class="span12">
                  <div class="widget">
                        <div class="widget-title">
                           <h4><i class="icon-user"></i>Upload Here <span style="color:red">(Image Size:- Width:1920px & Height:1080px)</span> </h4>             
                        </div>
                        <div class="widget-body">
                            <!--BEGIN ABOUT US-->
                 <form action="#" class="form-horizontal" id="fhgfh" runat="server">
                     
                           <div class="about-us">
                              
                               <div class="row-fluid">
                               <div class="span6">
                                       

                                       <div class="control-group">
                                       <label class="control-label">Choose  Category</label>
                                       <div class="controls">
                                           <asp:DropDownList ID="ddlcat" Width="100%"
                                      runat="server"></asp:DropDownList>
                                     <asp:RequiredFieldValidator ID="RequiredFieldValidator3" runat="server" ErrorMessage="Choose  Category" InitialValue="--- Select Category ---" ControlToValidate="ddlcat" ValidationGroup="tt" ForeColor="Red"></asp:RequiredFieldValidator>
                                       </div>
                                       </div>


                                     

                                

                                    <div class="control-group">
                                       <label class="control-label">Plan Name</label>
                                       <div class="controls">
                                            <asp:TextBox ID="txtplanName" runat="server"  Width="100%"></asp:TextBox>
                                           <asp:RequiredFieldValidator ID="RequiredFieldValidator4" runat="server" ControlToValidate="txtplanName" ValidationGroup="tt" ErrorMessage="Pls. Insert Course Name" ForeColor="#cc3300"></asp:RequiredFieldValidator> 
                                      
                                       </div>
                                       </div>


                                   <div class="control-group">
                                       <label class="control-label"> Plan Heading</label>
                                       <div class="controls">
                                            <asp:TextBox ID="Textplanheading" runat="server"  Width="100%"></asp:TextBox>
                                           <asp:RequiredFieldValidator ID="RequiredFieldValidator2" runat="server" ControlToValidate="Textplanheading" ValidationGroup="tt" ErrorMessage="Pls. Insert Course Name" ForeColor="#cc3300"></asp:RequiredFieldValidator> 
                                      
                                       </div>
                                       </div>






                        
                                     <div class="control-group">
                                       <label class="control-label">Choose Image :
                                           
                                       </label>
                                       <div class="controls">
                                           <asp:FileUpload ID="FileUpload1" runat="server" />
                                          
                                              <asp:RequiredFieldValidator ID="RequiredFieldValidator1" runat="server" ControlToValidate="FileUpload1" ValidationGroup="tt" ErrorMessage="Pls. Choose Image" ForeColor="#cc3300"></asp:RequiredFieldValidator> 
                                       </div>
                                       </div>
                                   

                                     
                                 
                                </div>

                                    </div>

                          


                                
                               
                             

                                <div class="form-actions clearfix">

                                 <div class="control-group">
                                       <label class="control-label"></label>
                                       <div class="controls">
                                       <asp:Button ID="Button1" runat="server" Text="Submit" CssClass="btn btn-success" ValidationGroup="tt" OnClick="Button1x_Click"></asp:Button> 
                                       </div>
                                       </div>
                                </div>



                               

                              
                               </div>
                               
                             
                          
                            <!--END ABOUT US-->

                      </form>
                   </div>
               </div>
            </div>
        </div>
   
</asp:Content>

