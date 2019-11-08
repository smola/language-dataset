using paninui.*.
using paninui.view.interfaces.*.
using paninui.view.init.display.*.

using panaedra.msroot.msutil.logic.*.
using panaedra.msroot.msutil.interfaces.*.

{panaedra/msroot/msutil/logic/top_i/ctop_class.i} 

class paninui.mdiwindow.sc_mdiwindow: 

  /****************************** Source Header ********************************\

      Description: Static methods to start up and maintain an MDI window.
                   Needed in the ABL -p startup procedure.

  \*****************************  Include Files ********************************/

  /******************************* $NoKeywords:  $ *****************************/
  
  define public static variable bApplicationRunAllowed as logical no-undo initial true.
  
  define public static property oFirstMdiWindow as Progress.Lang.Object no-undo 
    public get. 
    protected set. 


  method public static void StartNewMdiWindow(cMdiClassNameIP# as character):

    StartNewMdiWindow("[no dll]", cMdiClassNameIP#).
    
  end method.  

  
  method public static void StartNewMdiWindow(cMdiDllNameIP# as character, cMdiClassNameIP# as character):

    define variable oMdiNew#   as i_masterwindow             no-undo.
    define variable cMdiNew#   as character                  no-undo.
    define variable oAssembly# as System.Reflection.Assembly no-undo.
    define variable oForm#     as System.Windows.Forms.Form  no-undo.
   
    if cMdiDllNameIP# <> "[no dll]" then 
    do:

      file-info:file-name = cMdiDllNameIP#.
      cMdiNew# = file-info:full-pathname.

      if cMdiNew# <> ? then 
      do on error undo, throw:
        
        oAssembly# =
          System.Reflection.Assembly:LoadFile(cMdiNew#).
        if oAssembly# <> ? then
          oMdiNew# = cast(oAssembly#:CreateInstance(cMdiClassNameIP#), i_masterwindow).
        
        catch oDotnetErrorIP# as System.Exception:
          sc_showexception:ShowException(cMdiNew#, oDotnetErrorIP#).
        end catch.
        
      end.
    
    end.
    else
      oMdiNew# = dynamic-new cMdiClassNameIP#().
    
    oForm# = cast(cast(oMdiNew#,Progress.Lang.Object), System.Windows.Forms.Form).
    
    /* ShouldHave: SetAliases doesn't really belong here. */
    if lookup(sc_boot:SessionParameter("Layer"),"all,db,db_r,db_u") > 0 then
      panaedra.msroot.msas.logic.sc_dbconnect:SetAliases().

    paninui.view.use.sc_coll_eventdata:Initialize().
    
    /* ShouldHave: Support for multiple MDI windows in one session */
    if oFirstMdiWindow = ? then oFirstMdiWindow = oMdiNew#.
    paninui.view.init.events.sc_sgt_masterwindow_eventhandler:oMdiWindow        = oMdiNew#.

    if os-getenv("ECLIPSE_PROJECT") > "" then
      /* This means the eclipse debugger is running; use only Shown event for less crashes (but uglier screen at startup) */
      oForm#:Shown:subscribe(paninui.view.init.events.sc_sgt_masterwindow_eventhandler:oSgt:MdiOnShown).
    else 
    do:
      oForm#:Load:subscribe(paninui.view.init.events.sc_sgt_masterwindow_eventhandler:oSgt:MdiOnLoad).
      oForm#:Shown:subscribe(paninui.view.init.events.sc_sgt_masterwindow_eventhandler:oSgt:MdiOnShown).
    end.
    
    do on error undo, throw
      on stop undo, retry:
      
      if bApplicationRunAllowed = false then quit. /* Necessary when in loop */
      bApplicationRunAllowed = false.              /* Prevent infinite session startup loops when uncaught exceptions happen in another .NET thread */
      
      wait-for System.Windows.Forms.Application:Run(oForm#).
      
      if retry then do:
        
        undo, throw new c_panaedraexception(
          99949228,
          sc_lang:oEnglish,
          "Application:Run() loop has ended (stop condition)."
          ).
      end.
      
      catch oException# as System.Exception:
        sc_showexception:ShowException(oException#).  
      end catch.
      
      catch oError# as Progress.Lang.Error:
        sc_showexception:ShowException(oError#).  
      end catch.
      
    end. 

  end method. /* StartNewMdiWindow */
  
end class.

/* EOF */ 
