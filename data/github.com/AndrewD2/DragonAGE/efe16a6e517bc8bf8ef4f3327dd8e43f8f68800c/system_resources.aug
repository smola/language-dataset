<?xml version="1.0" encoding="UTF-8"?>

<!-- This file contains definitions of all system resource replacements to be utilized
      for the game system within the user-interface of Hero Lab. System resources are
      those utilized for general mechanisms within Hero Lab and are not exclusively
      used by elements defined by the data files. The ids for these resources are
      pre-defined for Hero Lab and, by defining them here, the built-in defaults of
      Hero Lab are replaced with the new behaviors.

      Custom visual elements used within the user-interface are defined within the file
      "styles_ui.aug", and Visual elements utilized for printed output are defined
      within "styles_output.aug".

      IMPORTANT!!!!
      If you are happy using the stock visual presentation provided within Hero Lab,
      you can freely delete this file from the data files for your game system. This
      file contains definition and mappings for all system resources that simply point
      to all of the built-in, default resources provided by Hero Lab. If you want to
      change the visuals, you can systematically swap out visual elements within this
      file to suit the visual look you want. But if you want the stock visuals, this
      file is extraneous and should be deleted.
-->

<document signature="Hero Lab Structure">


  <!-- **********   TILED BACKGROUND TEXTURES AND REFERENCE COLORS   ********** -->


  <!-- tile bitmap used in outer background areas -->
  <resource
    id="mainwindow"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="mainwindow.bmp">
      </bitmap>
    </resource>

  <!-- color used in conjunction with outer background tile bitmap for determining
        suitable colors for raised/lowered borders
  -->
  <resource
    id="refmainwin"
    issystem="yes">
    <color
      color="173766">
      </color>
    </resource>

  <!-- tile bitmap used in interior background regions -->
  <resource
    id="background"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="background.bmp">
      </bitmap>
    </resource>

  <!-- color used in conjunction with interior background tile bitmap for determining suitable colors for raised/lowered borders
  -->
  <resource
    id="refbackgrd"
    issystem="yes">
    <color
      color="15294e">
      </color>
    </resource>

  <!-- tile bitmap used within the "static" panel across the top -->
  <resource
    id="staticpan"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="mainwindow.bmp">
      </bitmap>
    </resource>


  <!-- **********   STANDARD LABEL COLORS   ********** -->


  <!-- color of text within standard labels within Hero Lab -->
  <resource
    id="lblnormal"
    issystem="yes">
    <color
      color="dcdce1">
      </color>
    </resource>

  <!-- color of text within title areas -->
  <resource
    id="lbltitle"
    issystem="yes">
    <color
      color="dcdce1">
      </color>
    </resource>

  <!-- text color used for disabled text, such as choices that fail their pre-requisites -->
  <resource
    id="lbldisable"
    issystem="yes">
    <color
      color="646464">
      </color>
    </resource>

  <!-- text color used for warning and error text, such as to identify invalid selections -->
  <resource
    id="lblwarning"
    issystem="yes">
    <color
      color="ee0033">
      </color>
    </resource>

  <!-- text color used for lighter shaded warnings, such as for the current actor within
        the validation report form -->
  <resource
    id="lblwarnlt"
    issystem="yes">
    <color
      color="ff8888">
      </color>
    </resource>

  <!-- color of text for small font areas where higher contrast is desired -->
  <resource
    id="lblsmall"
    issystem="yes">
    <color
      color="dcdceb">
      </color>
    </resource>

  <!-- color of text for tiny labels beneath edit fields, as used on the Journal tab and within the Editor -->
  <resource
    id="lbltiny"
    issystem="yes">
    <color
      color="61b5e7">
      </color>
    </resource>

  <!-- color used for highlighting special labels -->
  <resource
    id="lblbright"
    issystem="yes">
    <color
      color="ffff88">
      </color>
    </resource>

  <!-- color used for toning down labels that shouldn't be too prominent -->
  <resource
    id="lbldim"
    issystem="yes">
    <color
      color="a0a0a0">
      </color>
    </resource>

  <!-- color used for labels against the alternative "background" bitmap texture as seen in the Editor -->
  <resource
    id="lblaltern"
    issystem="yes">
    <color
      color="dcdceb">
      </color>
    </resource>

  <!-- color used for disabled labels against the alternative "background" bitmap texture -->
  <resource
    id="lblaltdis"
    issystem="yes">
    <color
      color="646464">
      </color>
    </resource>


  <!-- **********   SUMMARY PANEL FACETS   ********** -->


  <!-- bitmap used for the title background within summary panels -->
  <resource
    id="sumtitle"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="summary_title.bmp">
      </bitmap>
    </resource>

  <!-- tile bitmap used as background within summary panels -->
  <resource
    id="sumbackgrd"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="background.bmp">
      </bitmap>
    </resource>

  <!-- bitmap used for the "hide" button in the top right of summary panels -->
  <resource
    id="sumhide"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="summary_hide.bmp">
      </bitmap>
    </resource>

  <!-- color used when drawing the title above summary panels -->
  <resource
    id="sumtext"
    issystem="yes">
    <color
      color="dcdceb">
      </color>
    </resource>


  <!-- **********   TABS ACROSS THE TOP   ********** -->


  <!-- bitmaps used for the background region of tabs shown at the top
        -one bitmap is one for a selected tab and the other for an unselected tab
  -->
  <resource
    id="tabunsel"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="background.bmp">
      </bitmap>
    </resource>
  <resource
    id="tabsel"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="background.bmp">
      </bitmap>
    </resource>

  <!-- text colors used in conjunction with tabs
        -one for the text color of a selected tab, one for text in an unselected tab,
          and one for a tab with dirty contents in the Editor
  -->
  <resource
    id="tabtextsel"
    issystem="yes">
    <color
      color="f0f0f0">
      </color>
    </resource>
  <resource
    id="tabtextuns"
    issystem="yes">
    <color
      color="82c8f7">
      </color>
    </resource>
  <resource
    id="tabdirty"
    issystem="yes">
    <color
      color="ffad1f">
      </color>
    </resource>
  <resource
    id="tabwarning"
    issystem="yes">
    <color
      color="ee0033">
      </color>
    </resource>


  <!-- **********   VALIDATION REPORTING   ********** -->


  <!-- bitmap used to indicate that validation errors exist in the upper right of
        the main window
  -->
  <resource
    id="validbigup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="valid_big_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used to indicate that validation errors exist within the validation
        summary bar at the bottom
        -one bitmap for the "error" (up) state and one for the "no error" (off) state
  -->
  <resource
    id="validsmlup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="valid_small_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="validsmlof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="valid_small_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- color used for the validation summary text at the bottom of the main window -->
  <resource
    id="validtext"
    issystem="yes">
    <color
      color="dcdcf5">
      </color>
    </resource>

  <!-- color used to indicate errors exist with other heroes in the validation report -->
  <resource
    id="validother"
    issystem="yes">
    <color
      color="ffff88">
      </color>
    </resource>


  <!-- **********   TEXT-BASED BUTTONS   ********** -->


  <!-- color of text within buttons -->
  <resource
    id="btntext"
    issystem="yes">
    <color
      color="000f9d">
      </color>
    </resource>

  <!-- color of disabled text within buttons -->
  <resource
    id="btndisable"
    issystem="yes">
    <color
      color="505050">
      </color>
    </resource>

  <!-- color of bright text within buttons (used in the editor for highlighting
        buttons - for example, if a record has one or more scripts, the scripts
        button text will be drawn in this color) -->
  <resource
    id="btnbright"
    issystem="yes">
    <color
      color="7a155b">
      </color>
    </resource>

  <!-- color of text within large oval buttons ("Load" button on "Select Game" form) -->
  <resource
    id="ovlbtntext"
    issystem="yes">
    <color
      color="000f73">
      </color>
    </resource>

  <!-- bitmaps for the standard "default" button used throughout HL, with text
        being overlaid on top of the bitmap to identify the action to perform
        -one bitmap is for the "up" state of the button, another for the "down"
          state, and a third for the disabled "off" state
  -->
  <resource
    id="btnbigup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="button_big_up.bmp">
      </bitmap>
    </resource>
  <resource
    id="btnbigdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="button_big_down.bmp">
      </bitmap>
    </resource>
  <resource
    id="btnbigof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="button_big_off.bmp">
      </bitmap>
    </resource>

  <!-- bitmaps for the smaller size version of the text-overlaid button used
        throughout HL (such as within the Editor)
        -one bitmap is for the "up" state of the button, another for the "down" state,
          and a third for the disabled "off" state
  -->
  <resource
    id="btnsmallup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="button_small_up.bmp">
      </bitmap>
    </resource>
  <resource
    id="btnsmalldn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="button_small_down.bmp">
      </bitmap>
    </resource>
  <resource
    id="btnsmallof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="button_small_off.bmp">
      </bitmap>
    </resource>

  <!-- bitmaps for the big oval button used on the Select Game form, with text
        being overlaid on top of the bitmap to identify the action to perform
        -one bitmap is for the "up" state of the button, another for the "down"
          state, and a third for the disabled "off" state
  -->
  <resource
    id="btnovalup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="button_oval_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="btnovaldn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="button_oval_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="btnovalof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="button_oval_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>


  <!-- **********   MENUS AND CHOOSERS   ********** -->


  <!-- bitmaps used for the "expand" arrow at the right of a droplist menu
        -one bitmap is for the "live" state and the other is the disabled "off" state
  -->
  <resource
    id="mnuarrow"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="menu_arrow.bmp">
      </bitmap>
    </resource>
  <resource
    id="mnuarrowof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="menu_arrow_off.bmp">
      </bitmap>
    </resource>

  <!-- smaller version of the "expand" arrow at the right of a droplist menu,
        as described above
  -->
  <resource
    id="mnusmall"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="menu_small_arrow.bmp">
      </bitmap>
    </resource>
  <resource
    id="mnusmallof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="menu_small_arrow_off.bmp">
      </bitmap>
    </resource>

  <!-- colors used in conjunction with droplist menus
        -two colors control the foreground/background of the current selection
        -one color controls the background of the dropped menu
        -two colors control the foreground/background of the currently highlighted
          item in the dropped menu
  -->
  <resource
    id="mnutext"
    issystem="yes">
    <color
      color="84c8f7">
      </color>
    </resource>
  <resource
    id="mnubackgrd"
    issystem="yes">
    <color
      color="2a2c47">
      </color>
    </resource>
  <resource
    id="mnulistbck"
    issystem="yes">
    <color
      color="2a2c47">
      </color>
    </resource>
  <resource
    id="mnuseltext"
    issystem="yes">
    <color
      color="1414f7">
      </color>
    </resource>
  <resource
    id="mnuselback"
    issystem="yes">
    <color
      color="f0f0f0">
      </color>
    </resource>

  <!-- bitmaps used for the "choose" star icon associated with choosers
        -one bitmap is for the "live" state of the "choose" star icon and the other
          is for the disabled "off" state
  -->
  <resource
    id="chsstar"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="chooser_star.bmp">
      </bitmap>
    </resource>
  <resource
    id="chsstarof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="chooser_star_off.bmp">
      </bitmap>
    </resource>


  <!-- **********   CHECKBOXES AND RADIO BUTTONS   ********** -->


  <!-- bitmaps used for radio buttons
        -one bitmap is for the "selected" (or filled) state and the other is for
          the "unselected" (or empty) state
  -->
  <resource
    id="radsel"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="radio_select.bmp">
      </bitmap>
    </resource>
  <resource
    id="radunsel"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="radio_empty.bmp">
      </bitmap>
    </resource>

  <!-- color for text displayed within radio button portals -->
  <resource
    id="radtext"
    issystem="yes">
    <color
      color="ffffff">
      </color>
    </resource>

  <!-- bitmaps used for checkboxes
        -one pair of bitmaps is for the "live" state and the other is for the disabled
          "off" state
        -each pair consists of a "checked" state and an "unchecked" (or empty) state
  -->
  <resource
    id="chksel"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="check_select.bmp">
      </bitmap>
    </resource>
  <resource
    id="chkunsel"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="check_empty.bmp">
      </bitmap>
    </resource>
  <resource
    id="chkselof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="check_select_off.bmp">
      </bitmap>
    </resource>
  <resource
    id="chkunselof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="check_empty_off.bmp">
      </bitmap>
    </resource>

  <!-- color used for text within checkboxes -->
  <resource
    id="chktext"
    issystem="yes">
    <color
      color="f0f0f0">
      </color>
    </resource>

  <!-- color used for text in checkboxes against the alternative "background" bitmap texture -->
  <resource
    id="chkaltern"
    issystem="yes">
    <color
      color="f0f0f0">
      </color>
    </resource>


  <!-- **********   EDIT PORTALS   ********** -->


  <!-- colors used for text editing portals
        -one for the text color and the other for the background color
  -->
  <resource
    id="edttext"
    issystem="yes">
    <color
      color="d2d2d2">
      </color>
    </resource>
  <resource
    id="edtback"
    issystem="yes">
    <color
      color="000626">
      </color>
    </resource>

  <!-- colors used for read-only text editing controls
        -one for the text color and the other the background color
  -->
  <resource
    id="edttextro"
    issystem="yes">
    <color
      color="d2d2d2">
      </color>
    </resource>
  <resource
    id="edtbackro"
    issystem="yes">
    <color
      color="173766">
      </color>
    </resource>

  <!-- colors used for the edit overlay control background
  -->
  <resource
    id="edtbackov"
    issystem="yes">
    <color
      color="000626">
      </color>
    </resource>


  <!-- **********   PRINT PREVIEW BUTTONS   ********** -->


  <!-- bitmaps used for the "print" button used on the Print Preview form
        -separate bitmaps are for the "up" state and "down" state
  -->
  <resource
    id="printup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="print_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="printdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="print_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "pdf" button used on the Print Preview form
        -separate bitmaps are for the "up" state and "down" state
  -->
  <resource
    id="pdfup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="pdf_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="pdfdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="pdf_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "first page" button used on the Print Preview form
        -separate bitmaps are for the "up" state, "down" state, and "off" state
  -->
  <resource
    id="pgfirstup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_first_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="pgfirstdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_first_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="pgfirstof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_first_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "last page" button used on the Print Preview form
        -separate bitmaps are for the "up" state, "down" state, and "off" state
  -->
  <resource
    id="pglastup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_last_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="pglastdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_last_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="pglastof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_last_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "previous page" button used on the Print Preview form
        -separate bitmaps are for the "up" state, "down" state, and "off" state
  -->
  <resource
    id="pgprevup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_prev_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="pgprevdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_prev_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="pgprevof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_prev_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "next page" button used on the Print Preview form
        -separate bitmaps are for the "up" state, "down" state, and "off" state
  -->
  <resource
    id="pgnextup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_next_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="pgnextdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_next_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="pgnextof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="page_next_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "zoom in" button used on the Print Preview form
        -separate bitmaps are for the "up" state, "down" state, and "off" state
  -->
  <resource
    id="zoominup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="zoom_in_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="zoomindn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="zoom_in_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="zoominof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="zoom_in_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "zoom out" button used on the Print Preview form
        -separate bitmaps are for the "up" state, "down" state, and "off" state
  -->
  <resource
    id="zoomoutup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="zoom_out_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="zoomoutdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="zoom_out_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="zoomoutof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="zoom_out_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "settings info" button used on the Change Settings form
        -separate bitmaps are for the "up" state, "down" state, and "off" state
  -->
  <resource
    id="setinfoup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="settings_info_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="setinfodn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="settings_info_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="setinfoof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="settings_info_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- color used for the header text on the "Settings Info" form, accessed by
      pressing the button above
  -->
  <resource
    id="setinfohdr"
    issystem="yes">
    <color
      color="f0f0f0">
      </color>
    </resource>

  <!-- bitmaps used for the "live filter" texture and buttons on all Choose
      forms
      -separate bitmaps for the "empty" state, and "up" & "down" states of the
        clear filter button
  -->
  <resource
    id="livefempty"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="live_filter_empty.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="livefclrup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="live_filter_clear_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="livefclrdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="live_filter_clear_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>


  <!-- **********   MISCELLANEOUS BUTTONS   ********** -->


  <!-- bitmaps for the button used to modify the configuration settings (or rules)
        on the configure character form
        -one bitmap is for the "up" state of the button, another is for the "down"
          state of the button, and the third is the monochrome mask to be applied
          to both state bitmaps
  -->
  <resource
    id="settingup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="settings_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="settingdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="settings_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="settingof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="settings_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "delete" button that appears in various places
        -separate bitmaps are for the "up" state, "down" state, and "disabled" state
  -->
  <resource
    id="deleteup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="delete_up.bmp">
      </bitmap>
    </resource>
  <resource
    id="deletedn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="delete_down.bmp">
      </bitmap>
    </resource>
  <resource
    id="deleteof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="delete_off.bmp">
      </bitmap>
    </resource>

  <!-- small delete buttons -->
  <resource
    id="deletesmup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="delete_small_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="deletesmdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="delete_small_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="deletesmof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="delete_small_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "info" button that appears in various places
        -separate bitmaps are for the "up" state, "down" state, and "disabled" state
  -->
  <resource
    id="infoup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="info_up.bmp">
      </bitmap>
    </resource>
  <resource
    id="infodn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="info_down.bmp">
      </bitmap>
    </resource>
  <resource
    id="infoof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="info_off.bmp">
      </bitmap>
    </resource>

  <!-- bitmaps used for the "edit" button that appears in various places
        -separate bitmaps are for the "up" state, "down" state, and "disabled" state
  -->
  <resource
    id="editup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="edit_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="editdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="edit_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="editof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="edit_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>


  <!-- **********   LISTS AND GRIDS   ********** -->


  <!-- colors used for lists and grids of information, including selection lists for
        the Editor and Updates mechanism
  -->
  <resource
    id="listfixed"
    issystem="yes">
    <color
      color="6e7487">
      </color>
    </resource>
  <resource
    id="listunsel"
    issystem="yes">
    <color
      color="84c8f7">
      </color>
    </resource>
  <resource
    id="listsel"
    issystem="yes">
    <color
      color="f0f0f0">
      </color>
    </resource>

  <!-- color of lines used within lists and grids -->
  <resource
    id="listlines"
    issystem="yes">
    <color
      color="3c3c3c">
      </color>
    </resource>

  <!-- color of lines used within tables - table text is usually a different
      (less bright) color from text used in lists / grids, so the table line
      color needs to be different than regular list lines -->
  <resource
    id="tablelines"
    issystem="yes">
    <color
      color="373737">
      </color>
    </resource>


  <!-- **********   TREE VIEW DISPLAY   ********** -->


  <!-- bitmaps used for the expand and collapse state of nodes within a tree view -->
  <resource
    id="trvexpand"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="treeview_expanded.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="trvcollaps"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="treeview_collapsed.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- colors used when drawing the nodes and connections for tree views
  -->
  <resource
    id="trvnode"
    issystem="yes">
    <color
      color="6eb5e7">
      </color>
    </resource>
  <resource
    id="trvconnect"
    issystem="yes">
    <color
      color="6eb5e7">
      </color>
    </resource>


  <!-- **********   MESSAGES AND TIP INFORMATION   ********** -->


  <!-- colors used for tooltips and other mouse-over info
        -one for the text color and another for the background color
  -->
  <resource
    id="tiptext"
    issystem="yes">
    <color
      color="000000">
      </color>
    </resource>
  <resource
    id="tipback"
    issystem="yes">
    <color
      color="ede7c5">
      </color>
    </resource>

  <!-- colors used within alert messages such as error messages
        -one color for message text and the other for small notes text that is
            sometimes included
  -->
  <resource
    id="msgtext"
    issystem="yes">
    <color
      color="f0f0f0">
      </color>
    </resource>
  <resource
    id="msgnotes"
    issystem="yes">
    <color
      color="f0f0f0">
      </color>
    </resource>

  <!-- color of text used within checkboxes on alert messages, such as the "don't
        show again" checkbox)
  -->
  <resource
    id="msgchktext"
    issystem="yes">
    <color
      color="f0f0f0">
      </color>
    </resource>

  <!-- color of the border around alert messages and progress bars -->
  <resource
    id="msgborder"
    issystem="yes">
    <color
      color="ffffff">
      </color>
    </resource>

  <!-- color of the text in progress messages -->
  <resource
    id="progtext"
    issystem="no">
    <color
      color="f0f0f0">
      </color>
    </resource>


  <!-- **********   HELP AND DEBUGGING INFORMATION   ********** -->


  <!-- colors used for the various debug info windows
        -one color for information text and the other for header at the top
  -->
  <resource
    id="infowintxt"
    issystem="yes">
    <color
      color="dcdceb">
      </color>
    </resource>
  <resource
    id="infowinhdr"
    issystem="yes">
    <color
      color="67809c">
      </color>
    </resource>

  <!-- colors used for the various help windows
        -one color for text, one for the background, and one for the title at the top
  -->
  <resource
    id="helptext"
    issystem="yes">
    <color
      color="f5f5f5">
      </color>
    </resource>
  <resource
    id="helpback"
    issystem="yes">
    <color
      color="000732">
      </color>
    </resource>
  <resource
    id="helptitle"
    issystem="yes">
    <color
      color="ffffff">
      </color>
    </resource>


  <!-- **********   INCREMENTERS   ********** -->


  <!-- bitmaps used in conjunction with the built-in, system incrementer
        -four bitmaps for the various states of the "+" button and its mask
        -four bitmaps for the various states of the "-" button and its mask
        -the incrementer has *no* background since it is transparent in behavior
  -->
  <resource
    id="incplusup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="incr_simple_plus.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="incplusdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="incr_simple_plus_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="incplusof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="incr_simple_plus_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="incminusup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="incr_simple_minus.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="incminusdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="incr_simple_minus_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="incminusof"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="incr_simple_minus_off.bmp"
      istransparent="yes">
      </bitmap>
    </resource>


  <!-- **********   ENCOUNTER BUILDER   ********** -->

  <resource
    id="missourdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="missing_source_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="missourup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="missing_source_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <resource
    id="misimpdn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="missing_source_important_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="misimpup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="missing_source_important_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <resource
    id="plus_of"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="plus_of.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="plus_dn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="plus_dn.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="plus_up"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="plus_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>


  <!-- **********   MISCELLANEOUS VISUAL ELEMENTS   ********** -->


  <!-- bitmap used for the button at the top of the dashboard that triggers a re-sort of the dashboard contents -->
  <resource
    id="dshsort"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="dashboard_sort.bmp">
      </bitmap>
    </resource>

  <!-- bitmaps used for bullets to identify validation errors (typically in red)
        and warnings (typically in yellow) within the validation report
  -->
  <resource
    id="bullred"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="bullet_red.bmp">
      </bitmap>
    </resource>
  <resource
    id="bullyellow"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="bullet_yellow.bmp">
      </bitmap>
    </resource>

  <!-- color of the physical bar shown within the progress bar -->
  <resource
    id="progbar"
    issystem="yes">
    <color
      color="6eb5e7">
      </color>
    </resource>

  <!-- colors used by the dice roller
        -one color for the newest roll result and the other for old rolls
  -->
  <resource
    id="dicenew"
    issystem="yes">
    <color
      color="ffff88">
      </color>
    </resource>
  <resource
    id="diceold"
    issystem="yes">
    <color
      color="dcdceb">
      </color>
    </resource>

  <!-- colors used by the license info text on the configure hero form, and
      button bitmaps used for the "buy Hero Lab" button there
  -->
  <resource
    id="cnflictext"
    issystem="yes">
    <color
      color="d2d2d2">
      </color>
    </resource>
  <resource
    id="buygameup"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="purchase_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>
  <resource
    id="buygamedn"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="purchase_down.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- bitmap used for the 'new update' icon in the updates list
  -->
  <resource
    id="updatenew"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="update_new.bmp">
      </bitmap>
    </resource>

  <!-- bitmap used for the 'change rules' icon on the configure hero form
  -->
  <resource
    id="rulesicon"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="rules_icon.bmp">
      </bitmap>
    </resource>

  <!-- color of the "change rules" title text on the configure hero form -->
  <resource
    id="rulestext"
    issystem="yes">
    <color
      color="c0c0c0">
      </color>
    </resource>

  <!-- bitmap used for the 'load actor' button on the validation form -->
  <resource
    id="validload"
    isbuiltin="yes"
    issystem="yes">
    <bitmap
      bitmap="load_up.bmp"
      istransparent="yes">
      </bitmap>
    </resource>

  <!-- color of the title text in wizards like the License Activation Wizard -->
  <resource
    id="wiztitle"
    issystem="yes">
    <color
      color="d2d2d2">
      </color>
    </resource>


  <!-- **********   SCROLLERS   ********** -->


  <!-- bitmaps used in conjunction with the vertical scroller
        -two bitmaps define the top end of the scroller and its monochrome mask
        -two bitmaps define the bottom end of the scroller and its monochrome mask
        -two bitmaps define the vertical bar segment and its monochrome mask
        -two bitmaps define the "thumb" and its monochrome mask
  -->
  <resource
    id="scrvtop"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="vscroller_top.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrvtopmk"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="vscroller_top_mask.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrvbtm"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="vscroller_bottom.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrvbtmmk"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="vscroller_bottom_mask.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrvbar"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="vscroller_bar.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrvbarmk"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="vscroller_bar_mask.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrvthm"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="vscroller_thumb.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrvthmmk"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="vscroller_thumb_mask.bmp">
      </bitmap>
    </resource>

  <!-- bitmaps used in conjunction with the horizontal scroller
        -two bitmaps define the left end of the scroller and its monochrome mask
        -two bitmaps define the right end of the scroller and its monochrome mask
        -two bitmaps define the horizontal bar segment and its monochrome mask
        -two bitmaps define the "thumb" and its monochrome mask
  -->
  <resource
    id="scrhtop"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="hscroller_top.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrhtopmk"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="hscroller_top_mask.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrhbtm"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="hscroller_bottom.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrhbtmmk"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="hscroller_bottom_mask.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrhbar"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="hscroller_bar.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrhbarmk"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="hscroller_bar_mask.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrhthm"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="hscroller_thumb.bmp">
      </bitmap>
    </resource>
  <resource
    id="scrhthmmk"
    issystem="yes"
    isbuiltin="yes">
    <bitmap
      bitmap="hscroller_thumb_mask.bmp">
      </bitmap>
    </resource>


  <!-- **********   BORDERS   ********** -->


  <!-- border used around forms that don't utilize standard Windows borders
        WARNING! The thickness of this particular border MUST ALWAYS be three pixels.
  -->
  <resource
    id="brdform"
    issystem="yes"
    isbuiltin="yes">
    <border>
      <topleft bitmap="brd_form_topleft.bmp"/>
      <topright bitmap="brd_form_topright.bmp"/>
      <bottomleft bitmap="brd_form_bottomleft.bmp"/>
      <bottomright bitmap="brd_form_bottomright.bmp"/>
      <left bitmap="brd_form_left.bmp"/>
      <top bitmap="brd_form_top.bmp"/>
      <right bitmap="brd_form_right.bmp"/>
      <bottom bitmap="brd_form_bottom.bmp"/>
      </border>
    </resource>

  <!-- border used for various purposes within interior areas -->
  <resource
    id="brdsystem"
    issystem="yes"
    isbuiltin="yes">
    <border>
      <topleft bitmap="brd_system_topleft.bmp"/>
      <topright bitmap="brd_system_topright.bmp"/>
      <bottomleft bitmap="brd_system_bottomleft.bmp"/>
      <bottomright bitmap="brd_system_bottomright.bmp"/>
      <left bitmap="brd_system_left.bmp"/>
      <top bitmap="brd_system_top.bmp"/>
      <right bitmap="brd_system_right.bmp"/>
      <bottom bitmap="brd_system_bottom.bmp"/>
      </border>
    </resource>

  <!-- border used around the edit panel in HL and around the current tab within the Editor -->
  <resource
    id="brdedit"
    issystem="yes"
    isbuiltin="yes">
    <border>
      <topleft bitmap="brd_system_topleft.bmp"/>
      <topright bitmap="brd_system_topright.bmp"/>
      <bottomleft bitmap="brd_system_bottomleft.bmp"/>
      <bottomright bitmap="brd_system_bottomright.bmp"/>
      <left bitmap="brd_system_left.bmp"/>
      <top bitmap="brd_system_top.bmp"/>
      <right bitmap="brd_system_right.bmp"/>
      <bottom bitmap="brd_system_bottom.bmp"/>
      </border>
    </resource>

  <!-- border used around summary panels -->
  <resource
    id="brdsummary"
    issystem="yes"
    isbuiltin="yes">
    <border>
      <topleft bitmap="brd_system_topleft.bmp"/>
      <topright bitmap="brd_system_topright.bmp"/>
      <bottomleft bitmap="brd_system_bottomleft.bmp"/>
      <bottomright bitmap="brd_system_bottomright.bmp"/>
      <left bitmap="brd_system_left.bmp"/>
      <top bitmap="brd_system_top.bmp"/>
      <right bitmap="brd_system_right.bmp"/>
      <bottom bitmap="brd_system_bottom.bmp"/>
      </border>
    </resource>

  <!-- border used around the selected tab -->
  <resource
    id="brdtabsel"
    issystem="yes"
    isbuiltin="yes">
    <border>
      <topleft bitmap="brd_seltab_topleft.bmp"/>
      <topright bitmap="brd_seltab_topright.bmp"/>
      <bottomleft bitmap="brd_seltab_bottomleft.bmp"/>
      <bottomright bitmap="brd_seltab_bottomright.bmp"/>
      <left bitmap="brd_seltab_left.bmp"/>
      <top bitmap="brd_seltab_top.bmp"/>
      <right bitmap="brd_seltab_right.bmp"/>
      <bottom bitmap="brd_seltab_bottom.bmp"/>
      </border>
    </resource>

  <!-- border used around all unselected tabs -->
  <resource
    id="brdtabuns"
    issystem="yes"
    isbuiltin="yes">
    <border>
      <topleft bitmap="brd_unseltab_topleft.bmp"/>
      <topright bitmap="brd_unseltab_topright.bmp"/>
      <bottomleft bitmap="brd_unseltab_bottomleft.bmp"/>
      <bottomright bitmap="brd_unseltab_bottomright.bmp"/>
      <left bitmap="brd_unseltab_left.bmp"/>
      <top bitmap="brd_unseltab_top.bmp"/>
      <right bitmap="brd_unseltab_right.bmp"/>
      <bottom bitmap="brd_unseltab_bottom.bmp"/>
      </border>
    </resource>

  <!-- border used around summary panel titles (as seen in Mutants & Masterminds)
        NOTE! Only define this border if you want to utilize a funky border around the
              titles of summay panels. If no border is defined (it is commented out
              below), then the standard visual presentation of summary titles is used.
  -->
  <!--
  <resource
    id="brdsumtitl"
    issystem="yes"
    isbuiltin="yes">
    <border>
      <topleft bitmap="?.bmp"/>
      <topright bitmap="?.bmp"/>
      <bottomleft bitmap="?.bmp"/>
      <bottomright bitmap="?.bmp"/>
      <left bitmap="?.bmp"/>
      <top bitmap="?.bmp"/>
      <right bitmap="?.bmp"/>
      <bottom bitmap="?.bmp"/>
      </border>
    </resource>
  -->


  </document>
