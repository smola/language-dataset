<?xml version="1.0" encoding="UTF-8"?>

<!-- This file contains definitions of all custom resources and styles used for
      displaying visual elements within the user-interface of Hero Lab. Visual elements
      utilized for printed output are defined within "styles_output.aug". All sytem
      resources replaced for the game system are defined within "system_resources.aug".
-->

<document signature="Hero Lab Structure">


<!--
        ##########  Font Resource Definitions  ##########
-->

<!-- ##### Fonts - Labels ##### -->

  <!-- font used on normal labels (10pt) -->
  <resource
    id="fntnormal">
    <font
      face="Arial"
      size="40"
      style="bold">
      </font>
    </resource>

  <!-- font used when slightly larger is needed (11pt) -->
  <resource
    id="fntlarge">
    <font
      face="Arial"
      size="44"
      style="bold">
      </font>
    </resource>

  <!-- font used when much larger is needed (15pt) -->
  <resource
    id="fntxlarge">
    <font
      face="Arial"
      size="60"
      style="bold">
      </font>
    </resource>

  <!-- font used when slightly smaller is needed (9pt) -->
  <resource
    id="fntsmall">
    <font
      face="Arial"
      size="36"
      style="bold">
      </font>
    </resource>

  <!-- font used for headers above tables (8pt) -->
  <resource
    id="fntheader">
    <font
      face="Arial"
      size="32"
      style="bold">
      </font>
    </resource>

  <!-- font used on title labels (10pt) -->
  <resource
    id="fnttitle">
    <font
      face="Arial"
      size="40"
      style="bold">
      </font>
    </resource>

  <!-- font used on summary labels (10pt) -->
  <resource
    id="fntsummary">
    <font
      face="Arial"
      size="40">
      </font>
    </resource>

  <!-- font used on journal summary labels (9pt) -->
  <resource
    id="fntjrnsumm">
    <font
      face="Arial"
      size="36">
      </font>
    </resource>

  <!-- font used on a small action portal/button (8pt) -->
  <resource
    id="fntactsml">
    <font
      face="Arial"
      size="32"
      style="bold">
      </font>
    </resource>

  <!-- font used on a big action portal/button (9pt) -->
  <resource
    id="fntactbig">
    <font
      face="Arial"
      size="36"
      style="bold">
      </font>
    </resource>

  <!-- font used on title labels placed under edit portals / droplists (8pt) -->
  <resource
    id="fntcfgttl">
    <font
      face="Arial"
      size="32">
      </font>
    </resource>

  <!-- font used for tiny text (8pt) -->
  <resource
    id="fnttiny">
    <font
      face="Arial"
      size="32">
      </font>
    </resource>

  <!-- font used on tactical console labels (8pt) -->
  <resource
    id="fnttacinfo">
    <font
      face="Arial"
      size="32">
      </font>
    </resource>

  <!-- font used for extra secondary info (9pt) -->
  <resource
    id="fntsecond">
    <font
      face="Arial"
      size="36">
      </font>
    </resource>

  <!-- font used on labels that only show encoded text bitmaps (9pt default)
        -NOTE! This is needed because inline bitmaps will be automatically scaled up
          or down when displayed at other default font sizes.
  -->
  <resource
    id="fntencoded">
    <font
      face="Arial"
      size="36"
      style="bold">
      </font>
    </resource>


<!-- ##### Fonts - Edit Portals ##### -->

  <!-- font used within edit portals -->
  <resource
    id="fntedit">
    <font
      face="Arial"
      size="36">
      </font>
    </resource>


<!-- ##### Fonts - Checkboxes ##### -->

  <!-- font used within checkboxes -->
  <resource
    id="fntcheck">
    <font
      face="Arial"
      size="40"
      style="bold">
      </font>
    </resource>


<!-- ##### Fonts - Incrementers ##### -->

  <!-- font used within simple incrementers -->
  <resource
    id="fntincrsim">
    <font
      face="Arial"
      size="46"
      style="bold">
      </font>
    </resource>

  <!-- font used within big incrementers -->
  <resource
    id="fntincrbig">
    <font
      face="Arial"
      size="100"
      style="bold">
      </font>
    </resource>

  <!-- font used within small incrementers -->
  <resource
    id="fntincrsml">
    <font
      face="Arial"
      size="44">
      </font>
    </resource>


<!-- ##### Fonts - Menus and Choosers ##### -->

  <!-- font used within menu and chooser portals -->
  <resource
    id="fntmenu">
    <font
      face="Arial"
      size="36"
      style="bold">
      </font>
    </resource>

  <!-- small font used within menu and chooser portals -->
  <resource
    id="fntmenusm">
    <font
      face="Arial"
      size="30"
      style="bold">
      </font>
    </resource>


<!--
        ##########  Other Resource Definitions  ##########
-->


  <!-- border used as a thick white outline around an area -->
  <resource
    id="brdthick"
    isbuiltin="yes">
    <border>
      <topleft bitmap="brd_thick_topleft.bmp"/>
      <topright bitmap="brd_thick_topright.bmp"/>
      <bottomleft bitmap="brd_thick_bottomleft.bmp"/>
      <bottomright bitmap="brd_thick_bottomright.bmp"/>
      <left bitmap="brd_thick_left.bmp"/>
      <top bitmap="brd_thick_top.bmp"/>
      <right bitmap="brd_thick_right.bmp"/>
      <bottom bitmap="brd_thick_bottom.bmp"/>
      </border>
    </resource>

  <!-- bitmap for use as the background of label portals designated as a "title" -->
  <resource
    id="titleback"
    isbuiltin="yes">
    <bitmap
      bitmap="mainwindow.bmp">
      </bitmap>
    </resource>


<!--
        ##########  General Color Definitions  ##########
-->


  <!-- color used to reset a "text" block within encoded text -->
  <resource
    id="clrreset">
    <color
      color="010101">
      </color>
    </resource>

  <!-- color used for normal text throughout the ui -->
  <resource
    id="clrnormal">
    <color
      color="f0f0f0">
      </color>
    </resource>

  <!-- color used for text on the static panel - not quite as bright -->
  <resource
    id="clrstatic">
    <color
      color="d2d2d2">
      </color>
    </resource>

  <!-- color used for text in title labels -->
  <resource
    id="clrtitle">
    <color
      color="c0c0c0">
      </color>
    </resource>

  <!-- color used for names of automatically added picks -->
  <resource
    id="clrauto">
    <color
      color="99efed">
      </color>
    </resource>

  <!-- color used for disabled text -->
  <resource
    id="clrdisable">
    <color
      color="808080">
      </color>
    </resource>

  <!-- color used for summary text that should be a little dimmer than normal -->
  <resource
    id="clrsummary">
    <color
      color="a0a0a0">
      </color>
    </resource>

  <!-- color used for greyed out text for "add" items that are non-applicable -->
  <resource
    id="clrgrey">
    <color
      color="a0a0a0">
      </color>
    </resource>

  <!-- color used for softly glowing text -->
  <resource
    id="clrglow">
    <color
      color="ffff80">
      </color>
    </resource>

  <!-- color used for bright text -->
  <resource
    id="clrbright">
    <color
      color="ffff88">
      </color>
    </resource>

  <!-- color used for warning text -->
  <resource
    id="clrwarning">
    <color
      color="ff0000">
      </color>
    </resource>

  <!-- color used for prompt text inviting the user to change something -->
  <resource
    id="clrprompt">
    <color
      color="ffff00">
      </color>
    </resource>

  <!-- color used for blocks of text on the tactical console -->
  <resource
    id="clrtacinfo">
    <color
      color="c0c0c0">
      </color>
    </resource>

  <!-- color used to identify enemy actors on the tactical console -->
  <resource
    id="clrenemy">
    <color
      color="ff8080">
      </color>
    </resource>

  <!-- color used to identify active actors on the tactical console -->
  <resource
    id="clractive">
    <color
      color="ffff80">
      </color>
    </resource>

  <!-- color used for secondary text within mouse-info text -->
  <resource
    id="clrsecond">
    <color
      color="555555">
      </color>
    </resource>

  <!-- color used for the 'buy for free' checkbox on buy / sell panels -->
  <resource
    id="clrchkfree">
    <color
      color="a8a800">
      </color>
    </resource>

  <!-- color used for text on summary panels - a little dimmer than normal -->
  <resource
    id="clrsummtxt">
    <color
      color="d0d0d0">
      </color>
    </resource>

  <!-- color used for labels when choosing advancements -->
  <resource
    id="clradvance">
    <color
      color="ffffff">
      </color>
    </resource>

  <!-- color used for text on action buttons -->
  <resource
    id="clraction">
    <color
      color="000088">
      </color>
    </resource>

  <!-- colors used in edit controls -->
  <resource
    id="clredittxt">
    <color
      color="d2d2d2">
      </color>
    </resource>
  <resource
    id="clreditbck">
    <color
      color="000000">
      </color>
    </resource>

  <!-- colors used in menu and chooser controls -->
  <resource
    id="clrmenutxt">
    <color
      color="84c8f7">
      </color>
    </resource>
  <resource
    id="clrmenuslt">
    <color
      color="1414f7">
      </color>
    </resource>
  <resource
    id="clrmenubck">
    <color
      color="2a2c47">
      </color>
    </resource>


<!--
        ##########  Style Definitions  ##########
-->

<!-- ##### Styles - System Defaults ##### -->

  <!-- label style used in table "add" items (via the "additem" script) -->
  <style
    id="additem">
    <style_label
      textcolorid="clrnormal"
      font="fntnormal"
      alignment="center">
      </style_label>
    </style>

  <!-- label style used in table "header" items (via the "headertitle" script) -->
  <style
    id="headerui"
    border="raised">
    <style_label
      textcolorid="clrtitle"
      font="fnttitle"
      background="titleback"
      alignment="center">
      </style_label>
    </style>

  <!-- label style used in table "header" items (via the "headertitle" script)
      displayed within summary panels -->
  <style
    id="headersumm">
    <style_label
      textcolorid="clrsummary"
      font="fntheader"
      alignment="center">
      </style_label>
    </style>


<!-- ##### Styles - Labels ##### -->

  <!-- style used for "title" labels, usually shown at the top of every tab and
        above other key sections
  -->
  <style
    id="lblTitle"
    border="raised">
    <style_label
      textcolorid="clrtitle"
      font="fnttitle"
      background="titleback"
      alignment="center">
      </style_label>
    </style>

  <!-- labels used on the static tab - not quite as bright -->
  <style
    id="lblStatic">
    <style_label
      textcolorid="clrstatic"
      font="fnttitle"
      alignment="center">
      </style_label>
    </style>

  <!-- generic 'normal' label used all over the place - center-aligned -->
  <style
    id="lblNormal">
    <style_label
      textcolorid="clrnormal"
      font="fntnormal"
      alignment="center">
      </style_label>
    </style>

  <!-- generic left-aligned 'normal' label -->
  <style
    id="lblLeft">
    <style_label
      textcolorid="clrnormal"
      font="fntnormal"
      alignment="left">
      </style_label>
    </style>

  <!-- generic left-aligned 'bordered' label -->
  <style
    id="lblBrdLeft"
    border="brdsystem">
    <style_label
      textcolorid="clrnormal"
      font="fntnormal"
      alignment="left">
      </style_label>
    </style>
	
  <!-- generic center-aligned 'bordered' label -->
  <style
    id="lblBrdCent"
    border="brdsystem">
    <style_label
      textcolorid="clrnormal"
      font="fntnormal"
      alignment="center">
      </style_label>
    </style>	
  <!-- generic right-aligned 'normal' label -->
  <style
    id="lblRight">
    <style_label
      textcolorid="clrnormal"
      font="fntnormal"
      alignment="right">
      </style_label>
    </style>

  <!-- generic 'automatically added' label
        -identifies auto-added picks in tables via a color shift
  -->
  <style
    id="lblAuto">
    <style_label
      textcolorid="clrauto"
      font="fntnormal"
      alignment="center">
      </style_label>
    </style>

  <!-- generic 'disabled' label
        -for showing choices that don't meet pre-reqs or are disabled in grey
  -->
  <style
    id="lblDisable">
    <style_label
      textcolorid="clrdisable"
      font="fntnormal"
      alignment="center">
      </style_label>
    </style>

  <!-- left-aligned 'disabled' label
        -for showing choices that don't meet pre-reqs or are disabled in grey
  -->
  <style
    id="lblDisableLeft">
    <style_label
      textcolorid="clrdisable"
      font="fntnormal"
      alignment="left">
      </style_label>
    </style>
	
  <!-- generic 'bright' label
        -for highlighting material to stand out
  -->
  <style
    id="lblBright">
    <style_label
      textcolorid="clrbright"
      font="fntnormal"
      alignment="center">
      </style_label>
    </style>

  <!-- generic 'warning' label
        -for identifying invalid selections and erroneous choices
  -->
  <style
    id="lblWarning">
    <style_label
      textcolorid="clrwarning"
      font="fntnormal"
      alignment="center">
      </style_label>
    </style>

  <!-- generic 'prompt' label
        -identifies choices that are valid but require user attention, such as wehn
          the user must make a selection and has not done so yet
  -->
  <style
    id="lblPrompt">
    <style_label
      textcolorid="clrprompt"
      font="fntnormal"
      alignment="center">
      </style_label>
    </style>

  <!-- slightly larger label in same color as the "normal" label -->
  <style
    id="lblLarge">
    <style_label
      textcolorid="clrnormal"
      font="fntlarge"
      alignment="center">
      </style_label>
    </style>

  <!-- slightly larger label in disabled color -->
  <style
    id="lblLrgDis">
    <style_label
      textcolorid="clrdisable"
      font="fntlarge"
      alignment="center">
      </style_label>
    </style>

  <!-- slightly larger label in brighter "prompt" color -->
  <style
    id="lblLrgProm">
    <style_label
      textcolorid="clrprompt"
      font="fntlarge"
      alignment="center">
      </style_label>
    </style>

  <!-- slightly larger label in warning color -->
  <style
    id="lblLrgWarn">
    <style_label
      textcolorid="clrwarning"
      font="fntlarge"
      alignment="center">
      </style_label>
    </style>

  <!-- much larger label in same color as the "normal" label -->
  <style
    id="lblXLarge">
    <style_label
      textcolorid="clrnormal"
      font="fntxlarge"
      alignment="center">
      </style_label>
    </style>

  <!-- much larger label in disabled color -->
  <style
    id="lblXLrgDis">
    <style_label
      textcolorid="clrdisable"
      font="fntxlarge"
      alignment="center">
      </style_label>
    </style>

  <!-- much larger label in warning color -->
  <style
    id="lblXLrgWrn">
    <style_label
      textcolorid="clrwarning"
      font="fntxlarge"
      alignment="center">
      </style_label>
    </style>

  <!-- slightly smaller label in same color as "normal" label -->
  <style
    id="lblSmall">
    <style_label
      textcolorid="clrnormal"
      font="fntsmall"
      alignment="center">
      </style_label>
    </style>

  <!-- slightly smaller label that is left-aligned -->
  <style
    id="lblSmlLeft">
    <style_label
      textcolorid="clrnormal"
      font="fntsmall"
      alignment="left">
      </style_label>
    </style>

  <!-- label style for secondary info such as on the Special tab -->
  <style
    id="lblSecond">
    <style_label
      textcolorid="clrdisable"
      font="fntsecond"
      alignment="left">
      </style_label>
    </style>

  <!-- label style for header text above tables -->
  <style
    id="lblHeader">
    <style_label
      textcolorid="clrsummary"
      font="fntheader"
      alignment="center">
      </style_label>
    </style>

  <!-- label style for tiny info on TacCon/Dashboard -->
  <style
    id="lblTinyTac">
    <style_label
      textcolorid="clrnormal"
      font="fnttiny"
      alignment="center">
      </style_label>
    </style>

  <!-- generic 'tactical console info' label -->
  <style
    id="lblTacInfo">
    <style_label
      textcolorid="clrnormal"
      font="fnttacinfo"
      alignment="left">
      </style_label>
    </style>

  <!-- generic 'summary' label for text on summary panels -->
  <style
    id="lblSummary">
    <style_label
      textcolorid="clrsummtxt"
      font="fntsummary"
      alignment="center">
      </style_label>
    </style>

  <!-- disabled version of the 'summary' label -->
  <style
    id="lblSummDis">
    <style_label
      textcolorid="clrdisable"
      font="fntsummary"
      alignment="center">
      </style_label>
    </style>

  <!-- left-aligned version of the 'summary' label -->
  <style
    id="lblSummLf">
    <style_label
      textcolorid="clrsummtxt"
      font="fntsummary"
      alignment="left">
      </style_label>
    </style>

  <!-- style for title labels placed under edit portals or droplists -->
  <style
    id="lblSmTitle">
    <style_label
      textcolorid="clrsummary"
      font="fntcfgttl"
      alignment="center">
      </style_label>
    </style>

  <!-- style for the journal summary label -->
  <style
    id="lblJrnSumm"
    border="sunken">
    <style_label
      textcolorid="clrsummary"
      font="fntjrnsumm"
      alignment="center">
      </style_label>
    </style>

  <!-- style for labels used with static, non-chooser advances -->
  <style
    id="lblAdvance"
    border="sunken">
    <style_label
      textcolorid="clradvance"
      font="fnttitle"
      background="titleback"
      alignment="center">
      </style_label>
    </style>

  <!-- style for smaller labels used with static, non-chooser advances -->
  <style
    id="lblAdvSml"
    border="sunken">
    <style_label
      textcolorid="clradvance"
      font="fntsmall"
      background="titleback"
      alignment="center">
      </style_label>
    </style>

  <!-- style for labels that only show encoded text bitmaps
        -NOTE! This is needed because inline bitmaps will be automatically scaled up
          or down when displayed at other default font sizes.
  -->
  <style
    id="lblEncoded">
    <style_label
      textcolorid="clrnormal"
      font="fntencoded"
      alignment="center">
      </style_label>
    </style>


<!-- ##### Styles - Edit Portals ##### -->

  <!-- basic text edit portals -->
  <style
    id="editNormal"
    border="sunken">
    <style_edit
      textcolorid="clredittxt"
      backcolorid="clreditbck"
      font="fntedit">
      </style_edit>
    </style>

  <!-- edit portals that have the text centered within the portal -->
  <style
    id="editCenter"
    border="sunken">
    <style_edit
      textcolorid="clredittxt"
      backcolorid="clreditbck"
      font="fntedit"
      alignment="center">
      </style_edit>
    </style>

  <!-- edit portals that manage dates -->
  <style
    id="editDate">
    <style_edit
      textcolorid="clredittxt"
      backcolorid="clreditbck"
      font="fntedit"
      alignment="center"
      itemborder="sunken"
      septext="/"
      sepcolorid="clrdisable">
      </style_edit>
    </style>


<!-- ##### Styles - Incrementers ##### -->

  <!-- style used on 'simple incrementers', using the builtin system incrementer look -->
  <style
    id="incrSimple">
    <style_incrementer
      textcolorid="clrnormal"
      font="fntincrsim"
      textleft="13" texttop="0" textwidth="24" textheight="20"
      fullwidth="50" fullheight="20"
      plusup="incplusup" plusdown="incplusdn" plusoff="incplusof"
      plusx="39" plusy="0"
      minusup="incminusup" minusdown="incminusdn" minusoff="incminusof"
      minusx="0" minusy="0">
      </style_incrementer>
    </style>

  <!-- style used on 'narrow incrementers'
        -identical to the "simple" incrementer, except a bit narrower in width
  -->
  <style
    id="incrNarrow">
    <style_incrementer
      textcolorid="clrnormal"
      font="fntincrsml"
      textleft="13" texttop="0" textwidth="9" textheight="20"
      fullwidth="35" fullheight="20"
      plusup="incplusup" plusdown="incplusdn" plusoff="incplusof"
      plusx="24" plusy="0"
      minusup="incminusup" minusdown="incminusdn" minusoff="incminusof"
      minusx="0" minusy="0">
      </style_incrementer>
    </style>

  <!-- style used on 'big incrementers', with a text area and up/down buttons stacked
        on the right
  -->
  <style
    id="incrBig">
    <style_incrementer
      textcolorid="clrnormal"
      font="fntincrbig"
      textleft="3" texttop="0" textwidth="72" textheight="40"
      fullwidth="100" fullheight="40"
      plusup="incbigplup" plusdown="incbigpldn" plusoff="incbigplof"
      plusx="78" plusy="0"
      minusup="incbigmnup" minusdown="incbigmndn" minusoff="incbigmnof"
      minusx="78" minusy="20">
      </style_incrementer>
    <resource
      id="incbigplup"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_big_plus.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incbigpldn"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_big_plus_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incbigplof"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_big_plus_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incbigmnup"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_big_minus.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incbigmndn"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_big_minus_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incbigmnof"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_big_minus_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on 'box incrementers', with the value set off within a box -->
  <style
    id="incrBox">
    <style_incrementer
      textcolorid="clrnormal"
      font="fntincrsml"
      textleft="14" texttop="4" textwidth="42" textheight="18"
      background="incboxbk"
      plusup="incboxplup" plusdown="incboxpldn" plusoff="incboxplof"
      plusx="57" plusy="2"
      minusup="incboxmnup" minusdown="incboxmndn" minusoff="incboxmnof"
      minusx="2" minusy="2">
      </style_incrementer>
    <resource
      id="incboxbk"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_box_back.bmp">
        </bitmap>
      </resource>
    <resource
      id="incboxplup"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_box_plus.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incboxpldn"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_box_plus_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incboxplof"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_box_plus_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incboxmnup"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_box_minus.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incboxmndn"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_box_minus_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="incboxmnof"
      isbuiltin="yes">
      <bitmap
        bitmap="incr_box_minus_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>


<!-- ##### Styles - Action Portals/Buttons ##### -->

  <!-- style used on big action buttons -->
  <style
    id="actBig">
    <style_action
      textcolorid="clraction"
      font="fntactbig"
      up="actbigup" down="actbigdn" off="actbigof">
      </style_action>
    <resource
      id="actbigup"
      isbuiltin="yes">
      <bitmap
        bitmap="button_big_up.bmp">
        </bitmap>
      </resource>
    <resource
      id="actbigdn"
      isbuiltin="yes">
      <bitmap
        bitmap="button_big_down.bmp">
        </bitmap>
      </resource>
    <resource
      id="actbigof"
      isbuiltin="yes">
      <bitmap
        bitmap="button_big_off.bmp">
        </bitmap>
      </resource>
    </style>

  <!-- style used on small action buttons -->
  <style
    id="actSmall">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actsmallup" down="actsmalldn" off="actsmallof">
      </style_action>
    <resource
      id="actsmallup"
      isbuiltin="yes">
      <bitmap
        bitmap="button_small_up.bmp">
        </bitmap>
      </resource>
    <resource
      id="actsmalldn"
      isbuiltin="yes">
      <bitmap
        bitmap="button_small_down.bmp">
        </bitmap>
      </resource>
    <resource
      id="actsmallof"
      isbuiltin="yes">
      <bitmap
        bitmap="button_small_off.bmp">
        </bitmap>
      </resource>
    </style>

  <!-- style used on small oval action buttons -->
  <style
    id="actOvalSm">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actovalup" down="actovaldn" off="actovalof">
      </style_action>
    <resource
      id="actovalup"
      isbuiltin="yes">
      <bitmap
        bitmap="oval_small.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actovaldn"
      isbuiltin="yes">
      <bitmap
        bitmap="oval_small_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actovalof"
      isbuiltin="yes">
      <bitmap
        bitmap="oval_small.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on delete actions - bitmap-only with no text -->
  <style
    id="actDelete">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="deletesmup" down="deletesmdn" off="deletesmof">
      </style_action>
    </style>

  <!-- style used on disabled delete action buttons - bitmap-only with no text
        -these are used when a condition forbids the deletion of a pick
  -->
  <style
    id="actNoDel">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actnodelup" down="actnodeldn" off="actnodelof">
      </style_action>
    <resource
      id="actnodelup"
      isbuiltin="yes">
      <bitmap
        bitmap="forbidden_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actnodeldn"
      isbuiltin="yes">
      <bitmap
        bitmap="forbidden_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actnodelof"
      isbuiltin="yes">
      <bitmap
        bitmap="forbidden_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on info actions - bitmap-only with no text -->
  <style
    id="actInfo">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="infoup" down="infodn" off="infoof">
      </style_action>
    </style>

  <!-- style used on info actions that look disabled - bitmap-only with no text -->
  <style
    id="actInfoOff">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="infoof" down="infoof" off="infoof">
      </style_action>
    </style>

  <!-- style used on small info action buttons - bitmap-only with no text -->
  <style
    id="actInfoSm">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actinfoup" down="actinfodn" off="actinfoof">
      </style_action>
    <resource
      id="actinfoup"
      isbuiltin="yes">
      <bitmap
        bitmap="info_small_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actinfodn"
      isbuiltin="yes">
      <bitmap
        bitmap="info_small_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actinfoof"
      isbuiltin="yes">
      <bitmap
        bitmap="info_small_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the edit action button - bitmap-only with no text -->
  <style
    id="actEdit">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="editup" down="editdn" off="editof">
      </style_action>
    </style>

  <!-- style used on the gear action button - bitmap-only with no text -->
  <style
    id="actGear">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actgearup" down="actgeardn" off="actgearup">
      </style_action>
    <resource
      id="actgearup"
      isbuiltin="yes">
      <bitmap
        bitmap="gear_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actgeardn"
      isbuiltin="yes">
      <bitmap
        bitmap="gear_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the notes button - bitmap-only with no text -->
  <style
    id="actNotes">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actnotesup" down="actnotesdn" off="actnotesof">
      </style_action>
    <resource
      id="actnotesup"
      isbuiltin="yes">
      <bitmap
        bitmap="notes_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actnotesdn"
      isbuiltin="yes">
      <bitmap
        bitmap="notes_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actnotesof"
      isbuiltin="yes">
      <bitmap
        bitmap="notes_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the edit name button - bitmap-only with no text -->
  <style
    id="actName">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actnameup" down="actnamedn" off="actnameof">
      </style_action>
    <resource
      id="actnameup"
      isbuiltin="yes">
      <bitmap
        bitmap="name_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actnamedn"
      isbuiltin="yes">
      <bitmap
        bitmap="name_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actnameof"
      isbuiltin="yes">
      <bitmap
        bitmap="name_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the lock button for transitioning to advancement mode - bitmap-only with no text -->
  <style
    id="actLock">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actlockup" down="actlockdn" off="actlockup">
      </style_action>
    <resource
      id="actlockup"
      isbuiltin="yes">
      <bitmap
        bitmap="lock_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actlockdn"
      isbuiltin="yes">
      <bitmap
        bitmap="lock_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the unlock button for transitioning from advancement mode - bitmap-only with no text -->
  <style
    id="actUnlock">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actunlokup" down="actunlokdn" off="actunlokup">
      </style_action>
    <resource
      id="actunlokup"
      isbuiltin="yes">
      <bitmap
        bitmap="unlock_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actunlokdn"
      isbuiltin="yes">
      <bitmap
        bitmap="unlock_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the damage button - bitmap-only with no text -->
  <style
    id="actDamage">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actdmgup" down="actdmgdn" off="actdmgof">
      </style_action>
    <resource
      id="actdmgup"
      isbuiltin="yes">
      <bitmap
        bitmap="damage_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actdmgdn"
      isbuiltin="yes">
      <bitmap
        bitmap="damage_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actdmgof"
      isbuiltin="yes">
      <bitmap
        bitmap="damage_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- slightly greyer version of the damage button for the TacCon - bitmap-only with no text -->
  <style
    id="actDmgGrey">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actdmgtcup" down="actdmgtcdn" off="actdmgtcup">
      </style_action>
    <resource
      id="actdmgtcup"
      isbuiltin="yes">
      <bitmap
        bitmap="damage_grey_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actdmgtcdn"
      isbuiltin="yes">
      <bitmap
        bitmap="damage_grey_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the heal button - bitmap-only with no text -->
  <style
    id="actHeal">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="acthealup" down="acthealdn" off="acthealof">
      </style_action>
    <resource
      id="acthealup"
      isbuiltin="yes">
      <bitmap
        bitmap="heal_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="acthealdn"
      isbuiltin="yes">
      <bitmap
        bitmap="heal_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="acthealof"
      isbuiltin="yes">
      <bitmap
        bitmap="heal_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the undo button - bitmap-only with no text -->
  <style
    id="actUndo">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actundoup" down="actundodn" off="actundoof">
      </style_action>
    <resource
      id="actundoup"
      isbuiltin="yes">
      <bitmap
        bitmap="undo_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actundodn"
      isbuiltin="yes">
      <bitmap
        bitmap="undo_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actundoof"
      isbuiltin="yes">
      <bitmap
        bitmap="undo_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the reset button - bitmap-only with no text -->
  <style
    id="actReset">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actresetup" down="actresetdn" off="actresetof">
      </style_action>
    <resource
      id="actresetup"
      isbuiltin="yes">
      <bitmap
        bitmap="reset_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actresetdn"
      isbuiltin="yes">
      <bitmap
        bitmap="reset_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actresetof"
      isbuiltin="yes">
      <bitmap
        bitmap="reset_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the reroll button - bitmap-only with no text -->
  <style
    id="actReroll">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actrerlup" down="actrerldn" off="actrerlof">
      </style_action>
    <resource
      id="actrerlup"
      isbuiltin="yes">
      <bitmap
        bitmap="reroll_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actrerldn"
      isbuiltin="yes">
      <bitmap
        bitmap="reroll_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actrerlof"
      isbuiltin="yes">
      <bitmap
        bitmap="reroll_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the small notes button - bitmap-only with no text -->
  <style
    id="actNotesSm">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actnote2up" down="actnote2dn" off="actnote2up">
      </style_action>
    <resource
      id="actnote2up"
      isbuiltin="yes">
      <bitmap
        bitmap="notes_small_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actnote2dn"
      isbuiltin="yes">
      <bitmap
        bitmap="notes_small_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the star button - bitmap-only with no text -->
  <style
    id="actStar">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actstarup" down="actstardn" off="actstarup">
      </style_action>
    <resource
      id="actstarup"
      isbuiltin="yes">
      <bitmap
        bitmap="star_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actstardn"
      isbuiltin="yes">
      <bitmap
        bitmap="star_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the double-up button - bitmap-only with no text -->
  <style
    id="actDblUp">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actdblupup" down="actdblupdn" off="actdblupup">
      </style_action>
    <resource
      id="actdblupup"
      isbuiltin="yes">
      <bitmap
        bitmap="doubleup_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actdblupdn"
      isbuiltin="yes">
      <bitmap
        bitmap="doubleup_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the swap button - bitmap-only with no text -->
  <style
    id="actSwap">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actswapup" down="actswapdn" off="actswapof">
      </style_action>
    <resource
      id="actswapup"
      isbuiltin="yes">
      <bitmap
        bitmap="swap_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actswapdn"
      isbuiltin="yes">
      <bitmap
        bitmap="swap_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actswapof"
      isbuiltin="yes">
      <bitmap
        bitmap="swap_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the dice button -->
  <style
    id="actDice">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actdiceup" down="actdicedn" off="actdiceup">
      </style_action>
    <resource
      id="actdiceup"
      isbuiltin="yes">
      <bitmap
        bitmap="dice_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actdicedn"
      isbuiltin="yes">
      <bitmap
        bitmap="dice_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- Style used on the master button -->
  <style
    id="actMaster">
    <style_action
      textcolorid="clrnormal"
      font="fntactsml"
      up="actmastup" down="actmastdn" off="actmastup">
      </style_action>
    <resource
      id="actmastup"
      isbuiltin="yes">
      <bitmap
        bitmap="master_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actmastdn"
      isbuiltin="yes">
      <bitmap
        bitmap="master_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the "get gear" action button in the dashboard - bitmap-only with no text -->
  <style
    id="actGetGear">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actgetgrup" down="actgetgrdn" off="actgetgrup">
      </style_action>
    <resource
      id="actgetgrup"
      isbuiltin="yes">
      <bitmap
        bitmap="getgear_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actgetgrdn"
      isbuiltin="yes">
      <bitmap
        bitmap="getgear_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on load actions - bitmap-only with no text -->
  <style
    id="actLoad">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actloadup" down="actloaddn" off="actloadof">
      </style_action>
    <resource
      id="actloadup"
      isbuiltin="yes">
      <bitmap
        bitmap="load_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actloaddn"
      isbuiltin="yes">
      <bitmap
        bitmap="load_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actloadof"
      isbuiltin="yes">
      <bitmap
        bitmap="load_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the sort action button at the top of the dashboard - bitmap-only with no text -->
  <style
    id="actSort">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actsortup" down="actsortdn" off="actsortup">
      </style_action>
    <resource
      id="actsortup"
      isbuiltin="yes">
      <bitmap
        bitmap="sort_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actsortdn"
      isbuiltin="yes">
      <bitmap
        bitmap="sort_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the disable combat action button - bitmap-only with no text -->
  <style
    id="actDisable">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actdisabup" down="actdisabdn" off="actdisabup">
      </style_action>
    <resource
      id="actdisabup"
      isbuiltin="yes">
      <bitmap
        bitmap="disable_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actdisabdn"
      isbuiltin="yes">
      <bitmap
        bitmap="disable_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the enable combat action button - bitmap-only with no text -->
  <style
    id="actEnable">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actenabup" down="actenabdn" off="actenabup">
      </style_action>
    <resource
      id="actenabup"
      isbuiltin="yes">
      <bitmap
        bitmap="enable_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actenabdn"
      isbuiltin="yes">
      <bitmap
        bitmap="enable_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on act actions - bitmap-only with no text -->
  <style
    id="actActNow">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actnowup" down="actnowdn" off="actnowof">
      </style_action>
    <resource
      id="actnowup"
      isbuiltin="yes">
      <bitmap
        bitmap="action_act_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actnowdn"
      isbuiltin="yes">
      <bitmap
        bitmap="action_act_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actnowof"
      isbuiltin="yes">
      <bitmap
        bitmap="action_act_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on delay actions - bitmap-only with no text -->
  <style
    id="actDelay">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actdelayup" down="actdelaydn" off="actdelayof">
      </style_action>
    <resource
      id="actdelayup"
      isbuiltin="yes">
      <bitmap
        bitmap="action_delay_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actdelaydn"
      isbuiltin="yes">
      <bitmap
        bitmap="action_delay_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actdelayof"
      isbuiltin="yes">
      <bitmap
        bitmap="action_delay_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on abandon actions - bitmap-only with no text -->
  <style
    id="actAband">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actabandup" down="actabanddn" off="actabandof">
      </style_action>
    <resource
      id="actabandup"
      isbuiltin="yes">
      <bitmap
        bitmap="action_abandon_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actabanddn"
      isbuiltin="yes">
      <bitmap
        bitmap="action_abandon_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actabandof"
      isbuiltin="yes">
      <bitmap
        bitmap="action_abandon_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on active act actions - bitmap-only with no text -->
  <style
    id="actActNwAc">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="nowactup" down="nowactdn" off="nowactof">
      </style_action>
    <resource
      id="nowactup"
      isbuiltin="yes">
      <bitmap
        bitmap="action_act_active_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="nowactdn"
      isbuiltin="yes">
      <bitmap
        bitmap="action_act_active_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="nowactof"
      isbuiltin="yes">
      <bitmap
        bitmap="action_act_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on active delay actions - bitmap-only with no text -->
  <style
    id="actDelayAc">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="nowdelayup" down="nowdelaydn" off="nowdelayof">
      </style_action>
    <resource
      id="nowdelayup"
      isbuiltin="yes">
      <bitmap
        bitmap="action_delay_active_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="nowdelaydn"
      isbuiltin="yes">
      <bitmap
        bitmap="action_delay_active_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="nowdelayof"
      isbuiltin="yes">
      <bitmap
        bitmap="action_delay_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on active abandon actions - bitmap-only with no text -->
  <style
    id="actAbandAc">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="nowabandup" down="nowabanddn" off="nowabandof">
      </style_action>
    <resource
      id="nowabandup"
      isbuiltin="yes">
      <bitmap
        bitmap="action_abandon_active_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="nowabanddn"
      isbuiltin="yes">
      <bitmap
        bitmap="action_abandon_active_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="nowabandof"
      isbuiltin="yes">
      <bitmap
        bitmap="action_abandon_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on move to top actions - bitmap-only with no text -->
  <style
    id="actMoveTop">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actmvtopup" down="actmvtopdn" off="actmvtopof">
      </style_action>
    <resource
      id="actmvtopup"
      isbuiltin="yes">
      <bitmap
        bitmap="move_top_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actmvtopdn"
      isbuiltin="yes">
      <bitmap
        bitmap="move_top_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actmvtopof"
      isbuiltin="yes">
      <bitmap
        bitmap="move_top_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on move up actions - bitmap-only with no text -->
  <style
    id="actMoveUp">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actmvupup" down="actmvupdn" off="actmvupof">
      </style_action>
    <resource
      id="actmvupup"
      isbuiltin="yes">
      <bitmap
        bitmap="move_up_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actmvupdn"
      isbuiltin="yes">
      <bitmap
        bitmap="move_up_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actmvupof"
      isbuiltin="yes">
      <bitmap
        bitmap="move_up_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on move down actions - bitmap-only with no text -->
  <style
    id="actMoveDn">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actmvdnup" down="actmvdndn" off="actmvdnof">
      </style_action>
    <resource
      id="actmvdnup"
      isbuiltin="yes">
      <bitmap
        bitmap="move_down_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actmvdndn"
      isbuiltin="yes">
      <bitmap
        bitmap="move_down_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actmvdnof"
      isbuiltin="yes">
      <bitmap
        bitmap="move_down_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on move to bottom actions - bitmap-only with no text -->
  <style
    id="actMoveBot">
    <style_action
      textcolorid="clraction"
      font="fntactsml"
      up="actmvbtmup" down="actmvbtmdn" off="actmvbtmof">
      </style_action>
    <resource
      id="actmvbtmup"
      isbuiltin="yes">
      <bitmap
        bitmap="move_bottom_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actmvbtmdn"
      isbuiltin="yes">
      <bitmap
        bitmap="move_bottom_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="actmvbtmof"
      isbuiltin="yes">
      <bitmap
        bitmap="move_bottom_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the next button on the tactical console - bitmap-only with no text -->
  <style
    id="actTacNext">
    <style_action
      textcolorid="clrnormal"
      font="fnttiny"
      up="acttcnxtup" down="acttcnxtdn" off="acttcnxtof">
      </style_action>
    <resource
      id="acttcnxtup"
      isbuiltin="yes">
      <bitmap
        bitmap="tactical_next_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="acttcnxtdn"
      isbuiltin="yes">
      <bitmap
        bitmap="tactical_next_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="acttcnxtof"
      isbuiltin="yes">
      <bitmap
        bitmap="tactical_next_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the edit button on the tactical console - bitmap-only with no text -->
  <style
    id="actTacEdit">
    <style_action
      textcolorid="clrnormal"
      font="fnttiny"
      up="acttcedtup" down="acttcedtdn" off="acttcedtof">
      </style_action>
    <resource
      id="acttcedtup"
      isbuiltin="yes">
      <bitmap
        bitmap="tactical_edit_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="acttcedtdn"
      isbuiltin="yes">
      <bitmap
        bitmap="tactical_edit_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="acttcedtof"
      isbuiltin="yes">
      <bitmap
        bitmap="tactical_edit_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the "special" info button - bitmap-only with no text -->
  <style
    id="actInfoSpc">
    <style_action
      textcolorid="clrnormal"
      font="fnttiny"
      up="infspecup" down="infspecdn" off="infspecof">
      </style_action>
    <resource
      id="infspecup"
      isbuiltin="yes">
      <bitmap
        bitmap="info_special_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infspecdn"
      isbuiltin="yes">
      <bitmap
        bitmap="info_special_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infspecof"
      isbuiltin="yes">
      <bitmap
        bitmap="info_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the "rolls" info button - bitmap-only with no text -->
  <style
    id="actInfoRol">
    <style_action
      textcolorid="clrnormal"
      font="fnttiny"
      up="infrollup" down="infrolldn" off="infrollof">
      </style_action>
    <resource
      id="infrollup"
      isbuiltin="yes">
      <bitmap
        bitmap="info_rolls_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infrolldn"
      isbuiltin="yes">
      <bitmap
        bitmap="info_rolls_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infrollof"
      isbuiltin="yes">
      <bitmap
        bitmap="info_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the "combat" info button - bitmap-only with no text -->
  <style
    id="actInfoCom">
    <style_action
      textcolorid="clrnormal"
      font="fnttiny"
      up="infcombup" down="infcombdn" off="infcombof">
      </style_action>
    <resource
      id="infcombup"
      isbuiltin="yes">
      <bitmap
        bitmap="info_combat_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infcombdn"
      isbuiltin="yes">
      <bitmap
        bitmap="info_combat_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infcombof"
      isbuiltin="yes">
      <bitmap
        bitmap="info_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the "basics" info button - bitmap-only with no text -->
  <style
    id="actInfoBas">
    <style_action
      textcolorid="clrnormal"
      font="fnttiny"
      up="infbasicup" down="infbasicdn" off="infbasicof">
      </style_action>
    <resource
      id="infbasicup"
      isbuiltin="yes">
      <bitmap
        bitmap="info_basics_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infbasicdn"
      isbuiltin="yes">
      <bitmap
        bitmap="info_basics_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infbasicof"
      isbuiltin="yes">
      <bitmap
        bitmap="info_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>

  <!-- style used on the "active" info button - bitmap-only with no text -->
  <style
    id="actInfoAct">
    <style_action
      textcolorid="clrnormal"
      font="fnttiny"
      up="infactivup" down="infactivdn" off="infactivof">
      </style_action>
    <resource
      id="infactivup"
      isbuiltin="yes">
      <bitmap
        bitmap="info_active_up.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infactivdn"
      isbuiltin="yes">
      <bitmap
        bitmap="info_active_down.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    <resource
      id="infactivof"
      isbuiltin="yes">
      <bitmap
        bitmap="info_off.bmp"
        istransparent="yes">
        </bitmap>
      </resource>
    </style>


<!-- ##### Styles - Tables ##### -->

  <!-- style used on normal tables -->
  <style
    id="tblNormal">
    <style_table
      itemborder="sunken">
      </style_table>
    </style>

  <!-- style used on "invisible" tables with no cell borders -->
  <style
    id="tblInvis">
    <style_table>
      </style_table>
    </style>

  <!-- style used on invisible tables with light grid lines -->
  <style
    id="tblGridLt">
    <style_table
      showgridhorz="yes"
      gridcolorid="clrdisable">
      </style_table>
    </style>

  <!-- style used on tables with only an outer border -->
  <style
    id="tblOuter"
    border="brdsystem">
    <style_table>
      </style_table>
    </style>

  <!-- style used on SMALL invisible tables, such as on summary panels -->
  <style
    id="tblInvisSm">
    <style_table>
      </style_table>
    </style>


<!-- ##### Styles - Checkboxes ##### -->

  <!-- style used on the standard checkbox -->
  <style
    id="chkNormal">
    <style_checkbox
      textcolorid="clrnormal"
      font="fntcheck">
      </style_checkbox>
    </style>

  <!-- style used on checkboxes in a warning color, such as when the state is invalid -->
  <style
    id="chkWarning">
    <style_checkbox
      textcolorid="clrwarning"
      font="fntcheck">
      </style_checkbox>
    </style>

  <!-- style used on checkboxes in a disabled color, such as when the box
        should not be checked
  -->
  <style
    id="chkDisable">
    <style_checkbox
      textcolorid="clrdisable"
      font="fntcheck">
      </style_checkbox>
    </style>

  <!-- style used on the checkbox for "free" gear when purchasing equipment -->
  <style
    id="chkFree">
    <style_checkbox
      textcolorid="clrchkfree"
      font="fntcheck">
      </style_checkbox>
    </style>

  <!-- checkbox style for promoting selections to the top of the list -->
  <style
    id="chkPromote">
    <style_checkbox
      textcolorid="clrnormal"
      font="fntcheck"
      check="promochk"
      checkoff="promochkof"
      uncheck="promounc"
      uncheckoff="promouncof">
      </style_checkbox>
    <resource
      id="promounc"
      isbuiltin="yes">
      <bitmap
        bitmap="check_promote_up_up.bmp">
        </bitmap>
      </resource>
    <resource
      id="promouncof"
      isbuiltin="yes">
      <bitmap
        bitmap="check_promote_up_off.bmp">
        </bitmap>
      </resource>
    <resource
      id="promochk"
      isbuiltin="yes">
      <bitmap
        bitmap="check_promote_down_up.bmp">
        </bitmap>
      </resource>
    <resource
      id="promochkof"
      isbuiltin="yes">
      <bitmap
        bitmap="check_promote_down_off.bmp">
        </bitmap>
      </resource>
    </style>

  <!-- checkbox style for controlling whether something is printed -->
  <style
    id="chkPrint">
    <style_checkbox
      textcolorid="clrnormal"
      font="fntcheck"
      check="printchk"
      checkoff="printchk"
      uncheck="printunchk"
      uncheckoff="printunchk">
      </style_checkbox>
    <resource
      id="printchk"
      isbuiltin="yes">
      <bitmap
        bitmap="check_print_on.bmp">
        </bitmap>
      </resource>
    <resource
      id="printunchk"
      isbuiltin="yes">
      <bitmap
        bitmap="check_print_off.bmp">
        </bitmap>
      </resource>
    </style>


<!-- ##### Styles - Image Portals ##### -->

  <!-- standard image portal without a border -->
  <style
    id="imgNormal">
    <style_image/>
    </style>

  <!-- standard image portal WITH a border -->
  <style
    id="imgBorder"
    border="brdsystem">
    <style_image/>
    </style>


<!-- ##### Styles - Menus and Choosers ##### -->

  <!-- standard menu portal -->
  <style
    id="menuNormal"
    border="sunken">
    <style_menu
      textcolorid="clrmenutxt"
      backcolorid="clrmenubck"
      selecttextid="clrmenuslt"
      selectbackid="clrnormal"
      font="fntmenu">
      </style_menu>
    </style>

  <!-- standard menu portal with coloring to indicate contents are in error -->
  <style
    id="menuError"
    border="sunken">
    <style_menu
      textcolorid="clrmenutxt"
      backcolorid="clrmenubck"
      selecttextid="clrmenuslt"
      selectbackid="clrnormal"
      activetextid="clrwarning"
      font="fntmenu">
      </style_menu>
    </style>

  <!-- small menu portal -->
  <style
    id="menuSmall"
    border="sunken">
    <style_menu
      textcolorid="clrmenutxt"
      backcolorid="clrmenubck"
      selecttextid="clrmenuslt"
      selectbackid="clrnormal"
      font="fntmenusm"
      droplist="mnusmall"
      droplistoff="mnusmallof">
      </style_menu>
    </style>

  <!-- small menu portal with coloring to indicate contents are in error -->
  <style
    id="menuErrSm"
    border="sunken">
    <style_menu
      textcolorid="clrmenutxt"
      backcolorid="clrmenubck"
      selecttextid="clrmenuslt"
      selectbackid="clrnormal"
      activetextid="clrwarning"
      font="fntmenusm"
      droplist="mnusmall"
      droplistoff="mnusmallof">
      </style_menu>
    </style>

  <!-- standard chooser portal -->
  <style
    id="chsNormal"
    border="sunken">
    <style_chooser
      font="fntmenu"
      textcolorid="clrmenutxt"
      backcolorid="clrmenubck">
      </style_chooser>
    </style>

  <!-- standard chooser portal with coloring to indicate contents are in error -->
  <style
    id="chsError"
    border="sunken">
    <style_chooser
      font="fntmenu"
      textcolorid="clrwarning"
      backcolorid="clrmenubck">
      </style_chooser>
    </style>


<!-- ##### Styles - Separator ##### -->

  <!-- style used on horizontal separators -->
  <style
    id="sepHorz">
    <style_separator
      isvertical="no"
      start="sephorzsta" center="sephorzmid" end="sephorzend">
      </style_separator>
    <resource
      id="sephorzsta"
      isbuiltin="yes">
      <bitmap
        bitmap="sep_horz_start.bmp">
        </bitmap>
      </resource>
    <resource
      id="sephorzmid"
      isbuiltin="yes">
      <bitmap
        bitmap="sep_horz_middle.bmp">
        </bitmap>
      </resource>
    <resource
      id="sephorzend"
      isbuiltin="yes">
      <bitmap
        bitmap="sep_horz_end.bmp">
        </bitmap>
      </resource>
    </style>

  <!-- style used on vertical separators -->
  <style
    id="sepVert">
    <style_separator
      isvertical="yes"
      start="sepvertsta" center="sepvertmid" end="sepvertend">
      </style_separator>
    <resource
      id="sepvertsta"
      isbuiltin="yes">
      <bitmap
        bitmap="sep_vert_start.bmp">
        </bitmap>
      </resource>
    <resource
      id="sepvertmid"
      isbuiltin="yes">
      <bitmap
        bitmap="sep_vert_middle.bmp">
        </bitmap>
      </resource>
    <resource
      id="sepvertend"
      isbuiltin="yes">
      <bitmap
        bitmap="sep_vert_end.bmp">
        </bitmap>
      </resource>
    </style>


<!-- ##### Styles - Other ##### -->

  <!-- style used on regions with a light border -->
  <style
    id="rgnBorder"
    border="brdsystem">
    <style_region>
      </style_region>
    </style>


  </document>
