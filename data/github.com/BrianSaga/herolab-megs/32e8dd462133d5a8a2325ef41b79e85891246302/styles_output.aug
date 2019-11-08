<?xml version="1.0" encoding="ISO-8859-1"?>

<!-- This file contains definitions of all custom resources and styles used for
      displaying visual elements within printed output for Hero Lab. Visual elements
      utilized within the user-interface are defined within "styles_ui.aug". All sytem
      resources replaced for the game system are defined within "system_resources.aug".
-->

<document signature="Hero Lab Structure">

<!-- ##### Output Resources ##### -->

  <!-- font used on normal text -->
  <resource
    id="ofntnormal">
    <font
      face="Arial"
      size="40"
      style="bold">
      </font>
    </resource>

  <!-- font used on medium text -->
  <resource
    id="ofntmedium">
    <font
      face="Arial"
      size="44"
      style="bold">
      </font>
    </resource>

  <!-- font used on larger text -->
  <resource
    id="ofntlarge">
    <font
      face="Arial"
      size="48"
      style="bold">
      </font>
    </resource>

  <!-- font used on smaller text -->
  <resource
    id="ofntsmall">
    <font
      face="Arial"
      size="36"
      style="bold">
      </font>
    </resource>

  <!-- font used on title text -->
  <resource
    id="ofnttitle">
    <font
      face="Arial"
      size="50"
      style="bold">
      </font>
    </resource>

  <!-- font used on plain text -->
  <resource
    id="ofntplain">
    <font
      face="Arial"
      size="40">
      </font>
    </resource>

  <!-- font used on header text -->
  <resource
    id="ofntheader">
    <font
      face="Arial"
      size="36">
      </font>
    </resource>

  <!-- font used within validation report -->
  <resource
    id="ofntvalid">
    <font
      face="Arial"
      size="36"
      style="bold">
      </font>
    </resource>


<!-- ##### Output Styles ##### -->

  <!-- replace the default style for titles with our own for output -->
  <style
    id="headerout">
    <style_output
      textcolor="222222"
      backcolor="dddddd"
      font="ofnttitle"
      alignment="center">
      </style_output>
    </style>

  <!-- simple text for output -->
  <style
    id="outNormal">
    <style_output
      textcolor="000000"
      font="ofntnormal"
      alignment="center">
      </style_output>
    </style>

  <!-- title text for output -->
  <style
    id="outTitle">
    <style_output
      textcolor="222222"
      backcolor="dddddd"
      font="ofnttitle"
      alignment="center">
      </style_output>
    </style>

  <!-- hero name text for output -->
  <style
    id="outHeroNam">
    <style_output
      textcolor="000000"
      backcolor="dddddd"
      font="ofntlarge"
      alignment="center">
      </style_output>
    </style>

  <!-- solid border for use around character images -->
  <style
    id="outCharImg"
    border="solid">
    <style_output
      textcolor="000000"
      font="ofntnormal">
      </style_output>
    </style>

  <!-- large name for output -->
  <style
    id="outNameLg">
    <style_output
      textcolor="000000"
      font="ofntlarge"
      alignment="left">
      </style_output>
    </style>

  <!-- large value for output -->
  <style
    id="outValueLg">
    <style_output
      textcolor="000000"
      font="ofntlarge"
      alignment="right">
      </style_output>
    </style>

  <!-- value-aligned for output -->
  <style
    id="outNormRt">
    <style_output
      textcolor="000000"
      font="ofntnormal"
      alignment="right">
      </style_output>
    </style>

  <!-- left-aligned for output -->
  <style
    id="outNormLt">
    <style_output
      textcolor="000000"
      font="ofntnormal"
      alignment="left">
      </style_output>
    </style>

  <!-- medium name for output -->
  <style
    id="outNameMed">
    <style_output
      textcolor="000000"
      font="ofntmedium"
      alignment="center">
      </style_output>
    </style>

  <!-- small name for output -->
  <style
    id="outNameSm">
    <style_output
      textcolor="000000"
      font="ofntsmall"
      alignment="left">
      </style_output>
    </style>

  <!-- plain text for output -->
  <style
    id="outPlain">
    <style_output
      textcolor="000000"
      font="ofntplain"
      alignment="center">
      </style_output>
    </style>

  <!-- plain text left-aligned for output -->
  <style
    id="outPlainLt">
    <style_output
      textcolor="000000"
      font="ofntplain"
      alignment="left">
      </style_output>
    </style>

  <!-- dots text for output -->
  <style
    id="outDots">
    <style_output
      textcolor="202020"
      font="ofntnormal"
      alignment="left">
      </style_output>
    </style>

  <!-- header text for output -->
  <style
    id="outHeader">
    <style_output
      textcolor="000000"
      font="ofntheader"
      alignment="center">
      </style_output>
    </style>

  <!-- box outlining centered value for output -->
  <style
    id="outValBox"
    border="solid">
    <style_output
      textcolor="000000"
      font="ofntlarge"
      alignment="center">
      </style_output>
    </style>

  <!-- grey box outlining centered value for output -->
  <style
    id="outGreyBox"
    border="solid">
    <style_output
      textcolor="505050"
      font="ofntlarge"
      alignment="center">
      </style_output>
    </style>

  <!-- table with a grid around each item for output -->
  <style
    id="outTblGrid"
    border="solid">
    <style_output
      textcolor="000000"
      font="ofntlarge"
      alignment="left"
      gridcolor="505050"
      gridwidth="1"
      showgridhorz="yes"
      showgridvert="yes">
      </style_output>
    </style>

  <!-- text used in the validation report at the bottom for output -->
  <style
    id="outValid">
    <style_output
      textcolor="000000"
      font="ofntvalid">
      </style_output>
    </style>

  </document>
