#
# Copyright 2013 (c) Pointwise, Inc.
# All rights reserved.
#
# This sample script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#


# ===============================================
# THICKEN 2D GRID SCRIPT - POINTWISE
# ===============================================
# https://github.com/pointwise/Thicken2D
#
# Vnn: Release Date / Author
# v01: Nov 23, 2013 / David Garlisch
#
# ===============================================

if { ![namespace exists pw::Thicken2D] } {

package require PWI_Glyph


#####################################################################
#                       public namespace procs
#####################################################################
namespace eval pw::Thicken2D {
  namespace export setVerbose
  namespace export setExtDirection
  namespace export setExtDistance
  namespace export setExtSteps
  namespace export setMinSidewallBC
  namespace export setMaxSidewallBC
  namespace export setSidewallBC
  namespace export thicken
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::setVerbose { val } {
  variable verbose
  set verbose $val
  traceMsg "Setting verbose = $verbose."
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::setExtDirection { val } {
  if { 3 != [llength $val]} {
	set val {0 0 1}
  }
  variable extDirection
  set extDirection $val
  traceMsg "Setting extDirection = \{$val\}."
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::setExtDistance { val } {
  if { 0.0 >= $val} {
	set val 1.0
  }
  variable extDistance
  set extDistance $val
  traceMsg "Setting extDistance = $val."
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::setExtSteps { val } {
  if { 0 >= $val} {
	set val 1
  }
  variable extNumSteps
  set extNumSteps $val
  traceMsg "Setting extNumSteps = $val."
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::setMinSidewallBC { solverName bcName bcType {bcId "null"} } {
  setSidewallBC $solverName $bcName $bcType $bcId "min"
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::setMaxSidewallBC { solverName bcName bcType {bcId "null"} } {
  setSidewallBC $solverName $bcName $bcType $bcId "max"
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::setSidewallBC { solverName {bcName "Unspecified"} {bcType "Unspecified"} {bcId "null"} {minMax "both"} } {
  if { -1 == [lsearch -exact [pw::Application getCAESolverNames] $solverName] } {
	fatalMsg "Invalid solverName='$solverName' in setSidewallBC!"
  }
  switch $minMax {
  min -
  max {
    set key "$solverName,$minMax"
    set pattern $key
  }
  both {
    set key $solverName
    set pattern "$key*"
  }
  default {
	fatalMsg "Invalid minMax='$minMax' in setSidewallBC!"
  }
  }

  variable extSideWallBCInfo
  if { "Unspecified" == $bcName } {
    array unset extSideWallBCInfo $pattern
    traceMsg "Removing Side Wall BC Info for '$key'."
  } else {
    set extSideWallBCInfo($key) [list $bcName $bcType $bcId]
    traceMsg "Adding extSideWallBCInfo($key) = \{'$bcName' '$bcType' '$bcId'\}."
  }
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::thicken { domsToThicken } {
  if { 2 != [pw::Application getCAESolverDimension] } {
	fatalMsg "This script requires a 2D grid."
  }

  init

  puts "**** Preprocessing 2D grid..."

  array set con2DomsMap {} ;# maps a con from 2D grid to its doms
  array set reg2BCMap {}   ;# maps a 2D register to its BC

  # Process the 2D grid's connectors
  foreach con [pw::Grid getAll -type pw::Connector] {
    set doms [pw::Domain getDomainsFromConnectors [list $con]]
    foreach dom $doms {
      set bc [pw::BoundaryCondition getByEntities [list [list $dom $con]]]
      if { [$bc getName] == "Unspecified" } {
	# skip registers without a named BC applied
	continue
      }
      traceMsg "\{[$dom getName] [$con getName]\} has bc [bcToString $bc]"
      set reg2BCMap($dom,$con) $bc
    }
    if { 0 != [llength [array get reg2BCMap "*,$con"]] } {
      # con had at least one BC. Save the $con to $doms mapping.
      set con2DomsMap($con) $doms
    }
  }

  # Capture the list of connectors that had BCs applied
  set bcCons [array names con2DomsMap]

  puts "**** Converting to a 3D grid..."

  # switch current solver to 3D mode
  variable solverName
  pw::Application setCAESolver $solverName 3
  traceMsg "Solver '$solverName' switched to 3D mode."

  # sort list of domains - needed for lsearch
  set domsToThicken [lsort $domsToThicken]

  foreach dom $domsToThicken {
	extrudeDomain $dom
  }

  puts "**** Transferring BCs to the extruded domains..."

  # Process original BC connectors and transfer the BC to the extruded domain.
  foreach bcCon $bcCons {
    # Get the one or two domains from the original 2D grid that were on either
    # side of $bcCon. These domains were extuded to blocks in the 3D grid.
    set bcDoms $con2DomsMap($bcCon)

    # Get the domain ($extrudedDom) that was created by the extrusion of $bcCon.
    if { [getExtrudedDom $domsToThicken $bcCon extrudedDom] } {
      # drop through
    } elseif { [isInternalCon $con $bcDoms] } {
      warningMsg "Skipping internal connector [$bcCon getName]!"
      continue
    } else {
      fatalMsg "Could not find extruded domain for [$bcCon getName]!"
    }
    traceMsg "Move BC from [$bcCon getName] to [$extrudedDom getName]"
    foreach bcDom $bcDoms {
      # Get the block ($extrudedBlk) that was created by the extrusion of $bcDom.
      if { ![getExtrudedBlk $bcDom extrudedBlk] } {
	    fatalMsg "Could not find extruded block for [$bcDom getName]!"
      }
      # Get the BC associated with the 2D register
      if { [getRegBC reg2BCMap $bcDom $bcCon bc] } {
	    # The BC on the 2D register {$bcDom $bcCon} must be transferred to the 3D
	    # register {$extrudedBlk $extrudedDom}.
	    $bc apply [list [list $extrudedBlk $extrudedDom]]
      }
    }
  }
}


#####################################################################
#               private namespace procs and variables
#####################################################################
namespace eval pw::Thicken2D {

  # Set to 0/1 to disable/enable TRACE messages
  variable verbose 0

  # Controls extrusion direction
  variable extDirection {0 0 1}

  # Controls extrusion distance
  variable extDistance 1

  # Controls extrusion number of steps
  variable extNumSteps 1

  # Controls which BCs are used for extrusion base/top domains
  #   * Use SolverName entry to specify same BC for both base and top.
  #   * Use SolverName,min entry to specify base BC.
  #   * Use SolverName,max entry to specify top BC.
  # BCs are applied to the side wall domains ONLY if the solver is found
  variable extSideWallBCInfo
  array set extSideWallBCInfo {} ;#{"BCName" "BCType" Id}

  # BC applied to min (base) doms in extruded blocks.
  variable bcExtrusionBase "null"

  # BC applied to max (top) doms in extruded blocks.
  variable bcExtrusionTop "null"

  # Active CAE solver name
  variable solverName ""
}

#----------------------------------------------------------------------------
proc pw::Thicken2D::init {} {
  variable solverName
  set solverName [pw::Application getCAESolver]

  puts "**** Initializing namespace pw::Thicken2D ..."

  variable extSideWallBCInfo
  variable bcExtrusionBase
  variable bcExtrusionTop
  if { "" != [array names extSideWallBCInfo -exact $solverName] } {
    traceMsg "Same BC used for both side walls."
    lassign $extSideWallBCInfo($solverName) bcName bcType bcId
    set bcExtrusionBase [getMinMaxBC $bcName $bcType $bcId]
    set bcExtrusionTop $bcExtrusionBase
  } else {
    if { "" != [array names extSideWallBCInfo -exact "$solverName,min"] } {
      traceMsg "Using min side wall BC."
      lassign $extSideWallBCInfo($solverName,min) bcName bcType bcId
      set bcExtrusionBase [getMinMaxBC $bcName $bcType $bcId]
    }
    if { "" != [array names extSideWallBCInfo -exact "$solverName,max"] } {
      traceMsg "Using max side wall BC."
      lassign $extSideWallBCInfo($solverName,max) bcName bcType bcId
      set bcExtrusionTop [getMinMaxBC $bcName $bcType $bcId]
    }
  }
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::fatalMsg { msg {exitCode -1} } {
  puts "  ERROR: $msg"
  exit $exitCode
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::warningMsg { msg {exitCode -1} } {
  puts "  WARNING: $msg"
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::traceMsg { msg } {
  variable verbose
  if { $verbose } {
    puts "  TRACE: $msg"
  }
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::getMinMaxBC { bcName {physType null} {id null} } {
  if { [catch {pw::BoundaryCondition getByName $bcName} bc] } {
    traceMsg "Creating new BC('$bcName' '$physType' $id)."
    set bc [pw::BoundaryCondition create]
    $bc setName $bcName
    if { "null" != $physType } {
      $bc setPhysicalType $physType
    }
    if { "null" != $id } {
      $bc setId $id
    }
  } else {
    traceMsg "Found existing BC '$bcName'."
  }
  return $bc
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::bcToString { bc } {
  return "\{'[$bc getName]' '[$bc getPhysicalType]' [$bc getId]\}"
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::edgeContainsCon { edge con } {
  set ret 0
  set cnt [$edge getConnectorCount]
  for {set ii 1} {$ii <= $cnt} {incr ii} {
    if { "$con" == "[$edge getConnector $ii]" } {
      set ret 1
      break
    }
  }
  return $ret
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::isInternalCon { con doms } {
  foreach dom $doms {
    set cnt [$dom getEdgeCount]
    # edge 1 is ALWAYS the outer edge so we can skip it
    for {set ii 2} {$ii <= $cnt} {incr ii} {
      if { [edgeContainsCon [$dom getEdge $ii] $con] } {
	return 1
      }
    }
  }
  return 0
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::traceBlockFace { blk faceId } {
  if { [catch {[$blk getFace $faceId] getDomains} doms] } {
    traceMsg "  Bad faceid = $faceId"
  } else {
    foreach dom $doms {
      traceMsg "  $faceId = '[$dom getName]'"
    }
  }
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::traceBlockFaces { blk } {
  traceMsg "BLOCK '[$blk getName]'"
  set cnt [$blk getFaceCount]
  for {set ii 1} {$ii <= $cnt} {incr ii} {
    traceBlockFace $blk $ii
  }
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::extrudeDomain { dom } {
  set createMode [pw::Application begin Create]
    if { [$dom isOfType pw::DomainStructured] } {
      set face0 [lindex [pw::FaceStructured createFromDomains [list $dom]] 0]
      set blk [pw::BlockStructured create]
      set topFaceId KMaximum
    } else {
      set face0 [lindex [pw::FaceUnstructured createFromDomains [list $dom]] 0]
      set blk [pw::BlockExtruded create]
      set topFaceId JMaximum
    }
    $blk addFace $face0
  $createMode end
  unset createMode

  variable extDirection
  variable extDistance
  variable extNumSteps

  set solverMode [pw::Application begin ExtrusionSolver [list $blk]]
    $solverMode setKeepFailingStep true
    $blk setExtrusionSolverAttribute Mode Translate
    $blk setExtrusionSolverAttribute TranslateDirection $extDirection
    $blk setExtrusionSolverAttribute TranslateDistance $extDistance
    $solverMode run $extNumSteps
  $solverMode end
  unset solverMode
  unset face0
  traceMsg "----"
  traceMsg "Domain '[$dom getName]' extruded into block '[$blk getName]'"

  # BUG WORKAROUND - extruded block JMaximum is returning wrong face
  # FIXED in 17.1R5
  if { ![$dom isOfType pw::DomainStructured] } {
    set topFaceId [$blk getFaceCount]
  }

  variable bcExtrusionBase
  variable bcExtrusionTop
  if { "null" != $bcExtrusionBase } {
    $bcExtrusionBase apply [list [list $blk $dom]]
    traceMsg "Applied base BC '[$bcExtrusionBase getName]' to '[$dom getName]'"
  }

  if { "null" != $bcExtrusionTop } {
    set topDoms [[$blk getFace $topFaceId] getDomains]
    foreach topDom $topDoms {
      $bcExtrusionTop apply [list [list $blk $topDom]]
      traceMsg "Applied base BC '[$bcExtrusionTop getName]' to '[$topDom getName]'"
    }
  }
  traceBlockFaces $blk
  return $blk
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::getExtrudedDom { domsToThicken fromCon domVarName } {
  upvar $domVarName dom

  # get all domains using the current connector
  set doms [pw::Domain getDomainsFromConnectors [list $fromCon]]
  set foundDom 0
  foreach dom $doms {
    if { -1 == [lsearch -sorted $domsToThicken $dom] } {
      # $dom was NOT in the original 2D grid, it MUST have been extruded from
      # the original 2D connector $fromCon.
      set foundDom 1
      break
    }
  }
  return $foundDom
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::getExtrudedBlk { fromDom blkVarName } {
  upvar $blkVarName blk
  set ret 0
  set blocks [pw::Block getBlocksFromDomains [list $fromDom]]
  if { 1 == [llength $blocks] } {
    set blk [lindex $blocks 0]
    set ret 1
  }
  return $ret
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::getRegBC { mapName dom con bcVarName } {
  upvar $mapName reg2BCMap
  upvar $bcVarName bc
  set ret 0
  set pairs [array get reg2BCMap "$dom,$con"]
  if { 2 == [llength $pairs] } {
    set bc [lindex $pairs 1]
    set ret 1
  }
  return $ret
}



#####################################################################
#                       private namespace GUI procs
#####################################################################
namespace eval pw::Thicken2D::gui {

  namespace import ::pw::Thicken2D::*

  variable bcNames
  set bcNames [pw::BoundaryCondition getNames]

  variable bcNamesSorted
  set bcNamesSorted [lsort $bcNames]

  variable bcTypes
  set bcTypes [pw::BoundaryCondition getPhysicalTypes]

  variable errors
  array set errors [list]

  variable caeSolver
  set caeSolver [pw::Application getCAESolver]

  variable isVerbose
  set isVerbose 0

  variable extSteps
  set extSteps 1

  variable extDistance
  set extDistance 1.0

  variable minBCName
  set minBCName [lindex $bcNames 0]

  variable minBCType
  set minBCType [lindex $bcTypes 0]

  variable minBCId
  set minBCId null

  variable maxBCName
  set maxBCName [lindex $bcNames 0]

  variable maxBCType
  set maxBCType [lindex $bcTypes 0]

  variable maxBCId
  set maxBCId null

  # widget hierarchy
  variable w
  set w(LabelTitle)          .title
  set w(FrameMain)           .main

    set w(StepsLabel)        $w(FrameMain).stepsLabel
    set w(StepsEntry)        $w(FrameMain).stepsEntry

    set w(DistanceLabel)     $w(FrameMain).distanceLabel
    set w(DistanceEntry)     $w(FrameMain).distanceEntry

    set w(BoundaryLabel)     $w(FrameMain).boundaryLabel
    set w(BCNameLabel)       $w(FrameMain).bcNameLabel
    set w(BCTypeLabel)       $w(FrameMain).bcTypeLabel
    set w(BCIdLabel)         $w(FrameMain).bcIdLabel

    set w(MinBCLabel)        $w(FrameMain).minBCLabel
    set w(MinBCNameCombo)    $w(FrameMain).minBCNameCombo
    set w(MinBCTypeCombo)    $w(FrameMain).minBCTypeCombo
    set w(MinBCIdEntry)      $w(FrameMain).minBCIdEntry

    set w(MaxBCLabel)        $w(FrameMain).maxBCLabel
    set w(MaxBCNameCombo)    $w(FrameMain).maxBCNameCombo
    set w(MaxBCTypeCombo)    $w(FrameMain).maxBCTypeCombo
    set w(MaxBCIdEntry)      $w(FrameMain).maxBCIdEntry

    set w(VerboseCheck)      $w(FrameMain).verboseCheck

  set w(FrameButtons)        .fbuttons
    set w(Logo)              $w(FrameButtons).logo
    set w(OkButton)          $w(FrameButtons).okButton
    set w(CancelButton)      $w(FrameButtons).cancelButton
} ;# namespace eval pw::Thicken2D::gui

#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::run { } {
  makeWindow
  tkwait window .
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::checkErrors { } {
  variable errors
  variable w
  if { 0 == [array size errors] } {
    set state normal
  } else {
    set state disabled
  }
  if { [catch {$w(OkButton) configure -state $state} err] } {
    #puts $err
  }
  return 1
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::validateInput { val type key } {
  variable errors
  if { [string is $type -strict $val] } {
    array unset errors $key
  } else {
    set errors($key) 1
  }
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::validateInteger { val key } {
  validateInput $val integer $key
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::validateBCId { val key } {
  if { "null" == $val } {
    # make integer check happy
    set val 0
  }
  validateInteger $val $key
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::validateDouble { val key } {
  validateInput $val double $key
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::validateString { val key } {
  validateInput $val print $key
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::okAction { } {
  variable caeSolver
  variable isVerbose
  variable extDistance
  variable extSteps
  variable minBCName
  variable minBCType
  variable minBCId
  variable maxBCName
  variable maxBCType
  variable maxBCId

  setVerbose $isVerbose

  # Controls extrusion direction
  setExtDirection {0 0 1}

  # Controls extrusion distance
  setExtDistance $extDistance

  # Controls extrusion number of steps
  setExtSteps $extSteps

  # clear all BC setting for solver
  setSidewallBC $caeSolver
  setMinSidewallBC $caeSolver $minBCName $minBCType $minBCId
  setMaxSidewallBC $caeSolver $maxBCName $maxBCType $maxBCId

  # Capture a list of all the grid's domains
  set allDoms [pw::Grid getAll -type pw::Domain]
  # Only keep the visible and selectable domains
  set domsToThicken {}
  foreach dom $allDoms {
    if { ![pw::Display isLayerVisible [$dom getLayer]] } {
      continue
    } elseif { ![$dom getEnabled] } {
      continue
    } else {
      lappend domsToThicken $dom
    }
  }
  thicken $domsToThicken
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::stepsAction { action newVal oldVal } {
  if { -1 != $action } {
    validateInteger $newVal STEPS
    checkErrors
  }
  return 1
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::distanceAction { action newVal oldVal } {
  variable extDistance
  if { -1 != $action } {
    validateDouble $newVal DISTANCE
    checkErrors
  }
  return 1
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::bcNameAction { which action newVal oldVal } {
  set lwhich [string tolower $which]
  set bcTypeCombo ${which}BCTypeCombo
  set bcIdEntry ${which}BCIdEntry
  set bcTypeVar ${lwhich}BCType
  set bcIdVar ${lwhich}BCId

  variable w
  variable ${lwhich}BCName
  variable ${lwhich}BCType
  variable ${lwhich}BCId
  variable bcNamesSorted

  if { -1 == [lsearch -sorted $bcNamesSorted $newVal] } {
    # bc does not exist, allow type and id values
    $w($bcTypeCombo) configure -state readonly
    $w($bcIdEntry) configure -state normal
    set $bcIdVar null
  } else {
    # bc exists, disallow type and id values
    $w($bcTypeCombo) configure -state disabled
    $w($bcIdEntry) configure -state disabled
    set bc [pw::BoundaryCondition getByName $newVal]
    set $bcTypeVar [$bc getPhysicalType]
    if { "Unspecified" == $newVal } {
      set $bcIdVar ""
    } else {
      set $bcIdVar [$bc getId]
    }
  }
  validateString $newVal "${which}_NAME"
  checkErrors
  return 1
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::minBCNameAction { action newVal oldVal } {
  bcNameAction Min $action $newVal $oldVal
  return 1
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::maxBCNameAction { action newVal oldVal } {
  bcNameAction Max $action $newVal $oldVal
  return 1
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::minBCIdAction { action newVal oldVal } {
  if { -1 != $action } {
    validateBCId $newVal MIN_ID
    checkErrors
  }
  return 1
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::maxBCIdAction { action newVal oldVal } {
  if { -1 != $action } {
    validateBCId $newVal MAX_ID
    checkErrors
  }
  return 1
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::makeWindow { } {
  variable w
  variable caeSolver
  variable bcNames
  variable bcTypes
  variable minBCName
  variable maxBCName

  set disabledBgColor [ttk::style lookup TEntry -fieldbackground disabled]
  ttk::style map TCombobox -fieldbackground [list disabled $disabledBgColor]

  # create the widgets
  label $w(LabelTitle) -text "Thicken 2D Grid ($caeSolver)"
  setTitleFont $w(LabelTitle)

  frame $w(FrameMain) -padx 15

  label $w(StepsLabel) -text "Extrude Steps" -anchor w
  entry $w(StepsEntry) -textvariable pw::Thicken2D::gui::extSteps -width 4 \
    -validate key -validatecommand { pw::Thicken2D::gui::stepsAction %d %P %s }

  label $w(DistanceLabel) -text "Extrude Distance" -anchor w
  entry $w(DistanceEntry) -textvariable pw::Thicken2D::gui::extDistance -width 8 \
    -validate key -validatecommand { pw::Thicken2D::gui::distanceAction %d %P %s }

  label $w(BoundaryLabel) -text "Boundary" -anchor w
  label $w(BCNameLabel) -text "Name" -anchor w
  label $w(BCTypeLabel) -text "Type" -anchor w
  label $w(BCIdLabel) -text "Id" -anchor w

  label $w(MinBCLabel) -text "Min Side BC" -anchor w
  ttk::combobox $w(MinBCNameCombo) -values $bcNames -state normal \
    -textvariable pw::Thicken2D::gui::minBCName -validate key \
    -validatecommand { pw::Thicken2D::gui::minBCNameAction %d %P %s }
  bind $w(MinBCNameCombo) <<ComboboxSelected>> \
    {pw::Thicken2D::gui::minBCNameAction 9 $pw::Thicken2D::gui::minBCName \
      $pw::Thicken2D::gui::minBCName}
  ttk::combobox $w(MinBCTypeCombo) -values $bcTypes \
    -state readonly -textvariable pw::Thicken2D::gui::minBCType
  entry $w(MinBCIdEntry) -textvariable pw::Thicken2D::gui::minBCId -width 4 \
    -validate key -validatecommand { pw::Thicken2D::gui::minBCIdAction %d %P %s }

  label $w(MaxBCLabel) -text "Max Side BC" -anchor w
  ttk::combobox $w(MaxBCNameCombo) -values $bcNames \
    -state normal -textvariable pw::Thicken2D::gui::maxBCName -validate key \
    -validatecommand { pw::Thicken2D::gui::maxBCNameAction %d %P %s }
  bind $w(MaxBCNameCombo) <<ComboboxSelected>> \
    {pw::Thicken2D::gui::maxBCNameAction 9 $pw::Thicken2D::gui::maxBCName \
      $pw::Thicken2D::gui::maxBCName}
  ttk::combobox $w(MaxBCTypeCombo) -values $bcTypes \
    -state readonly -textvariable pw::Thicken2D::gui::maxBCType
  entry $w(MaxBCIdEntry) -textvariable pw::Thicken2D::gui::maxBCId -width 4 \
    -validate key -validatecommand { pw::Thicken2D::gui::maxBCIdAction %d %P %s }

  checkbutton $w(VerboseCheck) -text "Enable verbose output" \
    -variable pw::Thicken2D::gui::isVerbose -anchor w -padx 20 -state active

  frame $w(FrameButtons) -relief sunken -padx 15 -pady 5

  label $w(Logo) -image [pwLogo] -bd 0 -relief flat
  button $w(OkButton) -text "OK" -width 12 -bd 2 \
    -command { wm withdraw . ; pw::Thicken2D::gui::okAction ; exit }
  button $w(CancelButton) -text "Cancel" -width 12 -bd 2 \
    -command { exit }

  # lay out the form
  pack $w(LabelTitle) -side top -pady 5
  pack [frame .sp -bd 2 -height 2 -relief sunken] -pady 0 -side top -fill x
  pack $w(FrameMain) -side top -fill both -expand 1 -pady 10

  # lay out the form in a grid
  grid $w(StepsLabel)    -row 0 -column 0 -sticky we -pady 3 -padx 3
  grid $w(StepsEntry)    -row 0 -column 1 -sticky w -pady 3 -padx 3

  grid $w(DistanceLabel) -row 1 -column 0 -sticky we -pady 3 -padx 3
  grid $w(DistanceEntry) -row 1 -column 1 -sticky w -pady 3 -padx 3

  grid $w(BoundaryLabel) -row 2 -column 0 -sticky w -pady 3 -padx 3
  grid $w(BCNameLabel)   -row 2 -column 1 -sticky w -pady 3 -padx 3
  grid $w(BCTypeLabel)   -row 2 -column 2 -sticky w -pady 3 -padx 3
  grid $w(BCIdLabel)     -row 2 -column 3 -sticky w -pady 3 -padx 3

  grid $w(MinBCLabel)     -row 3 -column 0 -sticky we -pady 3 -padx 3
  grid $w(MinBCNameCombo) -row 3 -column 1 -sticky we -pady 3 -padx 3
  grid $w(MinBCTypeCombo) -row 3 -column 2 -sticky we -pady 3 -padx 3
  grid $w(MinBCIdEntry)   -row 3 -column 3 -sticky we -pady 3 -padx 3

  grid $w(MaxBCLabel)     -row 4 -column 0 -sticky we -pady 3 -padx 3
  grid $w(MaxBCNameCombo) -row 4 -column 1 -sticky we -pady 3 -padx 3
  grid $w(MaxBCTypeCombo) -row 4 -column 2 -sticky we -pady 3 -padx 3
  grid $w(MaxBCIdEntry)   -row 4 -column 3 -sticky we -pady 3 -padx 3

  grid $w(VerboseCheck)  -row 5 -columnspan 2 -sticky we -pady 3 -padx 3

  # lay out buttons
  pack $w(CancelButton) $w(OkButton) -pady 3 -padx 3 -side right
  pack $w(Logo) -side left -padx 5

  # give extra space to (only) column
  grid columnconfigure $w(FrameMain) 1 -weight 1

  pack $w(FrameButtons) -fill x -side bottom -padx 0 -pady 0 -anchor s

  # init GUI state for BC data
  minBCNameAction 8 $minBCName $minBCName
  maxBCNameAction 8 $maxBCName $maxBCName

  focus $w(VerboseCheck)
  raise .

  # don't allow window to resize
  wm resizable . 0 0
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::setTitleFont { widget {fontScale 1.5} } {
  # set the font for the input widget to be bold and 1.5 times larger than
  # the default font
  variable titleFont
  if { ! [info exists titleFont] } {
    set fontSize [font actual TkCaptionFont -size]
    set titleFont [font create -family [font actual TkCaptionFont -family] \
      -weight bold -size [expr {int($fontScale * $fontSize)}]]
  }
  $widget configure -font $titleFont
}


#----------------------------------------------------------------------------
proc pw::Thicken2D::gui::pwLogo {} {
  set logoData "
R0lGODlheAAYAIcAAAAAAAICAgUFBQkJCQwMDBERERUVFRkZGRwcHCEhISYmJisrKy0tLTIyMjQ0
NDk5OT09PUFBQUVFRUpKSk1NTVFRUVRUVFpaWlxcXGBgYGVlZWlpaW1tbXFxcXR0dHp6en5+fgBi
qQNkqQVkqQdnrApmpgpnqgpprA5prBFrrRNtrhZvsBhwrxdxsBlxsSJ2syJ3tCR2siZ5tSh6tix8
ti5+uTF+ujCAuDODvjaDvDuGujiFvT6Fuj2HvTyIvkGKvkWJu0yUv2mQrEOKwEWNwkaPxEiNwUqR
xk6Sw06SxU6Uxk+RyVKTxlCUwFKVxVWUwlWWxlKXyFOVzFWWyFaYyFmYx16bwlmZyVicyF2ayFyb
zF2cyV2cz2GaxGSex2GdymGezGOgzGSgyGWgzmihzWmkz22iymyizGmj0Gqk0m2l0HWqz3asznqn
ynuszXKp0XKq1nWp0Xaq1Hes0Xat1Hmt1Xyt0Huw1Xux2IGBgYWFhYqKio6Ojo6Xn5CQkJWVlZiY
mJycnKCgoKCioqKioqSkpKampqmpqaurq62trbGxsbKysrW1tbi4uLq6ur29vYCu0YixzYOw14G0
1oaz14e114K124O03YWz2Ie12oW13Im10o621Ii22oi23Iy32oq52Y252Y+73ZS51Ze81JC625G7
3JG825K83Je72pW93Zq92Zi/35G+4aC90qG+15bA3ZnA3Z7A2pjA4Z/E4qLA2KDF3qTA2qTE3avF
36zG3rLM3aPF4qfJ5KzJ4LPL5LLM5LTO4rbN5bLR6LTR6LXQ6r3T5L3V6cLCwsTExMbGxsvLy8/P
z9HR0dXV1dbW1tjY2Nra2tzc3N7e3sDW5sHV6cTY6MnZ79De7dTg6dTh69Xi7dbj7tni793m7tXj
8Nbk9tjl9N3m9N/p9eHh4eTk5Obm5ujo6Orq6u3t7e7u7uDp8efs8uXs+Ozv8+3z9vDw8PLy8vL0
9/b29vb5+/f6+/j4+Pn6+/r6+vr6/Pn8/fr8/Pv9/vz8/P7+/gAAACH5BAMAAP8ALAAAAAB4ABgA
AAj/AP8JHEiwoMGDCBMqXMiwocOHECNKnEixosWLGDNqZCioo0dC0Q7Sy2btlitisrjpK4io4yF/
yjzKRIZPIDSZOAUVmubxGUF88Aj2K+TxnKKOhfoJdOSxXEF1OXHCi5fnTx5oBgFo3QogwAalAv1V
yyUqFCtVZ2DZceOOIAKtB/pp4Mo1waN/gOjSJXBugFYJBBflIYhsq4F5DLQSmCcwwVZlBZvppQtt
D6M8gUBknQxA879+kXixwtauXbhheFph6dSmnsC3AOLO5TygWV7OAAj8u6A1QEiBEg4PnA2gw7/E
uRn3M7C1WWTcWqHlScahkJ7NkwnE80dqFiVw/Pz5/xMn7MsZLzUsvXoNVy50C7c56y6s1YPNAAAC
CYxXoLdP5IsJtMBWjDwHHTSJ/AENIHsYJMCDD+K31SPymEFLKNeM880xxXxCxhxoUKFJDNv8A5ts
W0EowFYFBFLAizDGmMA//iAnXAdaLaCUIVtFIBCAjP2Do1YNBCnQMwgkqeSSCEjzzyJ/BFJTQfNU
WSU6/Wk1yChjlJKJLcfEgsoaY0ARigxjgKEFJPec6J5WzFQJDwS9xdPQH1sR4k8DWzXijwRbHfKj
YkFO45dWFoCVUTqMMgrNoQD08ckPsaixBRxPKFEDEbEMAYYTSGQRxzpuEueTQBlshc5A6pjj6pQD
wf9DgFYP+MPHVhKQs2Js9gya3EB7cMWBPwL1A8+xyCYLD7EKQSfEF1uMEcsXTiThQhmszBCGC7G0
QAUT1JS61an/pKrVqsBttYxBxDGjzqxd8abVBwMBOZA/xHUmUDQB9OvvvwGYsxBuCNRSxidOwFCH
J5dMgcYJUKjQCwlahDHEL+JqRa65AKD7D6BarVsQM1tpgK9eAjjpa4D3esBVgdFAB4DAzXImiDY5
vCFHESko4cMKSJwAxhgzFLFDHEUYkzEAG6s6EMgAiFzQA4rBIxldExBkr1AcJzBPzNDRnFCKBpTd
gCD/cKKKDFuYQoQVNhhBBSY9TBHCFVW4UMkuSzf/fe7T6h4kyFZ/+BMBXYpoTahB8yiwlSFgdzXA
5JQPIDZCW1FgkDVxgGKCFCywEUQaKNitRA5UXHGFHN30PRDHHkMtNUHzMAcAA/4gwhUCsB63uEF+
bMVB5BVMtFXWBfljBhhgbCFCEyI4EcIRL4ChRgh36LBJPq6j6nS6ISPkslY0wQbAYIr/ahCeWg2f
ufFaIV8QNpeMMAkVlSyRiRNb0DFCFlu4wSlWYaL2mOp13/tY4A7CL63cRQ9aEYBT0seyfsQjHedg
xAG24ofITaBRIGTW2OJ3EH7o4gtfCIETRBAFEYRgC06YAw3CkIqVdK9cCZRdQgCVAKWYwy/FK4i9
3TYQIboE4BmR6wrABBCUmgFAfgXZRxfs4ARPPCEOZJjCHVxABFAA4R3sic2bmIbAv4EvaglJBACu
IxAMAKARBrFXvrhiAX8kEWVNHOETE+IPbzyBCD8oQRZwwIVOyAAXrgkjijRWxo4BLnwIwUcCJvgP
ZShAUfVa3Bz/EpQ70oWJC2mAKDmwEHYAIxhikAQPeOCLdRTEAhGIQKL0IMoGTGMgIBClA9QxkA3U
0hkKgcy9HHEQDcRyAr0ChAWWucwNMIJZ5KilNGvpADtt5JrYzKY2t8nNbnrzm+B8SEAAADs="

  return [image create photo -format GIF -data $logoData]
}

} ;# ![namespace exists pw::Thicken2D]


#####################################################################
#                           MAIN
#####################################################################
if { ![info exists disableAutoRun_Thicken2D] } {
  pw::Script loadTk
  pw::Thicken2D::gui::run
}

# END SCRIPT

#
# DISCLAIMER:
# TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW, POINTWISE DISCLAIMS
# ALL WARRANTIES, EITHER EXPRESS OR IMPLIED, INCLUDING, BUT NOT LIMITED
# TO, IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, WITH REGARD TO THIS SCRIPT.  TO THE MAXIMUM EXTENT PERMITTED
# BY APPLICABLE LAW, IN NO EVENT SHALL POINTWISE BE LIABLE TO ANY PARTY
# FOR ANY SPECIAL, INCIDENTAL, INDIRECT, OR CONSEQUENTIAL DAMAGES
# WHATSOEVER (INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF
# BUSINESS INFORMATION, OR ANY OTHER PECUNIARY LOSS) ARISING OUT OF THE
# USE OF OR INABILITY TO USE THIS SCRIPT EVEN IF POINTWISE HAS BEEN
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGES AND REGARDLESS OF THE
# FAULT OR NEGLIGENCE OF POINTWISE.
#
