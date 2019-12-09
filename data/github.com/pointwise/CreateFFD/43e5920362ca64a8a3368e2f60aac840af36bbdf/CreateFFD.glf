#
# Copyright 2014 (c) Pointwise, Inc.
# All rights reserved.
#
# This sample script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#

# ============================================================================
# GENERATE FFD BOXES FOR SU2
# ============================================================================
# Written by: Travis Carrigan, Senior Engineer, Pointwise Inc.
#
# This script will generate FFD boxes for use with Stanford's SU2 solver. The
# boxes are constructed using the extents of grouped connectors (2D) or domains
# (3D). The group names are parsed for the parameters necessary to generate the
# box. Each box is assigned a layer to help the user declutter the display and
# make it easier for the data to be written to the .su2 grid.
#
# To create a group, select the entities and go to Create, Group. The group
# name should be of the following format: ffd-name-scale-dimension.
#
# ffd:       Tag to let script know the group will be used for an FFD box
# name:      The name of the FFD box
# scale:     The x,y,z scale vector for the box scaling (default=1.2)
# dimension: Polynomial degree in the i,j,k directions (>=1, default=1)
#
# Example group name (please note the format): ffd-wing-1.2,1.2,1.2-4,5,2
# Short group name format (will use defaults): ffd-wing
#
# Grids created in 2D for SU2 must be in the z-plane. Therefore, when running
# in 2D, only x,y compenents of the scale vector and dimension are used. Also,
# to invoke 2D mode, ensure that the CAE solver dimension has been set to 2D.
#
# Example 2D group name (only x,y needed): ffd-wing-1.2,1.2-4,2
#
# The boxes are aligned with the global x,y,z axes. However, database points
# are generated at nodes and all connector, domain, and block centroids so
# the user can transform the box using the Edit, Transform menu. These points
# can be used as anchor points during various transformations.
#
# When the script is run it will check to see if there are any "FFD Layers"
# where an FFD layer is a layer with a description of the form: FFD - XXX.
# The script will then ensure that all "FFD groups" have been used to create
# their respective FFD boxes and layers. Lastly, the script will export all
# FFD boxes to a file containing all necessary FFD information for SU2. The
# file name will be ffd.su2, The script will only export the FFD boxes when
# there is a one-to-one match between the groups and the layers. Therefore,
# the script must be run at least twice to create all the FFD boxes and then
# export them for SU2.
#
# Because the script looks to export all FFD layers whenever it is executed,
# FFD layers you would not like to export should be tagged "FFDx" in the
# layer description field. For example,
#
# Script will export the following layer:     FFD - wing
# Script will not export the following layer: FFDx - wing
#
# Hint: You can organize the layers by name by clicking the Description
# column in the Layer panel.
#
# Typically, you'll run this script after you've completed the grid, setup
# all the CAE boundary conditions, and exported the .su2 grid file. Once
# you create the FFD boxes and export the data, simply concatenate the two
# files together like the following example,
#
# cat grid.su2 ffd.su2 > aircraft.su2
#

package require PWI_Glyph

# PROCS

# Get the extents (min, max corner points) for a list of entities
proc GetExtents {ents} {

  set entExtents [pwu::Extents empty]

  foreach ent $ents {
    set entExtents [pwu::Extents enclose $entExtents [$ent getExtents]]
  }

  return $entExtents

}

# Create a connector given two points and a dimension
proc CreateCon {pt1 pt2 dim} {

  set seg [pw::SegmentSpline create]
    $seg addPoint $pt1
    $seg addPoint $pt2

  set con [pw::Connector create]
    $con addSegment   $seg
    $con setDimension $dim

  CreateMidPoint  $con
  CreateEndPoints $con

  $con setRenderAttribute ColorMode Entity
  $con setColor [list 1.0 1.0 1.0]

  return $con

}

# Create a structured domain from a list of connectors
proc CreateDom {cons} {

  set dom [pw::DomainStructured createFromConnectors $cons]

  CreateMidPoint $dom

  $dom setRenderAttribute ColorMode Entity
  $dom setColor [list 1.0 1.0 1.0]

  return $dom

}

# Create a structured block given the block extents and dimensions
proc CreateBlock {extents dims} {

  set dimension [GetDimension]

  if {$dimension == 2} {

    set dim(x) [expr {[lindex $dims 0] + 1}]
    set dim(y) [expr {[lindex $dims 1] + 1}]

    set pt(1) [lindex $extents 0]
    set pt(7) [lindex $extents 1]

    set pt1(x) [pwu::Vector3 x $pt(1)]
    set pt1(y) [pwu::Vector3 y $pt(1)]
    set pt1(z) [pwu::Vector3 z $pt(1)]

    set pt7(x) [pwu::Vector3 x $pt(7)]
    set pt7(y) [pwu::Vector3 y $pt(7)]
    set pt7(z) [pwu::Vector3 z $pt(7)]

    set pt(2) "$pt7(x) $pt1(y) $pt1(z)"
    set pt(3) "$pt7(x) $pt7(y) $pt1(z)"
    set pt(4) "$pt1(x) $pt7(y) $pt1(z)"

    set mode [pw::Application begin Create]

      set con(1)  [CreateCon $pt(1) $pt(2) $dim(x)]
      set con(2)  [CreateCon $pt(2) $pt(3) $dim(y)]
      set con(3)  [CreateCon $pt(3) $pt(4) $dim(x)]
      set con(4)  [CreateCon $pt(4) $pt(1) $dim(y)]

      set block [CreateDom [list $con(1) $con(2)  $con(3) $con(4)]]

    $mode end

  } else {

    set dim(x) [expr {[lindex $dims 0] + 1}]
    set dim(y) [expr {[lindex $dims 1] + 1}]
    set dim(z) [expr {[lindex $dims 2] + 1}]

    set pt(1) [lindex $extents 0]
    set pt(7) [lindex $extents 1]

    set pt1(x) [pwu::Vector3 x $pt(1)]
    set pt1(y) [pwu::Vector3 y $pt(1)]
    set pt1(z) [pwu::Vector3 z $pt(1)]

    set pt7(x) [pwu::Vector3 x $pt(7)]
    set pt7(y) [pwu::Vector3 y $pt(7)]
    set pt7(z) [pwu::Vector3 z $pt(7)]

    set pt(2) "$pt7(x) $pt1(y) $pt1(z)"
    set pt(3) "$pt7(x) $pt7(y) $pt1(z)"
    set pt(4) "$pt1(x) $pt7(y) $pt1(z)"
    set pt(5) "$pt1(x) $pt1(y) $pt7(z)"
    set pt(6) "$pt7(x) $pt1(y) $pt7(z)"
    set pt(8) "$pt1(x) $pt7(y) $pt7(z)"

    set mode [pw::Application begin Create]

      set con(1)  [CreateCon $pt(1) $pt(2) $dim(x)]
      set con(2)  [CreateCon $pt(2) $pt(3) $dim(y)]
      set con(3)  [CreateCon $pt(3) $pt(4) $dim(x)]
      set con(4)  [CreateCon $pt(4) $pt(1) $dim(y)]
      set con(5)  [CreateCon $pt(5) $pt(6) $dim(x)]
      set con(6)  [CreateCon $pt(6) $pt(7) $dim(y)]
      set con(7)  [CreateCon $pt(7) $pt(8) $dim(x)]
      set con(8)  [CreateCon $pt(8) $pt(5) $dim(y)]
      set con(9)  [CreateCon $pt(1) $pt(5) $dim(z)]
      set con(10) [CreateCon $pt(2) $pt(6) $dim(z)]
      set con(11) [CreateCon $pt(3) $pt(7) $dim(z)]
      set con(12) [CreateCon $pt(4) $pt(8) $dim(z)]

      set dom(1) [CreateDom [list $con(1) $con(2)  $con(3) $con(4)]]
      set dom(2) [CreateDom [list $con(5) $con(8)  $con(7) $con(6)]]
      set dom(3) [CreateDom [list $con(4) $con(12) $con(8) $con(9)]]
      set dom(4) [CreateDom [list $con(2) $con(10) $con(6) $con(11)]]
      set dom(5) [CreateDom [list $con(1) $con(9)  $con(5) $con(10)]]
      set dom(6) [CreateDom [list $con(3) $con(11) $con(7) $con(12)]]

      set block [pw::BlockStructured createFromDomains \
                [list $dom(1) $dom(2) $dom(3) $dom(4) $dom(5) $dom(6)]]

      CreateMidPoint $block

    $mode end

  }

  return $block

}

# Get block corner points and dimensions
proc GetFFDBoxAttributes {box} {

  set dimension [GetDimension]
  set dims      [$box getDimensions]

  if {$dimension == 2} {

    set i(dim) [lindex $dims 0]
    set j(dim) [lindex $dims 1]

    set i(deg) [expr {$i(dim) - 1}]
    set j(deg) [expr {$j(dim) - 1}]

    set i(min) 1
    set j(min) 1

    set i(max) $i(dim)
    set j(max) $j(dim)

    set pt(1) [lrange [$box getXYZ "$i(min) $j(min)"] 0 1]
    set pt(2) [lrange [$box getXYZ "$i(max) $j(min)"] 0 1]
    set pt(3) [lrange [$box getXYZ "$i(max) $j(max)"] 0 1]
    set pt(4) [lrange [$box getXYZ "$i(min) $j(max)"] 0 1]

    return [list [list $i(deg) $j(deg)] \
                 [list $pt(1) $pt(2) $pt(3) $pt(4)]]

  } else {

    set i(dim) [lindex $dims 0]
    set j(dim) [lindex $dims 1]
    set k(dim) [lindex $dims 2]

    set i(deg) [expr {$i(dim) - 1}]
    set j(deg) [expr {$j(dim) - 1}]
    set k(deg) [expr {$k(dim) - 1}]

    set i(min) 1
    set j(min) 1
    set k(min) 1

    set i(max) $i(dim)
    set j(max) $j(dim)
    set k(max) $k(dim)

    set pt(1) [$box getXYZ "$i(min) $j(min) $k(min)"]
    set pt(2) [$box getXYZ "$i(max) $j(min) $k(min)"]
    set pt(3) [$box getXYZ "$i(max) $j(max) $k(min)"]
    set pt(4) [$box getXYZ "$i(min) $j(max) $k(min)"]
    set pt(5) [$box getXYZ "$i(min) $j(min) $k(max)"]
    set pt(6) [$box getXYZ "$i(max) $j(min) $k(max)"]
    set pt(7) [$box getXYZ "$i(max) $j(max) $k(max)"]
    set pt(8) [$box getXYZ "$i(min) $j(max) $k(max)"]

    return [list [list $i(deg) $j(deg) $k(deg)]    \
                 [list $pt(1) $pt(2) $pt(3) $pt(4) \
                       $pt(5) $pt(6) $pt(7) $pt(8)]]

  }

}

# Create a database point at an entity midpoint location
proc CreateMidPoint {ent} {

  CreateDbPoint [GetEntityCenter $ent]

}

# Create a database point at connector nodes
proc CreateEndPoints {con} {

  CreateDbPoint [$con getXYZ -arc 0.0]
  CreateDbPoint [$con getXYZ -arc 1.0]

}

# General procedure for creating a database point
proc CreateDbPoint {pt} {

  set dbPt [pw::Point create]
    $dbPt setPoint $pt

}

# Get the center of an extents vector
proc GetExtentsCenter {extents} {

  set minExtents [lindex $extents 0]
  set maxExtents [lindex $extents 1]

  set center [pwu::Vector3 divide [pwu::Vector3 add $minExtents $maxExtents] 2.0]

  return $center

}

# Return the center of an entity, useful for scaling operations
proc GetEntityCenter {ent} {

  set extents [GetExtents $ent]
  set center  [GetExtentsCenter $extents]

  return $center

}

# Scale the extents of an object by a scaling vector
proc ScaleExtents {extents n} {

  set minExtents [lindex $extents 0]
  set maxExtents [lindex $extents 1]

  set center [GetExtentsCenter $extents]

  set transformMin [pwu::Transform apply [pwu::Transform scaling -anchor $center $n] $minExtents]
  set transformMax [pwu::Transform apply [pwu::Transform scaling -anchor $center $n] $maxExtents]

  return [list $transformMin $transformMax]

}

# Get the layer entity count
proc GetLayerEntityCount {layer} {

  set entCnts [pw::Layer getLayerEntityCounts $layer]
  set totalEntCnt 0

  foreach entCnt $entCnts {
    set totalEntCnt [incr totalEntCnt $entCnt]
  }

  return $totalEntCnt

}

# Retrieve a free layer from the layer manager
proc GetFreeLayer {} {

  set numLayers [pw::Layer getCount]
  set layer [expr {$numLayers - 1}]

  while {$layer >= 0} {

    set totalEntCnt [GetLayerEntityCount $layer]

    if {$totalEntCnt} {
      incr layer -1
      continue
    } else {
      break
    }

  }

  return $layer

}

# Get all the domain groups
proc GetGroups {} {

  set dimension [GetDimension]

  if {$dimension == 2} {
    set groups [pw::Group getAll -type pw::Connector]
  } else {
    set groups [pw::Group getAll -type pw::Domain]
  }

  return $groups

}

# Return only the groups appended with "ffd"
proc GetFFDGroups {} {

  set groups    [GetGroups]
  set ffdGroups [list]

  foreach group $groups {

    set groupName [$group getName]
    set params    [split $groupName "-"]

    if {[lindex $params 0] eq "ffd"} {
      lappend ffdGroups $group
    }

  }

  return $ffdGroups

}

# Return only layers prepended with "FFD"
proc GetFFDLayers {} {

  set numLayers    [pw::Layer getCount]
  set ffdLayers    [list]

  for {set layer 0} {$layer < $numLayers} {incr layer} {

    set layerName [pw::Layer getDescription $layer]
    if {[string range $layerName 0 2] eq "FFD"} {
      lappend ffdLayers $layer
    }


  }

  return $ffdLayers

}

# Return FFD layers that contain entities
proc GetFullFFDLayers {} {

  set ffdLayers     [GetFFDLayers]
  set fullFFDLayers [list]

  foreach ffdLayer $ffdLayers {

    set entCnt [GetLayerEntityCount $ffdLayer]

    if {$entCnt} {
      lappend fullFFDLayers $ffdLayer
    }

  }

  return $fullFFDLayers

}

# Return FFD layers tagged "FFD" for export
proc GetExportFFDLayers {} {

  set fullFFDLayers   [GetFullFFDLayers]
  set exportFFDLayers [list]

  foreach layer $fullFFDLayers {

    set layerName [pw::Layer getDescription $layer]
    if {[string range $layerName 0 3] eq "FFD-"} {
      lappend exportFFDLayers $layer
    }

  }

  return $exportFFDLayers

}

# Get the FFD box name from the group name
proc GetFFDNameFromGroup {group} {

  set fullName [$group getName]
  set params   [split $fullName "-"]
  set ffdName  [lindex $params 1]

  return $ffdName

}

# Get the FFD box name from the layer name
proc GetFFDNameFromLayer {layer} {

  set fullName [pw::Layer getDescription $layer]
  set params   [split $fullName "-"]
  set ffdName  [lindex $params 1]

  return $ffdName

}

# Get the group from the FFD name
proc GetFFDGroupFromName {name} {

  set ffdGroups [GetFFDGroups]

  foreach group $ffdGroups {

    set ffdName [GetFFDNameFromGroup $group]

    if {$name eq $ffdName} {
      return $group
    }

  }

}

# Return FFD boxes (blocks) from FFD layers tagged for export
proc GetExportFFDBoxes {} {

  set dimension [GetDimension]

  set exportFFDLayers [GetExportFFDLayers]
  set boxes           [list]

  foreach layer $exportFFDLayers {

    if {$dimension == 2} {
      lappend boxes [pw::Layer getLayerEntities -type pw::DomainStructured $layer]
    } else {
      lappend boxes [pw::Layer getLayerEntities -type pw::BlockStructured $layer]
    }
  }

  return $boxes

}

# Given a group, return the parameters required to build the FFD box
proc GetParameters {group} {

  set dimension [GetDimension]
  set groupName [$group getName]
  set params    [split $groupName "-"]

  set ffdName  [lindex $params 1]

  if {[llength $params] > 2} {

    set ffdScale [lindex $params 2]
    set ffdDim   [lindex $params 3]

    if {$dimension == 2} {

      set ffdScale "$ffdScale,0"
      set ffdDim   "$ffdDim,0"

    }

  } else {

    if {$dimension == 2} {

      set ffdScale "1.2,1.2,0"
      set ffdDim   "1,1,0"

    } else {

      set ffdScale "1.2,1.2,1.2"
      set ffdDim   "1,1,1"

    }

  }

  set ffdScale [split $ffdScale ","]
  set ffdDim   [split $ffdDim   ","]

  set ffdDoms  [$group getEntityList]

  return [list $ffdName $ffdScale $ffdDim $ffdDoms]

}

# Write the FFD data to a file named ffd.su2
proc ExportFFD {} {

  set dimension [GetDimension]

  set cwd      [file dirname [info script]]
  set filename "$cwd/ffd.su2"
  set f        [open $filename w]
  set boxes    [GetExportFFDBoxes]
  set id       0

  puts $f "FFD_NBOX= [llength $boxes]"
  puts $f "FFD_NLEVEL= 1"

  foreach box $boxes {

    set boxAttributes [GetFFDBoxAttributes $box]
    set ffdDegree     [lindex $boxAttributes 0]
    set cornerPoints  [lindex $boxAttributes 1]

    if {$dimension == 2} {

      set ideg [lindex $ffdDegree 0]
      set jdeg [lindex $ffdDegree 1]

      set pt1  [lindex $cornerPoints 0]
      set pt2  [lindex $cornerPoints 1]
      set pt3  [lindex $cornerPoints 2]
      set pt4  [lindex $cornerPoints 3]

      puts $f "FFD_TAG= $id"
      puts $f "FFD_LEVEL= 0"
      puts $f "FFD_DEGREE_I= $ideg"
      puts $f "FFD_DEGREE_J= $jdeg"
      puts $f "FFD_PARENTS= 0"
      puts $f "FFD_CHILDREN= 0"
      puts $f "FFD_CORNER_POINTS=4"
      puts $f "$pt1"
      puts $f "$pt2"
      puts $f "$pt3"
      puts $f "$pt4"
      puts $f "FFD_CONTROL_POINTS=0"
      puts $f "FFD_SURFACE_POINTS=0"
      incr id

    } else {

      set ideg [lindex $ffdDegree 0]
      set jdeg [lindex $ffdDegree 1]
      set kdeg [lindex $ffdDegree 2]

      set pt1  [lindex $cornerPoints 0]
      set pt2  [lindex $cornerPoints 1]
      set pt3  [lindex $cornerPoints 2]
      set pt4  [lindex $cornerPoints 3]
      set pt5  [lindex $cornerPoints 4]
      set pt6  [lindex $cornerPoints 5]
      set pt7  [lindex $cornerPoints 6]
      set pt8  [lindex $cornerPoints 7]

      puts $f "FFD_TAG= $id"
      puts $f "FFD_LEVEL= 0"
      puts $f "FFD_DEGREE_I= $ideg"
      puts $f "FFD_DEGREE_J= $jdeg"
      puts $f "FFD_DEGREE_K= $kdeg"
      puts $f "FFD_PARENTS= 0"
      puts $f "FFD_CHILDREN= 0"
      puts $f "FFD_CORNER_POINTS=8"
      puts $f "$pt1"
      puts $f "$pt2"
      puts $f "$pt3"
      puts $f "$pt4"
      puts $f "$pt5"
      puts $f "$pt6"
      puts $f "$pt7"
      puts $f "$pt8"
      puts $f "FFD_CONTROL_POINTS=0"
      puts $f "FFD_SURFACE_POINTS=0"
      incr id

    }

  }

  close $f

}

# Find the relative complement of list2 with respect to list1
proc ListComplement { list1 list2 } {

  set common [list]

  foreach item $list1 {

    set found 0

    if {[lsearch -exact $list2 $item] < 0} {
      set found 1
    }

    if {$found} {
      lappend common $item
    }

  }

  return $common

}

# Return CAE solver dimension
proc GetDimension {} {

  return [pw::Application getCAESolverDimension]

}

# MAIN

set allFFDGroups  [GetFFDGroups]
set fullFFDLayers [GetFullFFDLayers]

if {![llength $allFFDGroups] && ![llength $fullFFDLayers]} {

  puts "No FFD groups or FFD layers found."
  exit

} else {

  set allFFDGroupNames [list]
  set allFFDLayerNames [list]
  set ffdGroups        [list]

  foreach ffdGroup $allFFDGroups {
    lappend allFFDGroupNames [GetFFDNameFromGroup $ffdGroup]
  }
  foreach ffdLayer $fullFFDLayers {
    lappend allFFDLayerNames [GetFFDNameFromLayer $ffdLayer]
  }

  set ffdGroupNames  [ListComplement $allFFDGroupNames $allFFDLayerNames]

  foreach ffdGroupName $ffdGroupNames {
    lappend ffdGroups [GetFFDGroupFromName $ffdGroupName]
  }

}

if {![llength $ffdGroups]} {
  ExportFFD
  exit
}

set originalLayer [pw::Display getCurrentLayer]

# Loop through each FFD group, create the structured domain or block
# and assign the domain or block to a free layer.
foreach group $ffdGroups {

  set layer [GetFreeLayer]
  pw::Display setCurrentLayer $layer

  set ffdParams [GetParameters $group]

  set ffdName  [lindex $ffdParams 0]
  set ffdScale [lindex $ffdParams 1]
  set ffdDim   [lindex $ffdParams 2]
  set ffdDoms  [lindex $ffdParams 3]

  set extents [ScaleExtents [GetExtents $ffdDoms] $ffdScale]
  set ffd     [CreateBlock $extents $ffdDim]

  pw::Layer setDescription $layer "FFD-$ffdName"

}

pw::Display setCurrentLayer $originalLayer

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
