#
# Copyright 2014 (c) Pointwise, Inc.
# All rights reserved.
#
# This sample Pointwise script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#

#############################################################################
##
## ScaledOffset.glf
##
## CREATE SCALED, OFFSET COPY OF CONNECTOR LOOP CENTERED AT CENTROID
## 
## This script automates the creation of a roughly offset loop of connectors by 
## applying a uniform scaling to the copied loop with the anchor placed at the
## geometric center of the original loop. This can be useful when trying to 
## create internal topology within an outer loop, or a discrete boundary layer
## O-block from a closed loop of connectors on the surface.
## 
## The parameter $alpha is the scaling factor (>1 is bigger than original loop)
##
## For maximum productivity, a GUI was excluded. Simply select a loop of 
## connectors and then run the script, or run the script and then select the 
## connectors. 
## 
#############################################################################

package require PWI_Glyph 2

## Scale factor (>1 implies new loop will be bigger than original, < 1 smaller)
set alpha 0.75

## Returns intersection of two lists
proc intersectLists { list1 list2 } {
  set output [list]
  foreach item $list1 {
    set result [lsearch $list2 $item]
    if {$result != -1} {
      lappend output $item
    }
  }
  
  return $output
}

## Check that connectors form singly-connected loop
proc isLoop { conList } {
  set e [pw::Edge createFromConnectors -single $conList]
  if { [llength $e] != 1 } {
    puts "Connectors do not form a closed loop or single chain"
    foreach edge $e {
      $edge delete
    }
    return -1
  }

  set closed [$e isClosed]
  set cons [list]
  set nodes [list]
  for { set i 1 } { $i <= [$e getConnectorCount] } { incr i } {
    set c [$e getConnector $i]
    
    lappend cons $c
    if { [$e getConnectorOrientation $i] == "Same" } {
      lappend nodes [$c getNode Begin]
    } else {
      lappend nodes [$c getNode End]
    }
  }
  $e delete
  
  return [list $closed $nodes $cons]
}

## Compute the area of a domain
proc computeArea { dom } {
    ## First determine cell type
    if { [$dom getType] == "pw::DomainStructured" } {
        set structured 1
    } else {
        set structured 0
    }

    ## Get total cell count for iteration loop
    set dim [$dom getCellCount]
    set domainDimension [$dom getDimensions]
    
    set centroid [list]
    set area [list]

    set exam [pw::Examine create DomainArea]
    $exam addEntity $dom
    $exam examine
    
    ## Manually step through cell-by-cell to get both centroid and area of each
    for {set jj 1} {$jj <= $dim} {incr jj} {
    
        if $structured {
            ## Must get ij index for structured domain
            set i_index [expr ($jj-1)%([lindex $domainDimension 0]-1)+1]
            set j_index [expr ($jj-1)/([lindex $domainDimension 0]-1)+1]
            set cellIndex "$i_index $j_index"
        } else {
            set cellIndex $jj
        }
    
        ## Compute cell centroid:
        set cellInds [$dom getCell $cellIndex]
        set cellCentroid [pwu::Vector3 zero]
        foreach ind $cellInds {
            set cellCentroid [pwu::Vector3 add $cellCentroid [$dom getXYZ $ind]]
        }
        set cellCentroid [pwu::Vector3 divide $cellCentroid [llength $cellInds]]
        
        ## Store centroid and area
        lappend centroid $cellCentroid
        lappend area [$exam getValue $dom $cellIndex]
    }
    
    $exam delete
    
    return [list $centroid $area]
}

## Create temporary unstructured domain to compute the area
proc getDomainArea { cons } {
    ## First disable unstructured initialization to save on time
    set initInterior [pw::DomainUnstructured getInitializeInterior]
    pw::DomainUnstructured setInitializeInterior 0 
    
    ## Begin mode to easily delete any constructed entities
    set tmpMode [pw::Application begin Create]
    
    ## First try to create a planar domain from the connectors
    ## If it fails, look for an existing domain
    set tmpDom [pw::DomainUnstructured createFromConnectors $cons]
    if {$tmpDom != "" } {
        $tmpDom setUnstructuredSolverAttribute ShapeConstraint Free
        set temp [computeArea $tmpDom]
    } else {
        puts "Domain already exists."
            
        set counter 0
        
        set cc [lindex $cons 0]
        set domList [pw::Domain getDomainsFromConnectors $cc]
        
        foreach cc $cons {
            set newList [pw::Domain getDomainsFromConnectors $cc]
            set sharedEnts [intersectLists $domList $newList]
        }
        
        foreach ss $sharedEnts {
            set outerEdge [$ss getEdge 1]
            if { [lsearch $cons [$outerEdge getConnector 1]] != -1 } {
                set temp [computeArea $ss]
                break
            }
        }
    }
    
    ## Abort mode to delete and constructed domains
    $tmpMode abort
    
    ## Reset unstructured initialization setting
    pw::DomainUnstructured setInitializeInterior $initInterior
    
    if {[info exists temp]} {
        return $temp
    } else {
        puts "Unable to create/find suitable domain."
        exit
    }
}

## Find centroid of connectors by computing average centroid
proc findCenter { cons } {
    set temp [getDomainArea $cons]
    
    set centroid [lindex $temp 0]
    set area [lindex $temp 1]
    
    set N [llength $area]
    
    set cntr [pwu::Vector3 zero]
    set totA 0.0
    
    for {set ii 0} {$ii < $N} {incr ii} {
        set vec [lindex $centroid $ii]
        set A [lindex $area $ii]
        set tmp [pwu::Vector3 scale $vec $A]
        set cntr [pwu::Vector3 add $cntr $tmp]
        set totA [expr $totA+[lindex $area $ii]]
    }
    
    set cntr [pwu::Vector3 divide $cntr $totA]
    
    return $cntr
}

set text1 "Please select connector loop to offset."
set mask [pw::Display createSelectionMask -requireConnector {}]

###############################################
## This script uses the getSelectedEntities command added in 17.2R2
## Catch statement should check for previous versions
if { [catch {pw::Display getSelectedEntities -selectionmask $mask curSelection}] } {
    puts {Command "getSelectedEntities" does not exist}

    set picked [pw::Display selectEntities -description $text1 \
        -selectionmask $mask curSelection]
    
    if {!$picked} {
        puts "Script aborted."
        exit
    }
} elseif { [llength $curSelection(Connectors)] == 0 } {
    set picked [pw::Display selectEntities -description $text1 \
        -selectionmask $mask curSelection]
    
    if {!$picked} {
        puts "Script aborted."
        exit
    }
}
###############################################

set temp [isLoop $curSelection(Connectors)]
set QLoop [lindex $temp 0]
set nodes [lindex $temp 1]
set cons [lindex $temp 2]

## Can only scale a closed loop, i.e. QLoop = 1
if {$QLoop==0 || $QLoop == -1} {
    puts "No loop present, script aborted."
    exit
}

set center [findCenter $cons]

pw::Application clearClipboard
pw::Application setClipboard $cons
set pasteMode [pw::Application begin Paste]
set pastedEnts [$pasteMode getEntities]
set modMode [pw::Application begin Modify $pastedEnts]
pw::Entity transform [pwu::Transform scaling -anchor $center \
    "$alpha $alpha $alpha"] [$modMode getEntities]

$modMode end
unset modMode
$pasteMode end
unset pasteMode

#
# DISCLAIMER:
# TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW, POINTWISE DISCLAIMS
# ALL WARRANTIES, EITHER EXPRESS OR IMPLIED, INCLUDING, BUT NOT LIMITED
# TO, IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE, WITH REGARD TO THIS SCRIPT. TO THE MAXIMUM EXTENT PERMITTED
# BY APPLICABLE LAW, IN NO EVENT SHALL POINTWISE BE LIABLE TO ANY PARTY
# FOR ANY SPECIAL, INCIDENTAL, INDIRECT, OR CONSEQUENTIAL DAMAGES
# WHATSOEVER (INCLUDING, WITHOUT LIMITATION, DAMAGES FOR LOSS OF
# BUSINESS INFORMATION, OR ANY OTHER PECUNIARY LOSS) ARISING OUT OF THE
# USE OF OR INABILITY TO USE THIS SCRIPT EVEN IF POINTWISE HAS BEEN
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGES AND REGARDLESS OF THE
# FAULT OR NEGLIGENCE OF POINTWISE.
#

