#
# Copyright 2013 (c) Pointwise, Inc.
# All rights reserved.
# 
# This sample Pointwise script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.  
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#

###############################################################################
##
## ButterflyMaker.glf
##
## Script with Tk interface that inserts a butterfly topology into selected
## structured blocks.
##
###############################################################################

package require PWI_Glyph 2.3

## GLOBAL VARIABLES ##
# GUI title and header strings
set guiTitle "Create Butterfly Topology" 
set guiHeader "Transform H Topology Block(s) into Butterfly Topology"
set guiGroupPrefix "GROUP: "

# Global creator set flag
set creation_mode 0
set isCreatorSet false
set oFraction .25
set oDimension 10
set Last_Job_Type "None"
set Cancel_Job false
set Error_Last_Job false
set Job_Running false
set oScaleFac [list 0.5 0.5 0.5]
set locatorH_prop {}
set End_Script false

# Inital direction
set Direction "I"
set dupDirection "I"
set prevDirection "I"

# Global entity lists
set Structured_Blocks {}
set Prev_Structured_Blocks {}
set Transformed_Blocks {}
set ListBoxBlocks {}
set ListBoxGroups {}
set MasterAlignBlocks {}
set ListBoxSelectableItems {}
set Original_EntRenderAtt {}
set Prev_Highlight_Selection {}

# Special entity collections
set tempEntCollection [pw::Collection create]
set prevOgridCollection [pw::Collection create]

# Gui and Color Constants
set defDomainColor #008F00
set defColor #00AAFF
set defWidth 1
set defColorMode Automatic
set defSelectColorMode Entity
set defSelectColor #FF7878
set defSelectWidth 2
set defListBoxStringWidth 20

###############################################################################
# blkGetIJK: return blocks ijk value based on direction
###############################################################################
proc blkGetIJK { dir ind1 ind2 ind3 } {
  if {$dir == "I"} {
    set i $ind3
    set j $ind1
    set k $ind2
  } elseif {$dir == "J"} {
    set j $ind3
    set i $ind1
    set k $ind2
  } else {
    set k $ind3
    set i $ind1
    set j $ind2
  }
  return "$i $j $k"
}

###############################################################################
# copyConDistribution: copy edge's distribution and run it to a connector
# - Distribution might need to be reversed; logic handles this by creating an
#   edge from the connectors in edge (this is done to ensure they are in order).
#   Then, the first and last point of con and the new edge are compared. If the
#   first and last are on opposite ends, the distribution is reversed before it
#   is applied.
###############################################################################
proc copyConDistribution { con edge } {
  # Form edge from connectors
  set tmpEdges [pw::Edge createFromConnectors -single $edge]

  # For all the edges, gather up the connectors
  foreach tEdge $tmpEdges {
    set conCount [$tEdge getConnectorCount]
    for {set i 1} {$i <= $conCount} {incr i} {
        lappend conList [$tmpEdges getConnector $i]
    }
  }

  # Use these connectors to create the distribution
  set dist [pw::DistributionGeneral create $conList]

  set c2ptA [$tmpEdges getPoint 1]
  set c2ptB [$tmpEdges getPoint [$tmpEdges getDimension]]
  set c1ptA [$con getXYZ -arc 0.0]
  set c1ptB [$con getXYZ -arc 1.0]
  set cp1 [$con closestPoint $c2ptA]
  set cp2 [$con closestPoint $c2ptB]
  set tolerance 0.0005

  # Compare first and last point of con and new edge to see if distribution
  # should be reversed
  if { [pwu::Vector3 equal -tolerance $tolerance $cp1 $c1ptB] &&
       [pwu::Vector3 equal -tolerance $tolerance $cp2 $c1ptA]} {
      $dist reverse
  }

  # Set distribution for a con's subconnectors
  set numSubConnectors [$con getSubConnectorCount]
  for {set i 1} {$i <= $numSubConnectors} {incr i} {
    $con setDistribution -lockEnds $i $dist
  }
}


###############################################################################
# getDist: use Glyph utility commands pwu::Vector3 subtract and length 
###############################################################################
proc getDist {pt1 pt2} {
   set 2sub1 [pwu::Vector3 subtract $pt2 $pt1]
   set dist [pwu::Vector3 length $2sub1]
   return $dist
}

###############################################################################
# getConByNode: given a pt and a connector list, return the connector and pt
###############################################################################
proc getConByNode { pt conList } {
  set tol [pw::Grid getGridPointTolerance]

  foreach con $conList { 
    set pta [$con getXYZ -arc 0]
    set ptb [$con getXYZ -arc 1]

    set dist_a [getDist $pt $pta]
    set dist_b [getDist $pt $ptb]
   
    if {$dist_a < $tol} {
          return "$con $ptb"
    } elseif {$dist_b < $tol} {
          return "$con $pta"
    } 
  }
}

###############################################################################
# GetBlkEdgeCons: 
# - Compares two faces on a given block and returns a list of shared connectors
###############################################################################
proc GetBlkEdgeCons {blk face1 face2} {
  set face_2_BoundCons {}
  set tmpFace2 [$blk getFace $face2]
  set tmpFace1 [$blk getFace $face1]
  set dom_2_count [$tmpFace2 getDomainCount]

  # get All domains of face2
  #   get All edges of all domains
  #     get All connectors of all edges
  # append all connectors to face_2_BoundCons
  for {set i 1} {$i <= $dom_2_count} {incr i} {
    set dom [$tmpFace2 getDomain $i]
    set dom_edge_count [$dom getEdgeCount]

    for {set j 1} {$j <= $dom_edge_count} {incr j} {
      set edge [$dom getEdge $j]
      set edge_con_count [$edge getConnectorCount]

      for {set k 1} {$k <= $edge_con_count} {incr k} {
        set con [$edge getConnector $k]
        if {[lsearch $face_2_BoundCons $con] == -1} {
          lappend face_2_BoundCons $con
        }
      }
    }
  }

  set sharingCons {}
  set dom_1_count [$tmpFace1 getDomainCount]

  # get All domains of face1
  #   get All edges of all domains
  #     get All connectors of all edges
  # append all connectors to sharingCons
  for {set i 1} {$i <= $dom_1_count} {incr i} {
    set dom [$tmpFace1 getDomain $i]
    set dom_edge_count [$dom getEdgeCount]

    for {set j 1} {$j <= $dom_edge_count} {incr j} {
      set edge [$dom getEdge $j]
      set edge_con_count [$edge getConnectorCount]

      for {set k 1} {$k <= $edge_con_count} {incr k} {
        set con [$edge getConnector $k]
        if {[lsearch $face_2_BoundCons $con] >= 0} {
          lappend sharingCons $con
        }
      }
    }
  }
    
  if {[llength $sharingCons] == 0} {
    set msg "Cannot find sharing edge of $blk between $face1 and $face2."
    return -code error $msg
  }

  return $sharingCons
}

###############################################################################
# createBlockFromDomainsAndFace:
# - Helper function for blkMakeBflyBlocks 
# - Creates a block from a list of domains and a list of faces
# - Ensures that domain list is flattened before block is created
###############################################################################
proc createBlockFromDomainsAndFace { domList faceList } {
  set resultList {}

  foreach dom $domList {
    lappend resultList $dom
  }

  if {[llength $faceList] > 0} { 
    set faceDomList [getFaceDomains $faceList]

    lappend resultList $faceDomList
  }

  # Create new structured block
  set blk [pw::BlockStructured create]

  # Create face with domain, then add face to block
  foreach dom $resultList {
    $blk addFace [pw::FaceStructured createFromDomains $dom]
  }

  return $blk
}

###############################################################################
# blkMakeBflyBlocks:
# - Given a Block and direction, transforms H Topology to Butterfly topology
###############################################################################
proc blkMakeBflyBlocks { blk dir } {
  global tempEntCollection
  set blkInfo [$blk getDimensions]

  set id [lindex $blkInfo 0]
  set jd [lindex $blkInfo 1]
  set kd [lindex $blkInfo 2]

  # Determine the butterfly face IDs, ind3_min_face and ind3_max_face.
  # The dimension in the propagating direction is ind3_max.
  if {$dir == "I"} {
    set max1 $jd
    set max2 $kd
    set ind3_max $id
    set ind1_min_face JMinimum
    set ind1_max_face JMaximum
    set ind2_min_face KMinimum
    set ind2_max_face KMaximum
    set ind3_min_face IMinimum
    set ind3_max_face IMaximum
  } elseif {$dir == "J"} {
    set max1 $id
    set max2 $kd
    set ind3_max $jd
    set ind1_min_face IMinimum
    set ind1_max_face IMaximum
    set ind2_min_face KMinimum
    set ind2_max_face KMaximum
    set ind3_min_face JMinimum
    set ind3_max_face JMaximum
  } else {
    set max1 $id
    set max2 $jd
    set ind3_max $kd
    set ind1_min_face IMinimum
    set ind1_max_face IMaximum
    set ind2_min_face JMinimum
    set ind2_max_face JMaximum
    set ind3_min_face KMinimum
    set ind3_max_face KMaximum
  }
  if {$dir == "ALL"} {
   set capMin 1
   set capMax 1
  } else {
   set capMin 0
   set capMax 0
  }

  if {[blkMakeBflyDomains $blk $dir doms] == 0} {
   return 0;
  }

  # Get faces according to ind1, ind2, ind3 - min & max
  set face1 [$blk getFace $ind2_min_face]
  set face2 [$blk getFace $ind1_max_face]
  set face3 [$blk getFace $ind2_max_face]
  set face4 [$blk getFace $ind1_min_face]
  set face5 [$blk getFace $ind3_min_face]
  set face6 [$blk getFace $ind3_max_face]

  $tempEntCollection add [$face5 getDomains]
  $tempEntCollection add [$face6 getDomains]

  set base [$blk getName]
  #-- create domain list for blk(center)
  set tmpDomainList [list $doms(center,ind3_min) $doms(center,ind3_max) \
    $doms(center,ind1_min) $doms(center,ind1_max) $doms(center,ind2_min) \
    $doms(center,ind2_max)]

  #-- Center block
  set blks(center) [createBlockFromDomainsAndFace $tmpDomainList {} ]

  #-- create domain list for blk(ogrid1)
  set tmpDomainList [list $doms(ogrid1,ind3_min) $doms(ogrid1,ind3_max) \
    $doms(center,ind2_min) $doms(corner1) $doms(corner2)]

  #-- Ogrid 1 (ind2_min) block
  set blks(ogrid1) [createBlockFromDomainsAndFace $tmpDomainList $face1]   

  #-- create domain list for blk(ogrid2)
  set tmpDomainList [list $doms(ogrid2,ind3_min) $doms(ogrid2,ind3_max) \
    $doms(center,ind1_max) $doms(corner2) $doms(corner3)]
  
  #-- Ogrid 2 (ind1_max) block
  set blks(ogrid2) [createBlockFromDomainsAndFace $tmpDomainList $face2]
  
  #-- create domain list for blk(ogrid3)
  set tmpDomainList [list $doms(ogrid3,ind3_min) $doms(ogrid3,ind3_max) \
    $doms(center,ind2_max) $doms(corner3) $doms(corner4)]
  
  #-- Ogrid 3 (ind2_max) block
  set blks(ogrid3) [createBlockFromDomainsAndFace $tmpDomainList $face3]

  #-- create domain list for blk(Ogrid4)
  set tmpDomainList [list $doms(ogrid4,ind3_min) $doms(ogrid4,ind3_max) \
    $doms(center,ind1_min) $doms(corner4) $doms(corner1)]

  #-- Ogrid 4 (ind1_min) block
  set blks(ogrid4) [createBlockFromDomainsAndFace $tmpDomainList $face4]
  
  set base [string trim $base]

  $blks(center) setName "${base}_center" 
  $blks(ogrid1) setName "${base}_ogrid1"
  $blks(ogrid2) setName "${base}_ogrid2"
  $blks(ogrid3) setName "${base}_ogrid3"
  $blks(ogrid4) setName "${base}_ogrid4"

  if {$capMin} {
    #-- Create domains for blks(capmin)
    set tmpDomainList [list $doms(center,ind3_min) $doms(ogrid1,ind3_min) \
      $doms(ogrid2,ind3_min) $doms(ogrid3,ind3_min) $doms(ogrid4,ind3_min)]

    #-- Cap min (ind3_min) block
    set blks(capmin) [createBlockFromDomainsAndFace $tmpDomainList $face5]

    #-- Set name of capmin (ind3_min) block
    $blks(capmin) setName "${base}_capmin"
  }

  if {$capMax} {
    #-- Cap max (ind3_max) block
    set tmpDomainList [list $doms(center,ind3_max) $doms(ogrid1,ind3_max) \
        $doms(ogrid2,ind3_max) $doms(ogrid3,ind3_max) $doms(ogrid4,ind3_max)]

    #-- Cap max (ind3_min) blockS
    set blks(capmax) [createBlockFromDomainsAndFace $tmpDomainList $face6]

    #-- Set name of capmax (ind3_min) block
    $blks(capmax) setName "${base}_capmax"
  }
      
  return 1
}

###############################################################################
# oneEdgeCreated:
# - Helper function for makeDomains 
# - Creates edges from connector list 
# - Those edges stored in named reference edgeVar
# - Returns false if more than one edge created
###############################################################################
proc oneEdgeCreated { edgeVar conList } {
  upvar $edgeVar tmpEdge

  set tmpEdge [pw::Edge createFromConnectors -single $conList]

  set result true

  # If more than one edge created, return false
  if {[llength $tmpEdge]  > 1} {
    set result false
  }

  return $result
}

###############################################################################
# makeConnector: blkMakeBlfyDomains helper function
###############################################################################
proc makeConnector { con dim } {
  set pt0 [$con getXYZ -arc 0]
  set pt1 [$con getXYZ -arc 1]

  $con setDimension $dim

  if [catch {$con -arc 0}] {
    set $con [getConnectorByEndPoints $pt0 $pt1]
  } 
}

###############################################################################
# makeDomains:
# - Helper function for blkMakeBlfyDomains
# - Creates domains
# - conList -> list of primary connectors to create domain
###############################################################################
proc makeDomains { domVar conList } {
  set tmpEdgeList {}
  upvar $domVar tmpDom

  foreach con $conList {
    if {[oneEdgeCreated edge $con]} {
      lappend tmpEdgeList $edge
    } else {
      set msg "Connectors could not be merged into single edge."
      return -code error $msg
    }
  }

  # Check that edges are valid before creating domain
  set tmpDom [pw::DomainStructured create]
  foreach edge $tmpEdgeList {
    if {[catch {$tmpDom addEdge $edge} msg]} {
      if {[string compare $msg "ERROR: (EXCEPTION) Edge is undefined"]} {
        return -code error \
          "Error: Edge undefined. \nA connector's dimension may be to small."
      }
    }
  }

  if {![$tmpDom isValid]} {
    pw::Entity delete $tmpDom
    # If tmpEdgeList does not "qualify" for a structured domain, then try to
    # find an existing domain using corner nodes of connectors in conList
    set tmpDom [getDomainByCorners \
      [[lindex $conList 0] getXYZ -arc 0] \
      [[lindex $conList 0] getXYZ -arc 1] \
      [[lindex $conList 2] getXYZ -arc 0] \
      [[lindex $conList 2] getXYZ -arc 1]] 
  }

  return 1;
}

###############################################################################
# projectDomainList:
# - Helper function for blkMakeBflyDomains  
# - Projects a domain onto a given database
# - If cannot find appropriate db for projection, uses argument database for 
#   projection
###############################################################################
proc projectDomainList { domain database} {
  set domainProjectable true
  set projectionDBList { }
  set ptCount [$domain getPointCount -constrained constrainedPtCount]

  # If all points are db constrained, try to project onto that database
  if {$ptCount == $constrainedPtCount} {
    
    for {set i 1} {$i <= $ptCount} {incr i} {
      set dbEntity [lindex [$domain getPoint $i] 2]
      if {[lsearch $projectionDBList $dbEntity] == -1} {
        if {[$dbEntity isBaseForProject]} {
          # If dbEntity is unique and projectable append it to dbEntityList
          lappend projectionDBList $dbEntity
        } else {
          # Since one of the databases is not projectable, 
          set domainProjectable false
          break
        }
      }
    } 
  # Otherwise, matching domain is not projectable
  } else {
    set domainProjectable false
  }

  if {!$domainProjectable} {
    set projectionDBList $database
  }

  $domain project -type ClosestPoint $projectionDBList
}

###############################################################################
# blkMakeBflyDomains:
# - Accepts a Block, Direction, and named reference to a domain array
# - Create butterfly block domains
###############################################################################
proc blkMakeBflyDomains { blk dir domVar } {
  global oFraction oDimension butterflyDBArr defDomainColor \
    prevOgridCollection butterflyDomList

  set butterflyDomList {}
  set splitEdges 1
  upvar $domVar doms

  if {$dir == "ALL"} {
   set capMin 1
   set capMax 1
  } else { 
   set capMin 0
   set capMax 0
  }

  #if not a structured block, return
  if {![$blk isOfType pw::BlockStructured]} {
     return 0
  }

  set blkInfo [$blk getDimensions]

  set id [lindex $blkInfo 0]
  set jd [lindex $blkInfo 1]
  set kd [lindex $blkInfo 2]

  # ind3_min -- the minimum I/J/K index to start with. It is always 1.
  # ind3_min_face and ind3_max_face are the butterfly faces.
        
  set ind3_min 1
  if {$dir == "I"} {
    set max1 $jd
    set max2 $kd
    set ind3_max $id
    set ind1_min_face JMinimum
    set ind1_max_face JMaximum
    set ind2_min_face KMinimum
    set ind2_max_face KMaximum
    set ind3_min_face IMinimum
    set ind3_max_face IMaximum
  } elseif {$dir == "J"} {
    set max1 $id
    set max2 $kd
    set ind3_max $jd
    set ind1_min_face IMinimum
    set ind1_max_face IMaximum
    set ind2_min_face KMinimum
    set ind2_max_face KMaximum
    set ind3_min_face JMinimum
    set ind3_max_face JMaximum
  } else {

    # Note this is applied to two cases: K and ALL modes.
    set max1 $id
    set max2 $jd
    set ind3_max $kd
    set ind1_min_face IMinimum
    set ind1_max_face IMaximum
    set ind2_min_face JMinimum
    set ind2_max_face JMaximum
    set ind3_min_face KMinimum
    set ind3_max_face KMaximum
  }

  # Determine the other 2 indices (J and K) of H domain given one (I) of 
  # butterfly faces. 
  #
  #     Jmax    o---------------o               o---------------o 
  #             |               |               |               |        
  #             |               |               |               |        
  #             |               |               |               |        
  #             |ind1_max       |               |ind2_min       |        
  #             |   o-------o   |               |   o-------o   |        
  #             |   |       |   |               |   |       |   |        
  #J index of H |   |       |   |               |   |       |   | K index of H        
  #             |   |       |   |               |   |       |   |        
  #             |   o-------o   |               |   o-------o   |        
  #             |ind1_min       |               |       ind2_max|        
  #             |               |               |               |        
  #             |               |               |               |        
  #     Jmin=1  o---------------o       Kmin=1  o---------------o   Kmax
  #
  #                Imin butterfly face                Imax butterfly face
  #
  #

  if {$max1 < $max2} {
    set ogrid_i [expr int($max1*$oFraction)]
    set id2 [expr $max1/2]
    if {$ogrid_i > $id2} {
      set ogrid_i $id2
    }
    if {$ogrid_i < 2 } {
      set ogrid_i 2
    }
  } else {
    set ogrid_i [expr int($max2*$oFraction)]
    set jd2 [expr $max2/2]
    if {$ogrid_i > $jd2} {
      set ogrid_i $jd2
    }
    if {$ogrid_i < 2 } {
      set ogrid_i 2
    }
  }
  set ogrid_j $ogrid_i

  set ind1_min $ogrid_i
  set ind1_max [expr $max1-$ogrid_i+1]
  set ind2_min $ogrid_j
  set ind2_max [expr $max2-$ogrid_j+1]

  #-- Create connectors

  blkMakeBflyConnectors $blk $dir con
  
  # Michael: Reduced with makeConnector 
  # Create 4 connectors based on the four nodes of H domain.
  foreach end {ind3_min ind3_max} {
    foreach beg {ind1_min ind1_max} {    
      makeConnector $con($beg,$end) $max2
    }
    foreach beg {ind2_min ind2_max} {    
      makeConnector $con($beg,$end) $max1
    }
    foreach beg {corner1 corner2 corner3 corner4} {    
      makeConnector $con($beg,$end) $oDimension 
    }
  }

  foreach end {ind1_min ind1_max} {
    foreach beg {ind2_min ind2_max} {    
      makeConnector $con($beg,$end) $ind3_max
    }
  }

  # Create the 2 H domains on butterfly faces, say, Imin and Imax if I selected.
  # con(ind2_min,ind3_min) -- Kmin H connector on Imin face.
  # con(ind1_max,ind3_min) -- Jmax H connector on Imin face.
  # con(ind2_max,ind3_min) -- Kmax H connector on Imin face.
  # con(ind1_min,ind3_min) -- Jmin H connector on Imin face.
  # Please note they have to be added in order such that a closed perimeter 
  # loop is created.
  #
  #                    Jmax Con
  #                o---------------o
  #                |               |
  #                |               |
  #                |               |
  #        Kmin Con|               | Kmax Con
  #                |               |
  #                |               |
  #                o---------------o
  #                    Jmin Con
  #
  #-- Center domains
  #                                                                           
  #                                                                           
  #                                                                           
  #                                                                           
  #                                 ind2_min                                  
  #                                 | | | |               ind3_min            
  #                            o----+-+-+-+-------------o / / /               
  #                          /      | | | |           . | / /                 
  #                        /        v v v v         . . | /                   
  #                      /                        . . . |                     
  #                    /                        . . . . |                     
  #                   o-----------------------o . . .   |                     
  #                   | \       ogrid1      . | . .     |                     
  #                   |   \               . . | .       |                     
  #                ---|     o-----------o . . |       <-+--------             
  #     ind1_max -----|     |         . | . . |     <---+------ ind1_min      
  #           --------|     |       . . | .   |   <-----+----                 
  #                   |     |     . . . |     |         |                     
  #                   ogrid2|   * * * * |ogrid4         |                     
  #                   |     | / / / /   |     |         o                     
  #                   |     / / / /     |     |       /                       
  #                   |   ind3_max -----o     |     /                         
  #                   |   /               \   |   /                           
  #                   | /       ogrid3      \ | /                             
  #                   o-----------------------o                               
  #                                 | | | |                                   
  #                                 | | | |                                   
  #                                 ind2_max                                  
  #                                                                           
  #                                                                           


  # Create doms(center,ind3_min)
  set conList [list \
    $con(ind2_min,ind3_min) $con(ind1_max,ind3_min) \
    $con(ind2_max,ind3_min) $con(ind1_min,ind3_min)]

  makeDomains doms(center,ind3_min) $conList

  # Create doms(center,ind3_max) 
  set conList [list \
    $con(ind2_min,ind3_max) $con(ind1_max,ind3_max) \
    $con(ind2_max,ind3_max) $con(ind1_min,ind3_max)]

  makeDomains doms(center,ind3_max) $conList

  # Create Jmin domain(s) of the center block if I mode is selected.
  #-- ind1_min face
  set conList [list \
    $con(ind2_min,ind1_min) $con(ind1_min,ind3_max) \
    $con(ind2_max,ind1_min) $con(ind1_min,ind3_min)]

  makeDomains doms(center,ind1_min) $conList

  # Create Jmax domain(s) of the center blockif I mode is selected.
  #-- ind1_max face
  set conList [list \
    $con(ind2_min,ind1_max) $con(ind1_max,ind3_max) \
    $con(ind2_max,ind1_max) $con(ind1_max,ind3_min)]

  makeDomains doms(center,ind1_max) $conList

  # Create Kmin domain(s) of the center block if I mode is selected.
  #-- ind2_min face
  set conList [list \
    $con(ind2_min,ind1_min) $con(ind2_min,ind3_max) \
    $con(ind2_min,ind1_max) $con(ind2_min,ind3_min)]

  makeDomains doms(center,ind2_min) $conList

  # Create Kmin domain(s) of the center block if I mode is selected.
  #-- ind2_max face
  set conList [list \
    $con(ind2_max,ind1_min) $con(ind2_max,ind3_max) \
    $con(ind2_max,ind1_max) $con(ind2_max,ind3_min)]

  makeDomains doms(center,ind2_max) $conList

  # Create 4 Ogrid domains on the 1st butterfly face, say Imin if I mode is 
  # selected.
  #-- Ogrid domain 1 ind3_min face
  set edge1 [GetBlkEdgeCons $blk $ind2_min_face $ind3_min_face]

  set conList [list \
    $con(corner2,ind3_min) $con(ind2_min,ind3_min) \
    $con(corner1,ind3_min) $edge1]

  makeDomains doms(ogrid1,ind3_min) $conList

  #-- Ogrid domain 2 ind3_min face
  set edge1 [GetBlkEdgeCons $blk $ind1_max_face $ind3_min_face]

  set conList [list \
    $con(corner3,ind3_min) $con(ind1_max,ind3_min) \
    $con(corner2,ind3_min) $edge1]

  makeDomains doms(ogrid2,ind3_min) $conList

  #-- Ogrid domain 3 ind3_min face
  set edge1 [GetBlkEdgeCons $blk $ind2_max_face $ind3_min_face]

  set conList [list \
    $con(corner4,ind3_min) $con(ind2_max,ind3_min) \
    $con(corner3,ind3_min) $edge1]

  makeDomains doms(ogrid3,ind3_min) $conList

  #-- Ogrid domain 4 ind3_min face
  set edge1 [GetBlkEdgeCons $blk $ind1_min_face $ind3_min_face]

  set conList [list \
    $con(corner1,ind3_min) $con(ind1_min,ind3_min) \
    $con(corner4,ind3_min) $edge1]

  makeDomains doms(ogrid4,ind3_min) $conList

  # Create 4 Ogrid domains on the 2st butterfly face, say Imax if I mode is 
  # selected.
  #-- Ogrid domain 1 ind3_max face
  set edge1 [GetBlkEdgeCons $blk $ind2_min_face $ind3_max_face]

  set conList [list \
    $con(corner2,ind3_max) $con(ind2_min,ind3_max) \
    $con(corner1,ind3_max) $edge1]

  makeDomains doms(ogrid1,ind3_max) $conList

  #-- Ogrid domain 2 ind3_max face
  set edge1 [GetBlkEdgeCons $blk $ind1_max_face $ind3_max_face]

  set conList [list \
    $con(corner3,ind3_max) $con(ind1_max,ind3_max) \
    $con(corner2,ind3_max) $edge1]

  makeDomains doms(ogrid2,ind3_max) $conList

  #-- Ogrid domain 3 ind3_max face
  set edge1 [GetBlkEdgeCons $blk $ind2_max_face $ind3_max_face]

  set conList [list \
    $con(corner4,ind3_max) $con(ind2_max,ind3_max) \
    $con(corner3,ind3_max) $edge1]

  makeDomains doms(ogrid3,ind3_max) $conList

  #-- Ogrid domain 4 ind3_max face
  set edge1 [GetBlkEdgeCons $blk $ind1_min_face $ind3_max_face]

  set conList [list \
    $con(corner1,ind3_max) $con(ind1_min,ind3_max) \
    $con(corner4,ind3_max) $edge1]

  makeDomains doms(ogrid4,ind3_max) $conList

  # Domain
  #-- Corner 1 domain
  set edge1 [GetBlkEdgeCons $blk $ind2_min_face $ind1_min_face]

  # Copy the distribution of the corresponding edge of the block to the 
  # Butterfly connector that is perpendicular to the butterfly faces.
  copyConDistribution $con(ind2_min,ind1_min) $edge1

  set conList [list \
    $con(corner1,ind3_max) $con(ind2_min,ind1_min) \
    $con(corner1,ind3_min) $edge1]

  makeDomains doms(corner1) $conList

  #-- Corner 2 domain
  set edge1 [GetBlkEdgeCons $blk $ind2_min_face $ind1_max_face]

  # Copy the distribution of the corresponding edge of the block to the 
  # Butterfly connector that is perpendicular to the butterfly faces.
  copyConDistribution $con(ind2_min,ind1_max) $edge1

  set conList [list \
    $con(corner2,ind3_max) $con(ind2_min,ind1_max) \
    $con(corner2,ind3_min) $edge1]

  makeDomains doms(corner2) $conList

  #-- Corner 3 domain
  set edge1 [GetBlkEdgeCons $blk $ind2_max_face $ind1_max_face]

  # Copy the distribution of the corresponding edge of the block to the 
  # Butterfly connector that is perpendicular to the butterfly faces.
  copyConDistribution $con(ind2_max,ind1_max) $edge1

  set conList [list \
    $con(corner3,ind3_max) $con(ind2_max,ind1_max) \
    $con(corner3,ind3_min) $edge1]

  makeDomains doms(corner3) $conList

  #-- Corner 4 domain
  set edge1 [GetBlkEdgeCons $blk $ind2_max_face $ind1_min_face]

  # Copy the distribution of the corresponding edge of the block to the 
  # Butterfly connector that is perpendicular to the butterfly faces.
  copyConDistribution $con(ind2_max,ind1_min) $edge1

  set conList [list \
    $con(corner4,ind3_max) $con(ind2_max,ind1_min) \
    $con(corner4,ind3_min) $edge1]

  makeDomains doms(corner4) $conList

  #Project the created butterfly domains onto the original block surfaces.
  if {$dir == "I" || $dir == "J" || $dir == "K"} {
    $prevOgridCollection set [list $doms(center,ind3_min) \
      $doms(center,ind3_max) $doms(ogrid1,ind3_min) $doms(ogrid1,ind3_max) \
      $doms(ogrid2,ind3_min) $doms(ogrid2,ind3_max) $doms(ogrid3,ind3_min) \
      $doms(ogrid3,ind3_max) $doms(ogrid4,ind3_min) $doms(ogrid4,ind3_max)] 

    # Project domains onto database entity
    set butterflyDomList [$prevOgridCollection list]
    foreach domain $butterflyDomList {
      projectDomainList $domain $butterflyDBArr($blk)

      # Color domains so that they do not appear DB constrained. An alternative
      # would be to (1) delete the tmp db surfaces now (2) run elliptical solver
      # on these domains; however, we can't do that at this point due to mode
      # restrictions. So, just hide that they are db constrained.
      $domain setRenderAttribute ColorMode Entity
      $domain setColor $defDomainColor
    }
  }

  return 1
}

###############################################################################
# getCornerPointsForIndex:
# - This is a helper function function for HDomLocator
# - Returns corner points for length and spacing (calcualted by 
#   getLengthSpacingArr)
###############################################################################
proc getCornerPointsForIndex { length spacing max index } {
  global oScaleFac
  set tol [pw::Grid getGridPointTolerance]
  set cornerPts {}

  set targetLength_1 [expr $length *(1.0 - [lindex $oScaleFac $index]) / 2.0]
  set targetLength_2 [expr $length - $targetLength_1 ]
  set testLength 0.0
  for {set ii 1} {$ii < $max} {incr ii 1} {
    set tmpSpacing [lindex $spacing [expr $ii-1]]

    set testLength [expr $testLength + $tmpSpacing]
    if {[expr abs( $targetLength_1 - $testLength )] < $tol ||
     [expr abs( $targetLength_2 - $testLength )] < $tol ||
     [expr abs( $testLength - $targetLength_1)] < $tmpSpacing ||
     [expr abs( $testLength - $targetLength_2)] < $tmpSpacing} {

     lappend cornerPts [expr $ii+1]
    }
  }
  return $cornerPts
}

###############################################################################
# getLengthSpacingArr:
# - This is a helper function function for HDomLocator
# - Returns an array containing length and spacing for given index
###############################################################################
proc getLengthSpacingArr { blk index transformDir max } {
  set spacing {}
  set length 0.0
  set indexArr(I) 1
  set indexArr(J) 1
  set indexArr(K) 1

  for {set ii 1} {$ii < $max} {incr ii} {
    set indexArr($index) $ii
    set pt1 [$blk getXYZ [blkGetIJK $transformDir $indexArr(I) $indexArr(J) \
      $indexArr(K)]]
    set indexArr($index) [expr $ii + 1]
    set pt2 [$blk getXYZ [blkGetIJK $transformDir $indexArr(I) $indexArr(J) \
      $indexArr(K)]]
    set incr [getDist $pt1 $pt2] 
    set length [expr $length + $incr]
    lappend spacing $incr
  }

  set lengthSpacingArr(length) $length
  set lengthSpacingArr(spacing) $spacing

  return [array get lengthSpacingArr]
}

###############################################################################
# HDomLocator:
#  - Replace the old guessing scheme below which requires domain joining if 
#   multidom butterfly faces occurs. That is not reliable when the topology 
#   gets complicated.
###############################################################################
proc HDomLocator { blk dir max1 max2 ind3_min ind3_max } {
  global oScaleFac Propagate locatorH_prop Preview  
  set tol [pw::Grid getGridPointTolerance]

  if {$Propagate} {
    set propagateBlks [getPropagatedBlockList $blk $dir]
  } else {
    set propagateBlks $blk
  }
  set propBlks {}

  foreach blkDir $propagateBlks {
    lappend propBlks [lindex $blkDir 0]
  }

  # For propagate mode only. If H location is already obtained for the first 
  # block in the propagating block list, the old location information will be 
  # returned. This method saves tremendous time if the topology change will be 
  # propagated through many blocks.
  if {$Propagate == 1 && [llength $locatorH_prop] > 1 && $dir != "ALL" && \
    [lsearch $locatorH_prop $blk] > 3} {
    # Skip the H calculation if it is already calculated for one block among 
    # this propagating set. Return the first four elements of the list as the 
    # location.
    return [lrange $locatorH_prop 0 3]
  }
  #
  # Improve the performance by simlifying the method of obtaining the 
  # ogrid_i/j/k. The old method above has to go through lots of iterations to 
  # decide the cooresponding ijk index for the H region. Moreover, its guessing 
  # scheme is based on the normalized dimention so the H region location highly 
  # depends on the connector distribution. 
  #
  # Determine the 2 indices (J and K) of H domain given one (I) of butterfly 
  # faces. 
  #
  #
  #                     index3   index4
  #     Jmax    o---?-------?---o               o---------------o 
  #             |               |               |               |        
  #             |               |               |               |        
  #             |               |               |               |        
  #             |               |               |               |        
  #     index2  ?   o-------o   |               |   o-------o   |        
  #             |   |       |   |               |   |       |   |        
  #             |   |       |   |               |   |       |   | 
  #             |   |       |   |               |   |       |   |        
  #     index1  ?   o-------o   |               |   o-------o   |        
  #             |               |               |               |        
  #             |               |               |               |        
  #             |               |               |               |        
  #     Jmin=1  o---------------o       Kmin=1  o---------------o   Kmax
  #
  #
  # Obtain the grid points, index 1~4, and use their JK indices to locate the 
  # H region.
  # Case study: max1 VS ind1_min and ind1_max; max2 VS ind2_min and ind2_max.
  #
  # Direction:            I                J               K
  # ogrid_i loop      (1,max1_i,1)    (max1_i,1,1)    (max1_i,1,1)
  # blkGetIJK indice  (max1_i,1,1)    (max1_i,1,1)    (max1_i,1,1)
  # ogrid_j loop      (1,1,max2_i)    (1,1,max2_i)    (1,max2_i,1)
  # blkGetIJK indice  (1,max2_i,1)    (1,max2_i,1)    (1,max2_i,1)
  # 

  # Calculate ind1_min & ind1_max
  array set iArr [getLengthSpacingArr $blk "I" $dir $max1]
  set corner1_Pts [getCornerPointsForIndex $iArr(length) $iArr(spacing) $max1 0]

  set ogrid_i [lindex $corner1_Pts 0]
  if {$ogrid_i == 1} {
    set ogrid_i 2
  }

  set ind1_min $ogrid_i
  set ind1_max [lindex $corner1_Pts end]
  if {$ind1_max == $max1} {
    set ind1_max [expr $max1-1]
  }

  # Calculate ind2_min & ind2_max
  array set jArr [getLengthSpacingArr $blk "J" $dir $max2]
  set corner2_Pts [getCornerPointsForIndex $jArr(length) $jArr(spacing) $max2 1]

  set ogrid_j [lindex $corner2_Pts 0]
  if {$ogrid_j == 1} {
    set ogrid_j 2
  }

  set ind2_min $ogrid_j
  set ind2_max [lindex $corner2_Pts end]
  if {$ind2_max == $max2} {
    set ind2_max [expr $max2-1]
  }

  # Obtain the dimension index for the third direction when ALL is applied.
  if {$dir == "ALL"} {

    # Calculate ind3_min & ind3_max
    array set kArr [getLengthSpacingArr $blk "K" $dir $ind3_max]
    set corner3_Pts [getCornerPointsForIndex $kArr(length) $kArr(spacing) \
        $ind3_max 2]

    set ogrid_k [lindex $corner3_Pts 0]
    if {$ogrid_k == 1} {
      set ogrid_k 2
    }

    set ind3_min $ogrid_k
    set ind3_max [lindex $corner3_Pts end]
  }

  # Please note this list should show the corresponding block. Otherwise, the 
  # H locator will be the same even when multiple propagating cases are 
  # selected. 
  if {$Propagate == 1 && $dir != "ALL"} {
    set locatorH_prop [list $ind1_min $ind1_max $ind2_min $ind2_max]
    set locatorH_prop [concat $locatorH_prop $propBlks]
  }
  set HLocation [list $ind1_min $ind1_max $ind2_min \
                      $ind2_max $ind3_min $ind3_max]
  return $HLocation
}

###############################################################################
# createConnectorFromIndices:
# - Creates connectors from segments
# - This is a helper function function for blkMakeBflyConnectors
# - indList => values for ind1 ind2 ind3
# - index => iterates through corresponding index 
#       (0 = ind1, 1 = ind2, 2 = ind3)
# - loopMinMax => values for loopMin & loopMax
###############################################################################
proc createConnectorFromIndices { indList index loopMinMax dir blk } {
  set loopMin [lindex $loopMinMax 0]
  set loopMax [lindex $loopMinMax 1]

  set segment [pw::SegmentSpline create]
  for {set i $loopMin} {$i <= $loopMax} {incr i} {
    lset indList [expr $index - 1] $i
    $segment addPoint [getBlkXYZFromIndices $blk $dir $indList]
  }

  set connector [pw::Connector create]

  # Add segment to Connector
  # if there is an error, return another connector by endpoints
  if {[catch {$connector addSegment $segment}]} {
    $connector delete
    set ind1 [lindex $indList 0]
    set ind2 [lindex $indList 1]
    set ind3 [lindex $indList 2]
    set connector [getConnectorByEndPoints \
        [$blk getXYZ [blkGetIJK $dir $ind1 $ind2 $ind3]] $pt]
  }

  return $connector
}

###############################################################################
# getBlkXYZFromIndicies:
# - returns a point on a block given 3 indicies (I,J,K)
###############################################################################
proc getBlkXYZFromIndices { blk dir indList } {
  set ind1 [lindex $indList 0]
  set ind2 [lindex $indList 1]
  set ind3 [lindex $indList 2]

  if {[catch {$blk getXYZ [blkGetIJK $dir $ind1 $ind2 $ind3]} pt]} {
    set msg "Failed to get point while processing block: [$blk getName]."
    return -code error $msg
  }

  return $pt
}

###############################################################################
# blkMakeBflyConnectors:
# - Accepts a block, direction, and a named reference to a connector array
# - Creates butterfly block connectors on the given block
###############################################################################
proc blkMakeBflyConnectors { blk dir conVar {temp 0}} {
  global oFraction oDimension Propagate oScaleFac scriptDir

  # Skip the H dimension index calculation if propagate is applied to adjacent 
  # blocks. #global ind1_min ind1_max ind2_min ind2_max
  upvar $conVar con

  # If block is not Structured, return 0
  if {![$blk isOfType pw::BlockStructured]} {
     return 0
  }

  set blkInfo [$blk getDimensions]

  set id [lindex $blkInfo 0]
  set jd [lindex $blkInfo 1]
  set kd [lindex $blkInfo 2]

  set ind3_min 1
  if {$dir == "I"} {
    set max1 $jd
    set max2 $kd
    set ind3_max $id
  } elseif {$dir == "J"} {
    set max1 $id
    set max2 $kd
    set ind3_max $jd
  } elseif {$dir == "K"} {
    set max1 $id
    set max2 $jd
    set ind3_max $kd
  } else {
    set max1 $id
    set max2 $jd
    set ind3_max $kd
  }

  # Move the H domain locator out of this procedure so that it can be skipped 
  # if Propagate is applied.
  set ind3_beg $ind3_min
  set ind3_end $ind3_max

  set HLocationData [HDomLocator $blk $dir $max1 $max2 $ind3_min $ind3_max]

  set ind1_min [lindex $HLocationData 0]
  set ind1_max [lindex $HLocationData 1]
  set ind2_min [lindex $HLocationData 2]
  set ind2_max [lindex $HLocationData 3]
  if {$dir == "ALL"} {
      set ind3_min [lindex $HLocationData 4]
      set ind3_max [lindex $HLocationData 5]
  }  
 
  ## Creates all indice connectors ##
  #-- Connectors for center Bfly block
  set indArr(1,0) $ind1_min
  set indArr(1,1) $ind1_max
  set indArr(2,0) $ind2_min
  set indArr(2,1) $ind2_max
  set indArr(3,0) $ind3_min
  set indArr(3,1) $ind3_max

  set indArr(1,0,name) "ind1_min"
  set indArr(1,1,name) "ind1_max"
  set indArr(2,0,name) "ind2_min"
  set indArr(2,1,name) "ind2_max"
  set indArr(3,0,name) "ind3_min"
  set indArr(3,1,name) "ind3_max"
     
  for {set i 1} {$i <= 3} {incr i} {
    for {set j 0} {$j < 2} {incr j} {
      for {set k 0} {$k < 2} {incr k} {
        set index $i
        set loopMinMax [list $indArr($i,0) $indArr($i,1)]
        set indList {}
        switch $i {
          1 { 
            lappend indList $indArr(1,0) $indArr(2,$j) $indArr(3,$k) 
            set name1 $indArr(2,$j,name)
            set name2 $indArr(3,$k,name)
          }
          2 { 
            lappend indList $indArr(1,$j) $indArr(2,0) $indArr(3,$k) 
            set name1 $indArr(1,$j,name)
            set name2 $indArr(3,$k,name)
          }
          3 { 
            lappend indList $indArr(1,$j) $indArr(2,$k) $indArr(3,0) 
            set name1 $indArr(2,$k,name)
            set name2 $indArr(1,$j,name)
          }
        }
        set con($name1,$name2) [createConnectorFromIndices $indList $index \
          $loopMinMax $dir $blk]
      }
    }
  }

  #-- Create connectors from corner to center Bfly Block
  set indArr(1,0,pt1) 1
  set indArr(1,1,pt1) $max1
  set indArr(2,0,pt1) 1
  set indArr(2,1,pt1) $max2
  set indArr(3,0,pt1) $ind3_beg
  set indArr(3,1,pt1) $ind3_end

  set indArr(1,0,pt2) $ind1_min
  set indArr(1,1,pt2) $ind1_max
  set indArr(2,0,pt2) $ind2_min
  set indArr(2,1,pt2) $ind2_max
  set indArr(3,0,pt2) $ind3_min
  set indArr(3,1,pt2) $ind3_max

  set conName1(0,0) "corner1"
  set conName1(1,0) "corner2"
  set conName1(0,1) "corner4"
  set conName1(1,1) "corner3"
  set conName2(0)   "ind3_min"
  set conName2(1)   "ind3_max"

  set isPtError false
  for {set i 0} {$i < 2} {incr i} {
    for {set j 0} {$j < 2} {incr j} {
      for {set k 0} {$k < 2} {incr k} {
        set name1 $conName1($j,$i)
        set name2 $conName2($k)

        set ind3 $indArr(3,$k,pt1)
        set ind1 $indArr(1,$j,pt1)
        set ind2 $indArr(2,$i,pt1)
        set indList [list $ind1 $ind2 $ind3]

        if {[catch {set pt1 [getBlkXYZFromIndices $blk $dir $indList]}]} {
          set isPtError true
        }

        set ind3 $indArr(3,$k,pt2)
        set ind1 $indArr(1,$j,pt2)
        set ind2 $indArr(2,$i,pt2)
        set indList [list $ind1 $ind2 $ind3]

        if {[catch {set pt2 [getBlkXYZFromIndices $blk $dir $indList]}]} {
          set isPtError true
        }

        if {!$isPtError} {
          set segment [pw::SegmentSpline create]
          $segment addPoint $pt1
          $segment addPoint $pt2
          set con($name1,$name2) [pw::Connector create]
          $con($name1,$name2) addSegment $segment

        } else {
          #If there is a problem, create connector from Segment
          set con($name1,$name2) [pw::Connector create]
          set con($name1,$name2) [getConnectorByEndPoints $pt1 $pt2]
        }

        set isPtError false
      }
    }
  }

  return 1
}

###############################################################################
# getConnectorByEndPoints:
# - Looks at ALL connectors on grid and returns that connector which has the 
# - end points of pt1 and pt2
###############################################################################
proc getConnectorByEndPoints { pt1 pt2 } {
  set tol [pw::Grid getNodeTolerance]

  foreach con [pw::Grid getAll -type pw::Connector] { 
    set pta [$con getXYZ -arc 0]
    set ptb [$con getXYZ -arc 1]

    set dist_1a [getDist $pt1 $pta]
    set dist_1b [getDist $pt1 $ptb]
    set dist_2a [getDist $pt2 $pta]
    set dist_2b [getDist $pt2 $ptb]

    if {($dist_1a < $tol && $dist_2b < $tol) || \
         ($dist_1b < $tol && $dist_2a < $tol)} {
     return $con
    }
  }
  
  set msg "Failed to create Connector: No connector found."
  return -code error $msg
}

###############################################################################
# getDomainByCorners:
# - Looks at ALL domains on grid and returns that domain which has the end
# - corners which correspond to arguments pt1 pt2 pt3 pt4
###############################################################################
proc getDomainByCorners { pt1 pt2 pt3 pt4 } {
  set allDoms [pw::Grid getAll -type pw::Domain]
  foreach dom $allDoms {
    set domPtList {}

    set dimensions [$dom getDimensions]
    lappend domPtList [$dom getXYZ [list 1 1]]
    lappend domPtList [$dom getXYZ [list [lindex $dimensions 0] 1]]
    lappend domPtList [$dom getXYZ [list 1 [lindex $dimensions 1]]]
    lappend domPtList [$dom getXYZ $dimensions]

    set matchCount 0
    foreach pt $domPtList {
      if {[pwu::Vector3 equal $pt $pt1] ||
           [pwu::Vector3 equal $pt $pt2] ||
           [pwu::Vector3 equal $pt $pt3] ||
           [pwu::Vector3 equal $pt $pt4]} {
        incr matchCount
      }
    }

    if {$matchCount == 4} {
      return $dom
    }
  }

  set msg "Failed to create Domain: Domain not found."
  return -code error $msg
}

###############################################################################
# buildGridInfoList: 
# - Saves grid information for use in Gui and Transformation
# - If refreshSelectBlocks is true, then blocks in GUI listbox are refreshed
#   and selection emptied. Should be set after run or ok since blocks change
###############################################################################
proc buildGridInfoList { refreshSelectBlocks } {
  global Structured_Blocks ListBoxBlocks ListBoxGroups Transformed_Blocks \
    defListBoxStringWidth Original_EntRenderAtt

  set prevBlks {}
  if {$refreshSelectBlocks} {
    set prevBlks $Structured_Blocks
  }

  set Structured_Blocks [pw::Grid getAll -type pw::BlockStructured]

  # We must manually remove unbalanced/invalid blocks
  set invalidBlks {}
  for {set i 0} {$i < [llength $Structured_Blocks]} {incr i} {
    set blk [lindex $Structured_Blocks $i]
    if {![$blk isValid]} {
      set Structured_Blocks [lreplace $Structured_Blocks $i $i]
      lappend invalidBlks $blk
    }
  }

  # Remove blocks that have already been transformed from Structured_Blocks
  foreach blk $Transformed_Blocks {
    set index [lsearch $Structured_Blocks $blk]
    if {$index >= 0} {
      set Structured_Blocks [lreplace $Structured_Blocks $index $index]
    }
  }

  # Add potential block groups
  set strBlkGroups [getStrBlkGroups [pw::Group getAll] $Structured_Blocks \
    [concat $Transformed_Blocks $invalidBlks]]

  if {$refreshSelectBlocks} {
    set ListBoxBlocks {}
    set ListBoxGroups {}
    foreach blk $Structured_Blocks {
      lappend ListBoxBlocks $blk
    }
    foreach group $strBlkGroups {
      lappend ListBoxGroups $group
    }
  }
}

###############################################################################
# getStrBlkGroups:
# - Given a list of groups, returns only groups containing structured blocks
# - Of the structured blocks in a group, at least one must be in masterBlkList
# - Of the remaining structured blocks, they must exist in secondaryBlkList
###############################################################################
proc getStrBlkGroups { groupList masterBlkList secondaryBlkList } {
  set strGroups {}
  foreach group $groupList {
    if {[string compare [$group getEntityType] pw::Block] == 0} {
      set groupHasStrBlocks false
      set entList [$group getEntityList]
      foreach ent $entList {
        if {[$ent isOfType pw::BlockStructured]} {
          if {[lsearch $masterBlkList $ent] >= 0} {
            set groupHasStrBlocks true
          } else {
            if {[lsearch $secondaryBlkList $ent] == -1} {
              set groupHasStrBlocks false
              break
            }
          }
        }
      }
      if {$groupHasStrBlocks} {
        set strGroups [lappend strGroups $group]
      }
    }
  }
  return $strGroups
}

###############################################################################
# buildConRenderList: Builds a special con-to-render attribute list for all
#   connectors. List format is like this,
#   {con1Name con1Color con1LineWidth con1ColorMode} {con2Name con2Color ...}
# - This is built so that original connector attributes may be restored once
#   the script finishes.
###############################################################################
proc buildConRenderList { cons } {
  set conRenderList {}
  foreach con $cons {
    set conRenderList [lappend conRenderList [list $con [$con getColor] \
      [$con getRenderAttribute LineWidth] [$con getRenderAttribute ColorMode]]]
  }
  return $conRenderList
}

###############################################################################
# restoreConRenderAtts: Restores connectors to their original render attributes
###############################################################################
proc restoreConRenderAtts {} {
  global Original_EntRenderAtt defWidth defColor
  # newCons is trimmed down from a list of all connectors to a list of only the
  # newly created ones.
  set newCons [pw::Grid getAll -type pw::Connector]
  foreach ent $Original_EntRenderAtt {
    set entName [lindex $ent 0]
    set conIndex [lsearch -exact $newCons $entName]
    if {$conIndex >= 0} {
      set newCons [lreplace $newCons $conIndex $conIndex]
    }
    # Set Color and Line Length
    $entName setColor [lindex $ent 1]
    $entName setRenderAttribute LineWidth [lindex $ent 2]
    $entName setRenderAttribute ColorMode [lindex $ent 3]
  }

  # Newly created connectors will not be in Original_EntRenderAtt, so they
  # are set to default render attributes.
  foreach con $newCons {
    $con setColor $defColor
    $con setRenderAttribute LineWidth $defWidth 
    $con setRenderAttribute ColorMode Entity
  }

  pw::Display update
}

###############################################################################
# updateGuiState:
# - Enables/Disables GUI Widgets based on user interaction
# - If clearSelection, then the users listbox selection is cleared
###############################################################################
proc updateGuiState {{clearSelection false}} {
  global Job_Running Last_Job_Type Propagate Direction ListBoxSelectableItems

  if {!$Job_Running} {
    if [string equal $Last_Job_Type "Transform"] {
      # Update GUI selectable blocks
      set ListBoxSelectableItems [getSelectionList true true]

      # If a job has been applied
      setGuiState disabled
      .right.top.list configure -state normal
      .bottom.buttons.ok configure -state normal
      .bottom.buttons.cancel configure -state normal
      .right.select configure -state normal
      if $clearSelection {
        .right.top.list selection clear 0 end
      }
    } 

    if {[llength [.right.top.list curselection]] > 0} {
      # If blocks are selected
      setGuiState normal
    } else {
      setApplicationGuiState disabled 
      if {[string compare $Last_Job_Type "Transform"] == 0} {
        .bottom.buttons.ok configure -state normal
      }
    }
    highlightSelectedBlocks
  } else {
    # If Job is running
    setGuiState disabled
  }
}

###############################################################################
# shortenItem: shorten string length to fit in listbox. repl appended
###############################################################################
proc shortenItem { item repl } {
  global defListBoxStringWidth
  set listBoxCharWidth [expr $defListBoxStringWidth - 1]
  set replIndex [expr $listBoxCharWidth - [string length $repl]]
  if {[string length $item] > $listBoxCharWidth && $replIndex > \
      [string length $repl]} {
    set item [string range $item 0 $replIndex]
    set item [append item $repl]
  }
  return $item
}

###############################################################################
# highlightSelectedBlocks: highlight selected blocks by changing render atts
###############################################################################
proc highlightSelectedBlocks { } {
  global Structured_Blocks Prev_Highlight_Selection defSelectColor \
    defSelectWidth defSelectColorMode defColor defWidth defColorMode

  # First, gather up the current selection of blocks
  set selectedBlks [getCurSelBlks]

  set newBlks [ldifference $selectedBlks $Prev_Highlight_Selection]
  set oldBlks [ldifference $Prev_Highlight_Selection $selectedBlks]
  if {[llength $newBlks] > 0 || [llength $oldBlks] > 0} {
    # Reset old blocks to default Bfly script colors
    if {[llength $oldBlks] > 0} {
      highlightBlockCons $oldBlks $defColorMode $defColor $defWidth
    }

    # Set All selected blocks to default Bfly script "selected" colors
    highlightBlockCons $selectedBlks $defSelectColorMode $defSelectColor \
      $defSelectWidth

    # If changes were made, update the display and prev. selected blocks
    pw::Display update
    set Prev_Highlight_Selection $selectedBlks
  }
}

###############################################################################
# highlightBlockCons: Using a block list, highlight all block connectors
###############################################################################
proc highlightBlockCons { blkList colorMode color width } {
  # Drill down and find all connectors that correspond to the blk list and
  # highlight them.
  set conList {}
  set doms {}
  foreach blk $blkList {
    set faceCount [$blk getFaceCount]
    for {set i 1} {$i <= $faceCount} {incr i} {
      set face [$blk getFace $i]
      set edgeCount [$face getEdgeCount]
      for {set j 1} {$j <= $edgeCount} {incr j} {
        set edge [$face getEdge $j]
        set conCount [$edge getConnectorCount]
        for {set k 1} {$k <= $conCount} {incr k} {
          set con [$edge getConnector $k]
          $con setRenderAttribute ColorMode Entity
          $con setColor $color
          $con setRenderAttribute LineWidth $width
        }
      }
    }
  }
}

###############################################################################
# ldifference: Helper function (list set difference)
###############################################################################
proc ldifference {a b} {
  set result {}
  foreach e $a {
    if {$e ni $b} {lappend result $e}
  }
  return $result
}

###############################################################################
# createBflyDBSurf:
# - Takes a block and direction and exports the domains on the block that
#   correspond the the direction.
# - It is then re-imported to produce a database for projection
# - Database is saved in global butterflyDBArr
###############################################################################
proc createBflyDBSurf { blk dir } {
  global butterflyDBFile scriptDir

  set butterflyDBFile [file join $scriptDir "butterflyDB.grd"]
  set butterflyFaces {}

  set boundaryList [list IMinimum IMaximum JMinimum JMaximum KMinimum KMaximum]
  set faceList {}

  foreach boundary $boundaryList {
    lappend faceList [$blk getFace $boundary]
  }
    
  set domFacImin [lindex $faceList 0]
  set domFacImax [lindex $faceList 1]
  set domFacJmin [lindex $faceList 2]
  set domFacJmax [lindex $faceList 3]
  set domFacKmin [lindex $faceList 4]
  set domFacKmax [lindex $faceList 5]
  set domNumImin [llength $domFacImin]
  set domNumImax [llength $domFacImax]
  set domNumJmin [llength $domFacJmin]
  set domNumJmax [llength $domFacJmax]
  set domNumKmin [llength $domFacKmin]
  set domNumKmax [llength $domFacKmax]

  if {$dir == "I"} {
    foreach face $domFacImin {
      lappend butterflyFaces $face
    }
    foreach face $domFacImax {
      lappend butterflyFaces $face
    }
  } elseif {$dir == "J"} {
    foreach face $domFacJmin {
      lappend butterflyFaces $face
    }
    foreach face $domFacJmax {
      lappend butterflyFaces $face
    }
  } elseif {$dir == "K"} {
    foreach face $domFacKmin {
      lappend butterflyFaces $face
    }
    foreach face $domFacKmax {
      lappend butterflyFaces $face
    }
  }
  set butterflyDoms [getFaceDomains $butterflyFaces]

  pw::Grid export -type PLOT3D -format ASCII -precision Double \
    $butterflyDoms $butterflyDBFile 

  set dbList [pw::Database import -type PLOT3D -format ASCII -precision Double \
    $butterflyDBFile]

  # Tmp Database file no longer needed. Delete it.
  if { [file exists $butterflyDBFile] } {
    file delete -force $butterflyDBFile
  }

  # Must join faces to avoid bad projections. For example, the ogrid might be
  # built on a split domain.
  set dbList [pw::Surface join -reject remain $dbList]
  return [concat $dbList $remain]
}

###############################################################################
# deleteBflyDBSurf:
# - deletes databases stored in butterflyDBArr
# - Cleans up database last used by createBflyDBSurf 
###############################################################################
proc deleteBflyDBSurf { } {
  global butterflyDBArr Transformed_Blocks prevDirection

  if {$prevDirection != "ALL" && [info exists butterflyDBArr]} {
    foreach blk $Transformed_Blocks {
      if {[llength $butterflyDBArr($blk)] > 0} {
        foreach db $butterflyDBArr($blk) {
          $db delete -force
        }
      }
      unset butterflyDBArr($blk)
    }
  }
}

###############################################################################
# switchMode:
# - This that happen when the direction (I,J,K,ALL) is changed
# - Called from Radiobutton presses
###############################################################################
proc switchMode { {dir 0} } {
  global ListBoxBlocks Propagate Direction

  if {$dir} {
   set currentSelection [.right.top.list curselection]
  } 
  .right.top.list selection clear 0 end
  foreach csel $currentSelection {
   .right.top.list selection set $csel
   .right.top.list selection set $csel
  }
  .left.dir.i configure -state normal        
  .left.dir.j configure -state normal
  .left.dir.k configure -state normal
  .left.dir.all configure -state normal
  if {$Direction == "ALL"} {
   set Propagate 0
   .left.values.propagate configure -state disabled
  } else {
   .left.values.propagate configure -state normal
  }
  # Disable Propagate if no direction is specified.
  if {$Direction == ""} {
      .left.values.propagate configure -state disabled
  }
}

###############################################################################
# setGuiState: 
# - Enable/disables GUI
# - It is used to indicate that a transformation is currently running
###############################################################################
proc setGuiState { isEnabled } {
  # Set listbox 
  .right.top.list configure -state $isEnabled

  .right.select configure -state $isEnabled

  # set the state of the GUI application options
  setApplicationGuiState $isEnabled

  # set CANCEL button states
  .bottom.buttons.cancel configure -state $isEnabled

  # Update GUI so state change is visible
  update
}

###############################################################################
# setApplicationGuiState: 
# - Enable/disables GUI related to options (Apply, Ok, Directions)
# - It is used to indicate that a transformation is currently running
###############################################################################
proc setApplicationGuiState { isEnabled } {
  global Direction

  # Set radio button state
  .left.dir.i configure -state $isEnabled 
  .left.dir.j configure -state $isEnabled 
  .left.dir.k configure -state $isEnabled 
  .left.dir.all configure -state $isEnabled

  # Set option panel state
  .left.values.dist.entdist configure -state $isEnabled
  .left.values.pts.entpts configure -state $isEnabled
 
  if {$Direction != "ALL"} {
    .left.values.propagate configure -state $isEnabled
  }

  # Set preview button state
  .left.values.preview configure -state $isEnabled
  
  # set OK and RUN button states
  .bottom.buttons.ok configure -state $isEnabled
  .bottom.buttons.run configure -state $isEnabled
}

###############################################################################
# getPropagatedBlockList: 
# - For all blocks in BlkList, gets adjacent blocks that are in the propagated 
#   direction.
# - Returns updated block list (checks for duplicates)
###############################################################################
proc getPropagatedBlockList { blkList dir } {

  for {set i 0} {$i < [llength $blkList]} {incr i} {
    set tmpBlk [lindex [lindex $blkList $i] 0]

    set adjBlockList [getAdjacentPropagatedBlocks $tmpBlk $dir]
    
    foreach adjblk $adjBlockList {
      # if not a duplicate, then add to blkList
      if {[lsearch $blkList $adjblk] == -1} {
        lappend blkList $adjblk
      }
    }
  }

  return $blkList
}

###############################################################################
# getAdjacentPropagatedBlocks: 
# - For a given domain and blocklist, returns all blocks that reference that
#   domain
###############################################################################
proc getAdjacentPropagatedBlocks { blk dir } {
  global Structured_Blocks
  set adjacentBlocks [list $blk]
  set faceDomList [list \
    [list [getFaceDomains [$blk getFace ${dir}Minimum]]] \
    [list [getFaceDomains [$blk getFace ${dir}Maximum]]]]

  foreach domList $faceDomList {
    # If domlist contains only one domain
    if {[llength $domList] == 1} {
      set refBlocks [getDomsReferencedBlocks $domList $Structured_Blocks]
      foreach tblk $refBlocks {
        if {[lsearch -exact $adjacentBlocks $tblk] == -1} {
          lappend adjacentBlocks "$tblk"
        }
      }
    }
  }
  return $adjacentBlocks
}

###############################################################################
# getDomsReferencedBlocks: 
# - For a given domain and blocklist, returns all blocks that reference that
#   domain
###############################################################################
proc getDomsReferencedBlocks { domain blkList } {
  set refedBlocks { }

  foreach blk $blkList {
    set domList [getRefedDomains $blk]
    foreach dom $domList {
      if {[string compare $domain $dom] == 0} {
        lappend refedBlocks $blk
        continue
     }
    }
  }
  return $refedBlocks
}

###############################################################################
# getFaceDomains: returns all domains associated with a list of faces
###############################################################################
proc getFaceDomains { faceList } {
  set resultList {}

  foreach subFace $faceList {
    set domList [$subFace getDomains]
    foreach dm $domList {

      #Append to result list
      lappend resultList $dm
    }
  }
  return $resultList
}

###############################################################################
# getRefedDomains: returns a list of domains belonging to the blocks in blkList
###############################################################################
proc getRefedDomains { blkList } {
  foreach blk $blkList {
    set faceCount [$blk getFaceCount]
    for {set i 1} {$i <= $faceCount} {incr i} {
      set tmpFace [$blk getFace $i]
      set domainCount [$tmpFace getDomainCount]
      for {set j 1} {$j <= $domainCount} {incr j} {
        set tmpDomain [$tmpFace getDomain $j] 
        lappend domainList $tmpDomain
      }
    }
  }
  return $domainList
}

###############################################################################
# buildBlockList: creates list of blocks for Bfly topology creation
###############################################################################
proc buildBlockList { dir propagate } {
  global Structured_Blocks
  set bflyBlockList { }

  # If Direction is set to "ALL"
  if {$dir == "ALL"} { 
    if {[llength [.right.top.list curselection]] > 0} {
      set bflyBlockList [getCurSelBlks]
    }

  # If Direction is set to "I, J, or, K" and at least 1 block is selected
  } elseif {[llength [.right.top.list curselection]] > 0} {
    set bflyBlockList [getCurSelBlks]
    
    # If propagated, get propagated block list
    if {$propagate == 1} {
      set bflyBlockList [getPropagatedBlockList $bflyBlockList $dir]
    }
  }
  return $bflyBlockList
}

###############################################################################
# buildAlignList:
# - Builds a Master-Block align list so that topologically connects blocks may
#   be oriented consistently
###############################################################################
proc buildAlignList { } {
  global MasterAlignBlocks

  set selBlocks [getSelectionList false false]
  for {set i 0} {[llength $selBlocks] > 0} {incr i} {
    set topBlock [lindex $selBlocks 0]
    set adjBlocks($i) [concat $topBlock [pw::Block getAdjacentBlocks -all \
      $topBlock]]
    set selBlocks [ldifference $selBlocks $adjBlocks($i)]
  }
  set MasterAlignBlocks {}
  for {set j 0} {$j < $i} {incr j} {
    lappend MasterAlignBlocks [lindex $adjBlocks($j) 0]
  }
}

###############################################################################
# orientBlocks:
# - orients blocks based on MasterAlignBlocks created with buildAlignList
###############################################################################
proc orientBlocks {} {
  global MasterAlignBlocks

  set selBlocks [getSelectionList false false]
  foreach blk $MasterAlignBlocks {
    $blk alignOrientation $selBlocks
  }
}

###############################################################################
# deleteLastBlock:
# - Delete pre-transformed blocks if they exist
# - Align new blocks according to master before old master may get deleted.
# - Set a new/valid MasterBlock after deleting old blocks
###############################################################################
proc deleteLastBlock { } {
  global Transformed_Blocks MasterBlock

  # Must orient the blocks now before they are deleted. This keeps orientation
  # consistent.
  orientBlocks
  foreach blk $Transformed_Blocks {
    $blk delete
  }
  # Rebuild align list with new master align blocks
  buildAlignList
}

###############################################################################
# runJob:
# - performs transformation to butterfly topology
# - If drawPreview == true, only connectors drawn for user preview
###############################################################################
proc runJob { dir propagate drawPreview } {
  global oScaleFac oDimension guiTitle Job_Running Transformed_Blocks
  global Last_Job_Type butterflyDBArr butterflyDBSurf prevDirection
  global Error_Last_Job locatorH_prop

  set Error_Last_Job false
  set Job_Running true
  updateGuiState
  
  # Warning if oScaleFac component is larger than 1.0.
  set oScale_1 [lindex $oScaleFac 0]
  set oScale_2 [lindex $oScaleFac 1]
  set oScale_3 [lindex $oScaleFac 2]

  if {$oScale_1 > 1.0 || $oScale_2 > 1.0 || $oScale_3 > 1.0 || \
      $oScale_1 < 0.0 || $oScale_2 < 0.0 || $oScale_3 < 0.0} {
    set Job_Running false 
    updateGuiState
    set msg "Invalid scaling factor input!"
    return -code error $msg
  } 

  if {$oDimension < 2 || ![string is digit $oDimension]} {
    set Job_Running false 
    updateGuiState
    set msg "Invalid Ogrid rib grid points!"
    return -code error $msg
  }

  # Clean any previous intermediate objects
  clean false

  # Create Databases for projections 
  set blkList [buildBlockList $dir $propagate]
  if {!$drawPreview} {
    if {$dir != "ALL"} {
      foreach blk $blkList {
        set butterflyDBArr($blk) [createBflyDBSurf $blk $dir]
      }
    }
  }

  # Run Job on all blocks that have been selected
  setCreatorState create
  set count 0
  foreach blk $blkList {
      incr count
    if {$drawPreview} {
      # Draw preview
      set Last_Job_Type "Preview"

      # Draw Connector Preview
      blkMakeBflyConnectors $blk $dir cons

    } else {
      # Apply Transformation
      set Last_Job_Type "Transform"

      # Draw butterfly topology
      blkMakeBflyBlocks $blk $dir
    }
  }

  # Re-build GUI grid info lists based on previous job
  if {$drawPreview} {
    buildGridInfoList false
  } else {
    set Transformed_Blocks $blkList
    buildGridInfoList true
  }
  set prevDirection $dir

  # Reset locatorH_prop to an empty list
  set locatorH_prop { }

  # Preview created, re-enable GUI and reset title
  wm title . $guiTitle

  set Job_Running false
  updateGuiState true

  # Update Pointwise display
  pw::Display update
}

###############################################################################
# selectBtnPressed: called when interactive block selection button is pressed
###############################################################################
proc selectBtnPressed { } {
  global Propagate Direction Structured_Blocks 

  if {[llength $Structured_Blocks] <= 0} {
    set title "Error: No Blocks Available"
    set msg "There are no structured blocks in the current grid"
    tk_messageBox -title $title -message $msg -type ok -icon error
    return
  }
 
  wm withdraw .

  set desc "Please select the blocks you wish to convert to a butterfly topology."
  set mask [pw::Display createSelectionMask -requireBlock Structured]
  pw::Display selectEntities -description $desc -selectionmask \
    $mask -pool $Structured_Blocks tSel 

  .right.top.list selection clear 0 end
  foreach i $tSel(Blocks) {
    .right.top.list selection set [lsearch $Structured_Blocks $i] \
      [lsearch $Structured_Blocks $i]
  }

  updateGuiState

  if {[winfo exists .]} {
    wm deiconify .
  }
}

###############################################################################
# makeInputField: create a Tk text widget
###############################################################################
proc makeInputField { parent name title variable {width 10} {when ""} \
                      {valid ""}} {
  frame $parent.$name
  label $parent.$name.lbl$name -text $title
  entry $parent.$name.ent$name -textvariable $variable -width $width
  if {[string compare $valid ""]!=0} {
    $parent.$name.ent$name configure -validate $when
    set var " 
      if {$valid==1} { if {[string compare %V focusout]==0} \
      { focus %W }; return 0 } else { %W configure -background #FFFFFF; \
       return 1 }"]

    $parent.$name.ent$name configure -validatecommand $var
    $parent.$name.ent$name configure -invalidcommand { 
      %W configure -background "#FFCCCC" }
  }
  pack "$parent.$name.lbl$name" -side left -padx 3 -pady 1
  pack "$parent.$name.ent$name" -side right -padx 3 -pady 1
  return $parent.$name
}

###############################################################################
# addUniqueToList: unique entities from addEntList are added to entList
###############################################################################
proc addUniqueToList { entList addEntList } {
  foreach ent $addEntList {
    if {[lsearch $entList $ent] == -1} {
      lappend entList $ent
    }
  }
  return $entList
}

###############################################################################
# getCurSelBlks:
#   - Determine user selected blocks from Gui listbox.
#   - Valid blocks from groups are added.
###############################################################################
proc getCurSelBlks {} {
  set curSelIndexList [.right.top.list curselection]
  set selectionList [getSelectionList false true]
  set curSelBlks {}
  foreach index $curSelIndexList {
    set curEnt [lindex $selectionList $index]
    if {[$curEnt isOfType pw::BlockStructured]} {
      lappend curSelBlks $curEnt
    } elseif {[$curEnt isOfType pw::Group]} {
      set groupEntList [$curEnt getEntityList]
      set strBlkEnts {}
      foreach ent $groupEntList {
        if {[$ent isOfType pw::BlockStructured] && [$ent isValid]} {
          lappend strBlkEnts $ent
        }
      }
      set curSelBlks [addUniqueToList $curSelBlks $strBlkEnts]
    }
  }
  return $curSelBlks
}

###############################################################################
# getSelectionList:
#   - Create a list of all blocks "available" to be picked.
#   - If makePrintable, then entity names are used and "shortened" if needed.
###############################################################################
proc getSelectionList { makePrintable returnGroups } {
  global ListBoxBlocks ListBoxGroups guiGroupPrefix
  set printableList {}
  foreach blk $ListBoxBlocks {
    if {$makePrintable} {
      lappend printableList [shortenItem [$blk getName] "..."]
    } else {
      lappend printableList $blk
    }
  }
  if {$returnGroups} {
    foreach group $ListBoxGroups {
      if {$makePrintable} {
        set groupName "$guiGroupPrefix[$group getName]"
        lappend printableList [shortenItem $groupName "..."]
      } else {
        lappend printableList $group
      }
    }
  }
  return $printableList
}

#################################################################################
# preselectBlocks: 
# -Checks if any structured blocks have been selected before script is run
# -Selects these blocks in the listbox
#################################################################################
proc preselectBlocks { } {
  global ListBoxSelectableItems

  # Update GUI selectable blocks
  set ListBoxSelectableItems [getSelectionList true true]

  pw::Display getSelectedEntities ents
        
  foreach blk $ents(Blocks) {
    if [$blk isOfType pw::BlockStructured] {
      # Name of the block as it appears in the List Tab
      set blkName [$blk getName]
      # Finds the index of selected block in the listbox 
      set blkStrucIndex [lsearch $ListBoxSelectableItems $blkName]
      # Selects the block in the listbox 
      .right.top.list select set $blkStrucIndex
    }
  }
}

###############################################################################
# setCreatorState: 
# - If possible, sets creator state according to input parameter "state" 
# - State = create, end, or abort
# - global variable isCreatorSet used to ensure that multiple modes aren't set,
#   and that non-existent modes are not ended or aborted
###############################################################################
proc setCreatorState { state } {
  global creation_mode isCreatorSet

  if {[string compare $state "create"] == 0} {
    if {!$isCreatorSet} {
      set creation_mode [pw::Application begin Create -monitor]
      set isCreatorSet true
    }
  } elseif {[string compare $state "end"] == 0} {
    if {$isCreatorSet} {
      $creation_mode end
      set isCreatorSet false
    }
  } elseif {[string compare $state "abort"] == 0} {
    if {$isCreatorSet} {
      $creation_mode abort
      set isCreatorSet false
    }
  }
}

###############################################################################
# changeDirection: 
# - Change direction of BflyBlock creation 
# - Called by the GUI RadioButtons in makeWindow
###############################################################################
proc changeDirection { } {
  global Direction dupDirection
  if {[string compare $Direction $dupDirection] != 0} { 
    switchMode 1 
  }
  set dupDirection $Direction 
}

###############################################################################
# handleError: Called when excetption is caught during runJob
###############################################################################
proc handleError { msg } {
  global locatorH_prop guiTitle Job_Running Last_Job_Type Error_Last_Job
  set Error_Last_Job true

  tk_messageBox -message $msg -type ok -icon error -title "Error"
  setCreatorState abort

  # Clear selection in list box
  .right.top.list configure -state normal
  .right.top.list selection clear 0 end

  buildGridInfoList false

  # Reset locatorH_prop to an empty list
  set locatorH_prop { }

  # Preview created, re-enable GUI and reset title
  wm title . $guiTitle

  set Job_Running false
  updateGuiState

  # Update Pointwise display
  pw::Display update
}

###############################################################################
# previewBtnPressed: 
# - Updates and redraws Pointwise Display based on chosen
# - Called by the GUI RadioButtons in makeWindow
###############################################################################
proc previewBtnPressed { dir } {
  global Propagate

  # While preview is being created, update window titile and disable GUI
  wm title . "Creating Butterfly Topology Preview" 

  # Run Job while catching & displaying errors
  if {[catch { runJob $dir $Propagate true } msg]} {
    handleError $msg
  }
}

###############################################################################
# listBoxInteract: respond to listbox item selection (clean and update gui)
###############################################################################
proc listBoxInteract {} {
  #clean false
  updateGuiState
}

###############################################################################
# cancelBtnPressed: abort changes and exit
###############################################################################
proc cancelBtnPressed { } {
  global Cancel_Job
  set Cancel_Job true
  clean true
  exit
}

###############################################################################
# okBtnPressed: 
# - Save changes by ending creator mode
# - exit
###############################################################################
proc okBtnPressed { } {
  runBtnPressed
  clean true
  exit
}

###############################################################################
# runBtnPressed: runs the current Butterfly topology conversion job
###############################################################################
proc runBtnPressed { } {
  global Direction Propagate guiTitle Last_Job_Type

  # While topology is being created, update window titile and disable GUI
  wm title . "Creating Butterfly Topology" 

  # Run Job while catching & displaying errors
  if {[catch {runJob $Direction $Propagate false} msg]} {
    handleError $msg
  }

  # Clear selection in list box
  set Propagate 0

  # Toplogy created, re-enable GUI and reset title
  wm title . $guiTitle

}

###############################################################################
# clean:
# - Deletes last transformed block (if necessary)
# - Deletes last Bfly Database surfaces (if necessary)
# - Ends or aborts creator state appropriately
###############################################################################
proc clean { isEnd } {
  global Last_Job_Type Cancel_Job Error_Last_Job Original_EntRenderAtt \
    End_Script tempEntCollection prevOgridCollection butterflyDomList

  if {[string compare $Last_Job_Type "Transform"] == 0} {
    if {$Cancel_Job || $Error_Last_Job} {

      setCreatorState abort
      deleteBflyDBSurf 
    } else {
      setCreatorState end
      deleteBflyDBSurf 
      deleteLastBlock 
      $tempEntCollection do "delete"
      $prevOgridCollection do "setRenderAttribute" "ColorMode" "Automatic"
    }
  } elseif {[string compare $Last_Job_Type "Preview"] == 0} {
    setCreatorState abort
  }

  if {$isEnd && !$End_Script} { 
    set End_Script true
    restoreConRenderAtts
  }

  set Last_Job_Type "None"
}

###############################################################################
# makeWindow: 
# - create GUI window for butterfly topology transformation
###############################################################################
proc makeWindow { } {
  global scriptDir guiTitle guiHeader defListBoxStringWidth

  # Create a title label
  label .title -text $guiHeader
  set font [.title cget -font]
  .title configure -font [font create -family [font actual $font -family] \
    -weight bold]
  pack .title -expand 1 -side top

  pack [frame .hr1 -bd 1 -height 2 -relief sunken] -fill x -pady 2
  pack [frame .content] -fill both -side top

  frame .contents
  frame .left

  # Create Radiobuttons For Directions "(I) (J) (K) (ALL)" 
  frame .left.dir -bd 1 -relief solid
  pack [radiobutton .left.dir.i -text "I" -command changeDirection \
    -variable Direction -value I] -side left -expand 1

  pack [radiobutton .left.dir.j -text "J" -command changeDirection \
    -variable Direction -value J] -side left -expand 1

  pack [radiobutton .left.dir.k -text "K" -command changeDirection \
    -variable Direction -value K] -side left -expand 1

  pack [radiobutton .left.dir.all -text "All" -command changeDirection \
    -variable Direction -value ALL] -side left -expand 1

  pack .left.dir -fill both -padx 2 -pady 0 -expand 1

  frame .left.values -bd 1 -relief solid

  # Allow user to input scaling factor in 2 dimensions for adjusting H region's
  # length and width.
  pack [makeInputField .left.values dist "H Region 3D Scaler (0,1.0):" \
    oScaleFac] -fill x -padx 2 -pady 4
  pack [makeInputField .left.values pts "Grid Points on Ogrid Ribs:" \
    oDimension] -fill x -padx 2 -pady 8
  checkbutton .left.values.propagate -text "Propagate Topology" \
    -variable Propagate 
  # Create button for previewing toplogy changes]
  button .left.values.preview -text "Preview Topology" \
    -command {previewBtnPressed $Direction}

  pack .left.values -fill x -padx 2 -pady 2
  pack .left.values.propagate -padx 2 -pady 8
  pack .left.values.preview -padx 2 -pady 8
  pack .left -in .contents -side right -expand 1

  frame .right -bd 1 -relief solid 

  # Change the position and the text of select block button. This command is 
  # not neccessary when the selection is already made in the GUI list.

  set listBoxPadY 5
  # Create button for interactive selection
  pack [button .right.select -text "Click to Select Interactively" \
    -command selectBtnPressed] -side bottom -pady $listBoxPadY -padx 20

  pack [frame .right.top] -side bottom 

  # Create scrollbar for blk listbox
  pack [scrollbar .right.top.scroll -command ".right.top.list yview" \
    -takefocus 0] -side right -fill y -pady $listBoxPadY
  
  # Create listbox for all available structured blocks
  pack [listbox .right.top.list -yscrollcommand ".right.top.scroll set" \
    -selectmode extended -exportselection false -takefocus 0 -height 8 \
    -width $defListBoxStringWidth -font TkFixedFont] -fill both -side right \
    -pady $listBoxPadY 

  # Populate block list on start
  .right.top.list configure -listvar ListBoxSelectableItems

  # Binds slecection in listbox to block selection in script
  bind .right.top.list <<ListboxSelect>> { listBoxInteract }

  pack .right -in .contents -side left -fill y 
  pack .contents -padx 10 -pady 10

  pack [frame .hr2 -bd 1 -height 2 -relief sunken] -fill x 
  frame .bottom
  frame .bottom.buttons -relief solid
  
  # Create Cancel button
  pack [button .bottom.buttons.cancel -text "Cancel" \
    -command  cancelBtnPressed] -padx 5 -pady 1 -side right -expand 1 -fill x

  # Create OK button
  pack [button .bottom.buttons.ok -text "OK" -command okBtnPressed]  \
    -padx 5 -pady 1 -side right -expand 1 -fill x

  # Create Run button
  pack [button .bottom.buttons.run -text "Run" -command runBtnPressed] \
    -padx 5 -pady 1 -side right -expand 1 -fill x

  pack [label .bottom.buttons.logo -image [pwLogo] -bd 0 -relief flat] \
    -side bottom -padx 5 -expand 1 -fill x
 
  pack .bottom.buttons -side bottom -fill x -padx 2 -pady 2 
  pack .bottom -side bottom -fill x

  # Add an empty label with pading to add some space between "left" and "right"
  # widgets
  pack [label .center] -padx 3
  pack .center -in .contents -side bottom -fill x

  # binds window destruction to clean
  bind . <Destroy> { clean true }

  # bind standard pointwise shortcuts for Cancel, Ok, & (Apply = Run)
  bind . <KeyPress-Escape> { .bottom.buttons.cancel invoke }
  bind . <Control-KeyPress-Return> { .bottom.buttons.ok invoke }
  bind . <Control-Shift-KeyPress-Return> { .bottom.buttons.run invoke }

  ::tk::PlaceWindow . widget
  wm title . $guiTitle

  # Disable window resize
  wm resizable . false false
}

###############################################################################
# pwLogo: get pointwise logo
###############################################################################
proc pwLogo { } {
  set logoData {
R0lGODlhiQAYAMZnAAAAAA4ODhQUFBsbGyQkJCwsLDMzMzs7O0REREtLS1RUVFtbW2JiYmtra3Jy
cnt7ewBjqgtqrgBmsABrtwBsuRJusBZwsRpzsyF2syJ4tSh7ty1/uQZ1xC+AujeFvDiFvT2IvjGQ
1EWNwUiPwk6TxEqVylSXxleYx1iZx1ybyGGeymSgy26mz3Coz2On122q1Wqt3XSq0Xuu03+x1Xqx
2HS04n+544ODg4qKipSUlJubm6Ojo6ysrLS0tLu8vIS01oi214q32I252ZK825i+2pzC3pXB4ZnI
6qPG4KXI4qvM46HL6rDP5bTR5rzW6b/Y6sTExMzMzM/R0dPT09ra2sTb68re7M/h79Di78ni9NXl
8d3q9N/u+OTk5Ovr6+Pu9ubw9+3z9+Xx+ery+PLy8vH3+vX5+///////////////////////////
/////////////////////////////////////////////////////////////////////////yH5
BAEKAH8ALAAAAACJABgAAAf+gH+Cg4SFhoeIiYqLjI2Oj5CRkpOUlZaPPDs5hDkKnp8KCzuKTk1K
f2BKp4ZQoF1/C6CgDlGDN7K4Cj5QCZ4NhlSGO58JXVGfC4NdoKOGt7mgPn/DDg43UIIJANvc3AiH
QlpBJE1DKDJFKUGFOt2v3fDbDoIG8fEPXd21ygOvhAHd/uQIKKgdt3mGCNiDNwqHjwVQdOAQxGDh
NgWEtKRQAUTLDywrIEDwkCTCB0I83P0BaBHAjT/aWgL4dYBbskEDJw6K0o3BnxvcAgzC0U3HIQQy
AfjoooPKAR9RNv2p2FLKICJbkvz4cubHmC9CZlQ5EwEDF0Epub1LOjXprx7+KgUpFECI6rYpf4hu
E4q2m85CSGWOeoBjCg8GzQJbfCBIiZgiT55wNaNCCRASKJpsYdGkr9o/SQH0aJAU4x8B3HTC3cZj
UDcDgvQC4PsnH7cBhwoknQilAQ8HRgX5yEFcB/GY23wuGSEDAhIkV84AUXElgsgTQUAU8bztFcuZ
OsLf+O6ACo/zPdDzSJ8e78/bgmpuI1CwWzPZtP8o6JbAUJQeAAYoIID+/LGPIrZtk0wIQVRwwRko
mPCDCDKUoUQIEMRQhEhH/JFWdytx454g+yUHSYJK6afPH/LNNlRQhKA40yWFGBPFjTcu0M1LHDT4
xBBMNHHCGUxAMEIYJ/z+0IJIMHioUjc91MXNL8pAYSU2R3HTHxXd/NLli3sVMlA3jA1CBY5oplmg
ISVa1IMVFBTxhQoZbvGDEiyIBIEWZuj5wh+rgQglITpuQ2Vs3YxISKAAvNKiAIXeBaaLhTwAT3As
JoWpIZEu9EcSElxRhUgt5LnnBSIp0QWdENDwhw8qfRflIJEeKlA3wRyi0DaMMfoaIfgdAlSi2Wia
iF32YFQDBEyEIdIQzUHwgwwYjBHSGCNAYAQRUzzJzayC1EqIQdvkakiwfwxgT2uT5keIpdzQB1NS
zRyCLDwHCGIDBEWEQ8IQrJJgRhOsatAEEyXIwOVnshI65bi4IoJilMP+duNuxe4S0qJo88r0lyF2
DbDDyDtg+YcWKwgBAQhJbGGFEk4MgQWrpJ4xQRULC/qtw4ZyEjEibeYrIwBltpsIiv0hRwBxOeDA
dA6KFmIXbImcoKcMSHyAqgVW6HlBE0+48AcUse5M68ODkAuAuaxE3CaIwMKYSIv5ttlfJHYVoMiG
Iqnwg57MLglCGSZAAIaTnw16ds9p/4zIrgD41C03+RaCLiLqbvNNm9/gzY3eiXzBBAgQXIAE4BUw
EcQXJIiUBeIgNrz4jDg5fsiYtOu2DbhGH0IFcpF3DEDnkOStSNZVILFCFRtYsEEKPxBpHQQycNco
aGaHi3Z93LB9CEvq/qRF17lyT0EAAQMI8N3OwA/g/vvps2vv54tka4IQX2xxcDopECFSCoP40PVk
pz3Gca9ci6jI3QSBmqLFLUxjawluhGcRqcxvG6BTxN9W5gTAQYAIIFgBSry1O57R7oBrWwTZ6tWW
NfWOJxbZB/AWwkLAEGQRWkABvywAuOqxg4QAkF+mLkKIigEgaocoBiF8UDlDOIAgZLPHAUaku5Z8
rBA7aIAWX+IIK2jhCTFAxxYOMYUHmBEff6hGNRSVRQf4hhBQcGM1XOgIOgoijmbkYhdwwMc+5kAa
nNCiIAcpSJPR6JCITKQiGREIADs=}

  return [image create photo -format GIF -data $logoData]
}  

# enable Tk in Glyph 2
pw::Script loadTk
catch {
  set scriptDir [file dirname [info script]]
  source [file join $scriptDir pwiLogo.glf]
}

# create the Tk window and place it
makeWindow
::tk::PlaceWindow . widget

# keep tk window "always on top"
wm attributes . -topmost yes

# Initialize Grid Info Lists
buildGridInfoList true

# Save original grid Render Attributes and set current conns to default colors
set allCons [pw::Grid getAll -type pw::Connector]
set allStrBlks [pw::Grid getAll -type pw::BlockStructured]
set Original_EntRenderAtt [buildConRenderList $allCons]
highlightBlockCons $allStrBlks $defColorMode $defColor $defWidth
pw::Display update

# Align all blocks to begin with. This keeps things consistent
buildAlignList
orientBlocks
preselectBlocks
# Initial Gui State update
updateGuiState

# process Tk events until the window is destroyed
tkwait window .

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
