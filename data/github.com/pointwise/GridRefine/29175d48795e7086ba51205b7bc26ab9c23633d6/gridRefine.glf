#
# Copyright 2013 (c) Pointwise, Inc.
# All rights reserved.
# 
# This sample script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.  
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#

# ==========================================================================
# GRID REFINEMENT SCRIPT - POINTWISE
# ==========================================================================
# Written by Travis Carrigan & Claudio Pita
#
#

# --------------------------------------------------------------------------
# User Defined Parameters
# --------------------------------------------------------------------------
# Refinement factor
set refinementFactor 2

# Pointwise file name, please include .pw file extension
set pwFile "TestGrid.pw"

# Whether to create volume mesh, YES or NO
set volMesh "YES"
# --------------------------------------------------------------------------



# Load Pointwise Glyph package
package require PWI_Glyph

# --------------------------------------------------------------------------
# Unstructure solver attribute names
set unsSolverAttsNames { BoundaryDecay EdgeMaximumLength EdgeMinimumLength \
  PyramidMaximumHeight PyramidMinimumHeight PyramidAspectRatio \
  InitialMemorySize IterationCount TRexMaximumLayers TRexFullLayers \
  TRexGrowthRate TRexPushAttributes TRexSpacingSmoothing \
  TRexSpacingRelaxationFactor TRexIsotropicSeedLayers TRexCollisionBuffer \
  TRexAnisotropicIsotropicBlend TRexSkewCriteriaDelayLayers \
  TRexSkewCriteriaMaximumAngle TRexSkewCriteriaEquivolume \
  TRexSkewCriteriaEquiangle TRexSkewCriteriaCentroid \
  TRexCheckCombinedElementQuality TRexVolumeFunction TetMesher }

# --------------------------------------------------------------------------
# Remove element from list
proc removeFromList { list values } {
  foreach val $values {
    set idx [lsearch $list $val]
    set list [lreplace $list $idx $idx]
  }
  return $list
}

# --------------------------------------------------------------------------
# Compare lists
proc compareLists { a b } {
  set la [llength $a]
  set lb [llength $b]
  if {$la != $lb} {
    return 0
  } else {
    set i 0
    foreach ae $a be $b {
      if {![string equal $ae $be]} {
        return 0
      }
      incr i
    }
  }
  return 1
}

# --------------------------------------------------------------------------
# Get connectors used by a domain
proc getConnectors { domain } {
  set conns [list]
  set edgeCount [$domain getEdgeCount]
  for {set i 1} {$i <= $edgeCount} {incr i} {
    set edge [$domain getEdge $i]
    set connectorCount [$edge getConnectorCount]
    for {set j 1} {$j <= $connectorCount} {incr j} {
      lappend conns [$edge getConnector $j]
    }
  }
  return $conns
}

# --------------------------------------------------------------------------
# Get domains used by a block
proc getDomains { block } {
  set doms [list]
  set faceCount [$block getFaceCount]
  for {set i 1} {$i <= $faceCount} {incr i} {
    set face [$block getFace $i]
    set domainCount [$face getDomainCount]
    for {set j 1} {$j <= $domainCount} {incr j} {
      lappend doms [$face getDomain $j]
    }
  }
  return $doms
}

# --------------------------------------------------------------------------
# Get boundary conditions
proc getBoundaryConditions {} {
  global boundaryConditions

  # Get list of boundary condition names
  set condsNames [pw::BoundaryCondition getNames]

  # Loop through all the condition names
  foreach name $condsNames {
    # Getcondition object from its name
    set condition [pw::BoundaryCondition getByName $name]

    # If condition exist, cache it
    if { $condition ne ""  && [$condition getPhysicalType] ne "Unspecified"} {
      set boundaryConditions($name) $condition
    }
  }
}

# --------------------------------------------------------------------------
# Get and redimension TRex conditions
proc getAndRedimensionTRexConditions {} {
  global refinementFactor
  global trexConditions

  # Get list of TRex condition names
  set condsNames [pw::TRexCondition getNames]

  # Loop through all the condition names
  foreach name $condsNames {
    # Get condition object from its name
    set condition [pw::TRexCondition getByName $name]

    # If condition exist, adjust spacing (if necessary) and cache it
    if { $condition ne "" && [$condition getConditionType] ne "Off" } {
      if { [$condition getConditionType] eq "Wall" } {
        # Get spacing
        set spc [$condition getSpacing]

        # Refine spacing
        set newSpc [expr (1.0 / $refinementFactor) * $spc]

        # Set new spacing
        $condition setSpacing $newSpc
      }

      # Cache it
      set trexConditions($name) $condition
    }
  }
}

# --------------------------------------------------------------------------
# Get volume conditions
proc getVolumeConditions {} {
  global volumeConditions

  # Get list of volume condition names
  set condsNames [pw::VolumeCondition getNames]

  # Loop through all the condition names
  foreach name $condsNames {
    # Get condition object from its name
    set condition [pw::VolumeCondition getByName $name]

    # If condition exist, cache it
    if { $condition ne ""  && [$condition getPhysicalType] ne "Unspecified"} {
      set volumeConditions($name) $condition
    }
  }
}

# --------------------------------------------------------------------------
# Get and redimension unstructured solver attributes for the block
proc getAndRedimensionUnstructuredSolverAttributes { blk } {
  global refinementFactor
  global unsSolverAttsNames

  set attributes [dict create]
  foreach name $unsSolverAttsNames {
    set value [$blk getUnstructuredSolverAttribute $name]

    # Redimension certain attributes
    switch $name {
      EdgeMinimumLength -
      EdgeMaximumLength {
        if { $value ne "Boundary" } {
          set value [expr {$value / $refinementFactor}]
        }
      }
      PyramidMinimumHeight -
      PyramidMaximumHeight {
        if { $value > 0 } {
          set value [expr {$value / $refinementFactor}]
        }
      }
    }
    dict set attributes $name $value
  }
  return $attributes
}

# --------------------------------------------------------------------------
# Match diagonalized domains with their structured counterpart
proc findMatchedOrigDiagDomains { } {
  global strDomList
  global blksRegenData
  global diagDomNameToOrigDom

  set blksRegenData(names) ""
  array set diagDomNameToOrigDom {}

  # Loop through all structured domains to find their diagonalized counterpart
  if { [llength $strDomList] > 0 } {
    foreach dom $strDomList {
      # Get list of connectors used by this domain
      set conList [getConnectors $dom]

      # Get list of domains adjacent to this domain
      set adjDomList [pw::Domain getAdjacentDomains $dom]
      foreach adjDom $adjDomList {
        # The diagonalized domain is unstructured
        if { [$adjDom isOfType "pw::DomainUnstructured"] } {
          # Get list of connectors used by this domain
          set adjConList [getConnectors $adjDom]

          # If the lists of connectos match then adjDom is the diagonalized
          # version of dom.
          if { [compareLists $conList $adjConList] } {
            # Add matching pair to the map
            set diagDomNameToOrigDom([$adjDom getName]) $dom

            # Cache data to regenerate unstructured blocks using the diagonalized
            # domain
            set blkList [pw::Block getBlocksFromDomains $adjDom]
            foreach blk $blkList {
              if { [$blk isOfType "pw::BlockUnstructured"] } {
                set domList [getDomains $blk]
                cacheBlockRegenerationData $adjDom $domList $blk
              }
            }
          }
        }
      }
    }
  }
}

# --------------------------------------------------------------------------
# Cache data necessary for block regeneration
proc cacheBlockRegenerationData { domRegenerate domList blk } {
  global blksRegenData
  global boundaryConditions
  global trexConditions

  set domName [$domRegenerate getName]
  set blkName [$blk getName]

  if { [lsearch -exact $blksRegenData(names) $blkName] == -1 } {
    # Name
    lappend blksRegenData(names) $blkName

    # Layer
    lappend blksRegenData($blkName,layer) [$blk getLayer]

    # Domain list (this list does not contain the diagonalized domain to be
    # regenerated)
    set domList [removeFromList $domList $domRegenerate]
    set blksRegenData($blkName,domains,keep) $domList

    # Domain names list (diagonalized domains to be regenerated)
    set blksRegenData($blkName,domains,regenerate) $domName

    # Attributes
    set blksRegenData($blkName,attributes) \
      [getAndRedimensionUnstructuredSolverAttributes $blk]

    # Boundary conditions
    cacheBoundaryConditions "boundary" $blk

    # TRex conditions
    cacheBoundaryConditions "trex" $blk

    # Volume conditions
    cacheVolumeConditions "volume" $blk
  } else {
    # Domain names list (diagonalized domains to be regenerated)
    if { [lsearch -exact $blksRegenData($blkName,domains,regenerate) \
         $domName] == -1 } {
      set domList $blksRegenData($blkName,domains,keep)
      set domList [removeFromList $domList $domRegenerate]
      set blksRegenData($blkName,domains,keep) $domList
      lappend blksRegenData($blkName,domains,regenerate) $domName
    }
  }
}

# --------------------------------------------------------------------------
# Cache boundary conditions (boundary, TRex)
proc cacheBoundaryConditions { type blk } {
  global blksRegenData
  global boundaryConditions
  global trexConditions

  switch $type {
    boundary { array set conditions [array get boundaryConditions] }
    trex { array set conditions [array get trexConditions] }
  }

  if { [array size conditions] > 0 } {
    foreach {name condition} [array get conditions] {
      set add 0
      set registers [$condition getRegisters]
      set blkName [$blk getName]
      foreach reg $registers {
        set highLevelEntity [lindex $reg 0]
        if { $highLevelEntity eq $blk } {
          # Boundary condition applied to this block, cache it
          set add 1
          set data [list [[lindex $reg 1] getName] [lindex $reg 2]]
          lappend blksRegenData($blkName,${type}Conditions,$name) $data
        }
      }
      if { $add == 1 } {
        lappend blksRegenData($blkName,${type}Conditions,names) $name
      }
    }
  }
}

# --------------------------------------------------------------------------
# Apply boundary conditions (boundary, TRex)
proc applyBoundaryConditions { type blk } {
  global blksRegenData
  global boundaryConditions
  global trexConditions

  set blkName [$blk getName]

  if {$type eq "boundary"}  {
    array set conditions [array get boundaryConditions]
  } elseif {$type eq "trex"} {
    array set conditions [array get trexConditions]
  }

  set argument "$blkName,${type}Conditions"
  if { [array names blksRegenData -exact $argument,names] ne "" } {
    foreach condName $blksRegenData($argument,names) {
      # Get BC
      set condition $conditions($condName)
      set bcData $blksRegenData($argument,$condName)

      # Build register
      foreach data $bcData {
        set orient [lindex $data 1]
        set domName [lindex $data 0]
        set dom [pw::GridEntity getByName [lindex $data 0]]
        set register [list $blk $dom $orient]
        $condition apply $register
      }
    }
  }
}

# --------------------------------------------------------------------------
# Cache volume conditions
proc cacheVolumeConditions { type blk } {
  global blksRegenData
  global volumeConditions

  if { [array size volumeConditions] > 0 } {
    foreach {name condition} [array get volumeConditions] {
      set entities [$condition getEntities]
      foreach entity $entities {
        if { $entity eq $blk } {
          set blkName [$blk getName]
          set blksRegenData($blkName,volumeCondition) $name
        }
      }
    }
  }
}

# --------------------------------------------------------------------------
# Apply volume conditions
proc applyVolumeConditions { blk } {
  global blksRegenData
  global volumeConditions

  set blkName [$blk getName]

  set argument "$blkName,volumeCondition"
  if { [array names blksRegenData -exact $argument] ne "" } {
    set condName $blksRegenData($argument)
    # Get VC
    set condition $volumeConditions($condName)
    $condition apply $blk
  }
}

# --------------------------------------------------------------------------
# Redimension balanced connectors
proc redimensionBalancedConnectors {} {
  global refinementFactor
  global strDomList
  global conList

  set refinedConList ""

  # Loop over structured domains, redimension connectors, and check for edge
  # dimension balance
  foreach domain $strDomList {
    set edgeDims {0 0}
    for {set i 1} {$i <= 4} {incr i} {
      set tmpConList ""
      set edge [$domain getEdge $i]
      set connectorCount [$edge getConnectorCount]
      for {set j 1} {$j <= $connectorCount} {incr j} {
        set connector [$edge getConnector $j]
        if { [lsearch -exact $refinedConList $connector] == -1 } {
          # Add connector to a temporary list of connectors to be refined now
          lappend tmpConList $connector
        }
      }

      # Refine connectors in this edge
      redimensionConnectors $tmpConList

      # Append to the list of refined connectors
      lappend refinedConList $tmpConList
      set refinedConList [join $refinedConList]

      # Remove from the list of connectors that still need to be refined
      set conList [removeFromList $conList $tmpConList]

      # A balanced structured domain requires edge_1 to be balanced with edge_3
      # and edge_2 to be balanced with edge_4. If edge is not balanced with its
      # opposing edge, attempt to balance them
      set idx [expr $i - 1]
      set edgeDim [$edge getDimension]
      if { $i <= 2 } {
        set edgeDims [lreplace $edgeDims $idx $idx $edgeDim]
      } elseif { $edgeDim != [lindex $edgeDims [expr $idx - 2]] } {
        balanceEdge [lindex $edgeDims [expr $idx - 2]] $edgeDim $tmpConList \
          $domain
      }
    }
  }
}

# --------------------------------------------------------------------------
# Redimension connectors
proc redimensionConnectors { conList } {
  global refinementFactor
  global numCons
  global conCnt

  set conMode [pw::Application begin Modify $conList]
  foreach con $conList {
    # Progress information
    incr conCnt
    puts ""
    puts "Refining connector $conCnt of $numCons..."
    puts "      ...connector [$con getName]"
    puts ""

    # Get connector distribution type
    set conDist [$con getDistribution 1]

    # Check if distribution is of type growth
    if { [$conDist isOfType "pw::DistributionGrowth"] } {
      # Decrease grid point spacing
      $conDist setBeginSpacing [expr {(1.0 / $refinementFactor) * \
        [[$conDist getBeginSpacing] getValue]}]
      $conDist setEndSpacing [expr {(1.0 / $refinementFactor) * \
        [[$conDist getEndSpacing] getValue]}]

      # Set optimal connector dimension
      $con setDimensionFromDistribution
    } else {
      # Increase connector dimension in 3 steps ...
      # 1) Store refined subconnector dimensions
      set totalDim 0
      set subConnCount [$con getSubConnectorCount]
      for {set i 1} {$i <= $subConnCount} {incr i} {
        set dim [expr {round($refinementFactor * \
          [$con getSubConnectorDimension $i] - 1)}]
        lappend conSubDim $dim
        incr totalDim $dim
      }

      # 2) Redimension connector
      $con setDimension [expr {$totalDim - ($subConnCount - 1)}]

      # 3) Adjust subconnector dimension
      if { $subConnCount > 1 } {
        $con setSubConnectorDimension $conSubDim
      }
      catch {unset conSubDim}

      # Decrease grid point spacing
      for {set i 1} {$i <= $subConnCount} {incr i} {
        set conDist [$con getDistribution $i]
        $conDist setBeginSpacing [expr (1.0 / $refinementFactor)* \
          [[$conDist getBeginSpacing] getValue]]
        $conDist setEndSpacing [expr (1.0 / $refinementFactor)* \
          [[$conDist getEndSpacing] getValue]]
      }
    }
  }
  $conMode end
  catch {unset conMode}
}

# --------------------------------------------------------------------------
# Balance the edges of free standing structured domains.
# Note: this function balnces free-standing structured domains only (basically
# structured domains used to create diagonalized unstructured domains
# that are not being used by any block)
proc balanceEdge { edgeBalancedDimension edgeDimension conList domain } {
  if { [llength [pw::Block getBlocksFromDomains $domain]] == 0 } {
    # How many points do we need to add/remove?
    set deltaPoints [expr abs($edgeBalancedDimension - $edgeDimension)]

    # Sort connectors according to their dimension
    set dimensionData [dict create]
    foreach con $conList {
      dict lappend dimensionData [$con getDimension] $con
    }
    set sortedDims [lsort -integer -decreasing [dict keys $dimensionData]]

    # Modify dimension
    # Note: The grid points are added to or removed from connectors with the
    # larger dimension first.
    foreach dim $sortedDims {
      # Get list of connectors with this dimension
      set conSubList [dict get $dimensionData $dim]

      # Set new dimension
      if { $edgeBalancedDimension > $edgeDimension } {
        incr dim 1
      } else {
        incr dim -1
      }

      # Redimension connectors
      foreach con $conSubList {
        $con setDimension $dim
        incr deltaPoints -1
        if { $deltaPoints == 0 } {
          # No more points need to be added/removed, return
          return
        }
      }
    }
  }
}

# --------------------------------------------------------------------------
# Redimension unstructured domains
proc redimensionDomains {} {
  global refinementFactor
  global domList
  global numDoms
  global domCnt

  foreach dom $domList {
    # Progress information
    incr domCnt
    puts ""
    puts "Refining domain $domCnt of $numDoms..."
    puts "      ...domain [$dom getName]"
    puts ""

    # Refine interior triangles of unstructured domains if necessary. Do not refine
    # diagonalized domains, they will be regenerated
    if { [$dom isOfType "pw::DomainUnstructured"] &&
         [array get diagDomNameToOrigDom $dom] eq "" } {
      set domMinEdgeLen [$dom getUnstructuredSolverAttribute EdgeMinimumLength]
      set domMaxEdgeLen [$dom getUnstructuredSolverAttribute EdgeMaximumLength]

      # Refine min. and max. edge length (if necessary)
      if { $domMinEdgeLen ne "Boundary" } {
        $dom setUnstructuredSolverAttribute EdgeMinimumLength \
          [expr {$domMinEdgeLen / $refinementFactor}]
      }
      if { $domMaxEdgeLen ne "Boundary" } {
        $dom setUnstructuredSolverAttribute EdgeMaximumLength \
          [expr {$domMaxEdgeLen / $refinementFactor}]
      }

      # Refine
      set unsSolver [pw::Application begin UnstructuredSolver $dom]
        if [catch {$unsSolver run Refine}] {
        lappend domError [$dom getName]
        $unsSolver end
        continue
      }
     $unsSolver end
    }
  }
  catch {unset unsSolver}

  # Write out unstructured domains that could not be refined due to solver error
  if { [info exists domError] } {
    set errMsg "Error refining [llength $domError] domain"
    printErrorInformation $domError $errMsg
  }
}

# --------------------------------------------------------------------------
# Regenerate diagonalized domains
proc regenerateDiagDomains {} {
  global diagDomNameToOrigDom
  global diagDomNameToDiagDom

  # Generate new diagonalized domains and delete old ones
  foreach {diagDomName origDom} [array get diagDomNameToOrigDom] {
    if { [catch {set newDiagDom [$origDom triangulate Aligned]}] } {
      if { [catch {set newDiagDom [$origDom triangulate]}] } {
        # Error during triangularization
        lappend domError $diagDomName
        continue
      }
    }
    set oldDiagDom [pw::GridEntity getByName $diagDomName]
    set oldDiagDomLayer [$oldDiagDom getLayer]
    $oldDiagDom delete -force
    $newDiagDom setName $diagDomName
    $newDiagDom setLayer $oldDiagDomLayer
    set diagDomNameToDiagDom($diagDomName) $newDiagDom
  }

  # Write out unstructured domains that could not be re-diagonalized
  if { [info exists domError] } {
    set errMsg "Error re-diagonalizing [llength $domError] domain"
    printErrorInformation $domError $errMsg
  }
}

# --------------------------------------------------------------------------
# Regenerate unstructured blocks
proc regenerateUnstructuredBlocks {} {
  global blksRegenData
  global diagDomNameToDiagDom
  global unsSolverAttsNames
  global trexConditions

  if { [array get blksRegenData names] ne "" } {
    set blkNameList $blksRegenData(names)

    foreach blkName $blkNameList {

      set domList $blksRegenData($blkName,domains,keep)
      # Add regenerated domains to the list. If there was an error
      # re-diagonalizing the domain, the old domain is already in
      # the list, no need to add it again
      foreach name $blksRegenData($blkName,domains,regenerate) {
        set dom [pw::GridEntity getByName $name]
        if { [lsearch -exact $domList $dom] == -1 } {
          # The domain was re-diagonalized
          lappend domList $dom
        }
      }

      # Create block
      if { [catch {set blk [pw::BlockUnstructured createFromDomains -reject \
           unused $domList]}] || [llength $unused] > 0 } {
        # Error during block generation
        lappend blkError $blkName
      } else {
        # Name
        if { [$blk getName] ne $blkName } {
          $blk setName $blkName
        }

        # Attributes
        set attributes $blksRegenData($blkName,attributes)
        foreach name [dict keys $attributes] {
          set value [dict get $attributes $name]
          if { $name ne "" && $value ne "" } {
            $blk setUnstructuredSolverAttribute $name $value
          }
        }

        # Boundary conditions
        applyBoundaryConditions "boundary" $blk

        # TRex conditions
        applyBoundaryConditions "trex" $blk

        # Volume conditions
        applyVolumeConditions $blk

        # Layer
        $blk setLayer $blksRegenData($blkName,layer)
      }
    }

    # Write out unstructured blocks that could not be regenerated
    if { [info exists blkError] } {
      set errMsg "Error re-generating [llength $blkError] block"
      printErrorInformation $blkError $errMsg
    }
  }
}

# --------------------------------------------------------------------------
# Initialize unstructured blocks
proc initializeUnstructuredBlocks {} {
  global refinementFactor
  global unsBlkList

  foreach unsBlk $unsBlkList {
    set unsSolver [pw::Application begin UnstructuredSolver $unsBlk]
    if [catch {$unsSolver run Initialize}] {
      lappend blkError [$unsBlk getName]
      $unsSolver end
      continue
    }
    $unsSolver end
    unset unsSolver
  }

  # Write out unstructured blocks that could not be initialized due to solver error
  if { [info exists blkError] } {
    set errMsg "Error initializing [llength $blkError] block"
    printErrorInformation $blkError $errMsg
  }
}

# --------------------------------------------------------------------------
# Print error information
proc printErrorInformation { entityList errMsg } {
  if { [ llength $entityList] > 0 } {
    # Print out error information
    if { [llength $entityList] == 1 } {
      set errMsg "${errMsg}:"
    } else {
      set errMsg "${errMsg}s:"
    }
    puts $errMsg
    foreach entity $entityList {
      puts "$entity"
    }
  }
}

# --------------------------------------------------------------------------
# Print block information
proc printBlockInformation {} {
  global blkList

  foreach blk $blkList {
    if { [$blk isOfType "pw::BlockStructured"] } {
      puts ""
      puts "Block [$blk getName]"
      puts "--------------------"
      puts "Block Type: Structured"
      puts "Total Cell Count: [$blk getCellCount]"
      puts ""
    } elseif {[$blk isOfType "pw::BlockUnstructured"]} {
      puts ""
      puts "Block [$blk getName]"
      puts "--------------------"
      puts "Block Type: Unstructured"
      if {[$blk getTRexCellCount]>0} {
        puts "Full TRex Layers:  [$blk getTRexFullLayerCount]"
        puts "Total TRex Layers: [$blk getTRexTotalLayerCount]"
        puts "Total TRex Cells:  [$blk getTRexCellCount]"
        puts "Total Cell Count:  [$blk getCellCount]"
        puts ""
      } else {
        puts "Total Cell Count: [$blk getCellCount]"
      }
    } elseif {[$blk isOfType "pw::BlockExtruded"]} {
      puts ""
      puts "Block [$blk getName]"
      puts "--------------------"
      puts "Block Type: Extruded"
      puts "Total Cell Count: [$blk getCellCount]"
      puts ""
    } else {
      puts ""
      puts "Block [$blk getName] type not supported by this script."
      puts ""
    }
  }
}

# --------------------------------------------------------------------------
# Save volume mesh
proc saveVolumeMesh { volStartTime volEndTime } {
  global refinementFactor
  global cwd
  global fileRoot

  set fileExport "$fileRoot-Volume-$refinementFactor.pw"
  puts ""
  puts "Writing $fileExport file..."
  puts "Volume initialization completed in [expr {$volEndTime-$volStartTime}] \
      seconds"
  puts ""
  pw::Application save [file join $cwd $fileExport]
}

# --------------------------------------------------------------------------
# Main script body

# Start timer
set startTime [clock seconds]

# Setup Pointwise and define working directory
pw::Application reset
pw::Application clearModified
set cwd [file dirname [info script]]

# File root
set fileRoot [file rootname $pwFile]

# Output Pointwise version information
puts ""
puts "[pw::Application getVersion]"
puts ""
puts "Refinement factor is set to $refinementFactor"
puts ""

# Check if refinement factor is lower or equal than 1
if { $refinementFactor <= 1 } {
  if { $volMesh eq "YES" } {
    # Load Pointwise file
    pw::Application load [file join $cwd $pwFile]

    # Save surface mesh
    set fileExport "$fileRoot-Surface-$refinementFactor.pw"

    puts ""
    puts "Writing $fileExport file..."
    puts ""
    pw::Application save [file join $cwd $fileExport]

    puts ""
    puts "Initializing volume mesh..."
    puts ""

    # Start timer
    set volStartTime [clock seconds]

    # Gather all blocks
    set blkList [pw::Grid getAll -type pw::Block]

    # Gather all unstructured blocks
    set unsBlkList [pw::Grid getAll -type pw::BlockUnstructured]

    # Initialize unstructured blocks
    initializeUnstructuredBlocks

    # End timer
    set volEndTime [clock seconds]

    # Print block information
    printBlockInformation

    # Save volume mesh
    saveVolumeMesh $volStartTime $volEndTime

    # End timer
    set endTime [clock seconds]
    puts ""
    puts "Pointwise script executed in [expr $endTime-$startTime] seconds"
    puts ""
    exit
  } else {
    puts ""
    puts "Refinement factor is 1 or lower, nothing to do..."
    puts ""
    exit
  }
}

# Load Pointwise file
pw::Application load [file join $cwd $pwFile]

# Start timer
set surfStartTime [clock seconds]

# Get current layer
set currentLayer [pw::Display getCurrentLayer]

# Gather all connectors
set conList [pw::Grid getAll -type pw::Connector]
set numCons [llength $conList]
set conCnt  0

# Gather all domains
set domList [pw::Grid getAll -type pw::Domain]
set numDoms [llength $domList]
set domCnt  0

# Gather all structured domains
set strDomList [pw::Grid getAll -type pw::DomainStructured]

# Gather all blocks
set blkList [pw::Grid getAll -type pw::Block]

# Gather all unstructured blocks
set unsBlkList [pw::Grid getAll -type pw::BlockUnstructured]

# Get boundary conditions
getBoundaryConditions

# Get and redimension TRex conditions
getAndRedimensionTRexConditions

# Get volume conditions
getVolumeConditions

# Match diagonalized domains with their structured counterpart
findMatchedOrigDiagDomains

# Redimension balanced connectors
redimensionBalancedConnectors

# Redimension connectors
redimensionConnectors $conList

# Redimension domains
redimensionDomains

# Regenerate diagonalized domains
regenerateDiagDomains

# Save surface mesh
set fileExport "$fileRoot-Surface-$refinementFactor.pw"

# End timer
set surfEndTime [clock seconds]

# Regenerate unstructured blocks
regenerateUnstructuredBlocks

puts ""
puts "Writing $fileExport file..."
puts "Surface refinement completed in [expr {$surfEndTime-$surfStartTime}]\
  seconds"
puts ""
pw::Application save [file join $cwd $fileExport]

# Initialize volume mesh if required
if { $volMesh eq "YES" } {
  puts ""
  puts "Initializing volume mesh..."
  puts ""

  # Start timer
  set volStartTime [clock seconds]

  # Unstructured blocks could have been regenerated, gather all blocks (again)
  set blkList [pw::Grid getAll -type pw::Block]

  # Unstructured blocks could have been regenerated, gather all unstructured \
  # blocks (again)
  set unsBlkList [pw::Grid getAll -type pw::BlockUnstructured]

  # Initialize unstructured blocks
  initializeUnstructuredBlocks

  # End timer
  set volEndTime [clock seconds]

  # Print block information
  printBlockInformation

  # Save volume mesh
  saveVolumeMesh $volStartTime $volEndTime
}

# Restore current layer
pw::Display setCurrentLayer $currentLayer

# End timer
set endTime [clock seconds]

puts ""
puts "Pointwise script executed in [expr $endTime-$startTime] seconds"
puts ""

#
# END SCRIPT
#

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

