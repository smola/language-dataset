#
# Copyright 2019 (c) Pointwise, Inc.
# All rights reserved.
#
# This sample Pointwise script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.

# ========================================================================================
# GeomToMesh: Main script
# ========================================================================================
# Written by Steve Karman, Nick Wyman & Spenser Owen

# Script for performing automatic mesh generation
# It is developed under contract to MIT for Automatic Mesh Generation
#
# This script can be run from the PW GUI or batch on the command line.
#
# The GUI mode will allow users to select two files:
#   1) Optional user defaults file that overrides the standard file GeomToMeshDefaults.glf
#   2) Required CAD file (IGES, STEP or NMB)
#
#
#  Script Flow Chart:
#
#    ------------------------------
#   |   Load GeomToMesh Defaults   |
#    ------------------------------    ----------------
#                |                    |                |
#    ------------------------------   |   --------------------------
#   |    Load User Defaults        |  |  |  Apply domain attributes |
#    ------------------------------   |   --------------------------
#                |                    |                |
#    ------------------------------   |   --------------------------
#   |       Load CAD File          |  |  |   Set up domain T-Rex    |
#    ------------------------------   |   --------------------------
#                |                    |                |
#    ------------------------------   |   --------------------------
#   |   Assemble tagged quilts     |  |  |  Domain refinement pass  |
#    ------------------------------   |   --------------------------
#                |                    |                |
#    ------------------------------   |   --------------------------
#   | Create domains from quilts   |  |  |  Assemble single block   |
#    ------------------------------   |   --------------------------
#                |                    |                |
#    ------------------------------   |   --------------------------
#   | Merge duplicate connectors   |  |  |  Apply block attributes  |
#    ------------------------------   |   --------------------------
#                |                    |                |
#    ------------------------------   |   --------------------------
#   |        Join connectors       |  |  |     Set up 3D T-Rex      |
#    ------------------------------   |   --------------------------
#                |                    |                |
#    ------------------------------   |   --------------------------
#   | Reduce connector dimension   |  |  |     Initialize Block     |
#   |  using avg sp and minDim     |  |   --------------------------
#    ------------------------------   |                |
#                |                    |   --------------------------
#    ------------------------------   |  |     Export CAE File      |
#   |  Process baffle geometries   |  |   --------------------------
#    ------------------------------   |                |
#                |                    |   --------------------------
#    ------------------------------   |  |          FINISH          |
#   | Increase connector dimension |  |   --------------------------   
#   |  using deviation or turning  |  |
#    ------------------------------   |
#                |                    |
#    ------------------------------   |
#   | Increase connector dimension |  |
#   |  using proximity test        |  |
#    ------------------------------   |
#                |                    |
#    ------------------------------   |
#   | Apply connector attributes   |  |
#    ------------------------------   |
#                |                    |
#    ------------------------------   |
#   |  Adjust connector dimension  |  |
#   |  using applied end spacing   |  |
#    ------------------------------   |
#                |                    |
#    ------------------------------   |
#   |   Setup periodic domains     |  |
#    ------------------------------   |
#                |                    |
#                ---------------------
#

# Load Glyph and TK

package require PWI_Glyph

# ----------------------------------------------
# Process filename from GUI
# ----------------------------------------------
proc getFileNameDlg { filenameVar { ftypes "" } { defFType "" } { initDir @@@@ } } {
    upvar $filenameVar filename

    # Does ftypes contain an * entry?
    set needAllTypes 1
    foreach ftype $ftypes {
        lassign $ftype desc exts macType
        if { "$defFType" == "" } {
            # default to first item in ftypes
            set defFType $desc
        }
        if { "[string trim $exts]" == "*" } {
            set needAllTypes 0
            break
        }
    }
    if { $needAllTypes } {
        # Silently add an {{All Files} *} entry
        lappend ftypes {{All Files} *}
    }

    if { ![file isdir $initDir] } {
        # Silently default to cwd
        set initDir $::cwd
    }

    set fullFilename [tk_getOpenFile -initialdir $initDir -filetypes $ftypes -typevariable defFType]
    set filename [file tail $fullFilename]
    puts "$filenameVar = $filename ($fullFilename)"

    return $fullFilename
}

# ----------------------------------------------
# Main procedure
# ----------------------------------------------
proc geomtomesh { } {

    # Start Time
    timestamp
    set tBegin [clock seconds]

    set version [pw::Application getVersion]
    puts $version

    # parameters from GUI or batch
    global conParams domParams blkParams genParams eoeParams
    global CADFile UserDefaultsFileName

    # validate geometry input file name
    if { $CADFile == "" } {
        if [pw::Application isInteractive] {
            set CADFile [tk_getOpenFile]
        }
    }

    if { ! [file readable $CADFile] } {
        puts "$CADFile does not exist or is not readable"
        exit -1
    }

    #    ------------------------
    #   |   Load User Defaults   |
    #    ------------------------

    if { $UserDefaultsFileName != "" } {
        if { ! [file readable $UserDefaultsFileName] } {
            puts "$UserDefaultsFileName does not exist or is not readable"
            exit -1
        }

        puts "Loading user defaults from $UserDefaultsFileName"
        SafeSource $UserDefaultsFileName
    }

    # echo PW defaults
    puts "GeomToMesh: Defaults"
    puts "Connector level"
    puts "    InitDim                  = $conParams(InitDim)"
    puts "    MaxDim                   = $conParams(MaxDim)"
    puts "    MinDim                   = $conParams(MinDim)"
    puts "    TurnAngle                = $conParams(TurnAngle)"
    puts "    Deviation                = $conParams(Deviation)"
    puts "    SplitAngle               = $conParams(SplitAngle)"
    puts "    JoinCons                 = $conParams(JoinCons)"
    puts "    ProxGrowthRate           = $conParams(ProxGrowthRate)"
    puts "    SourceSpacing            = $conParams(SourceSpacing)"
    puts "    TurnAngleHard            = $conParams(TurnAngleHard)"
    puts "Domain level"
    puts "    Algorithm                = $domParams(Algorithm)"
    puts "    FullLayers               = $domParams(FullLayers)"
    puts "    MaxLayers                = $domParams(MaxLayers)"
    puts "    GrowthRate               = $domParams(GrowthRate)"
    puts "    IsoType                  = $domParams(IsoType)"
    puts "    TRexType                 = $domParams(TRexType)"
    puts "    TRexARLimit              = $domParams(TRexARLimit)"
    puts "    TRexAngleBC              = $domParams(TRexAngleBC)"
    puts "    Decay                    = $domParams(Decay)"
    puts "    MinEdge                  = $domParams(MinEdge)"
    puts "    MaxEdge                  = $domParams(MaxEdge)"
    puts "    Adapt                    = $domParams(Adapt)"
    puts "    WallSpacing              = $domParams(WallSpacing)"
    puts "Block level"
    puts "    Algorithm                = $blkParams(Algorithm)"
    puts "    VoxelLayers              = $blkParams(VoxelLayers)"
    puts "    boundaryDecay            = $blkParams(boundaryDecay)"
    puts "    collisionBuffer          = $blkParams(collisionBuffer)"
    puts "    maxSkewAngle             = $blkParams(maxSkewAngle)"
    puts "    edgeMaxGrowthRate        = $blkParams(edgeMaxGrowthRate)"
    puts "    fullLayers               = $blkParams(fullLayers)"
    puts "    maxLayers                = $blkParams(maxLayers)"
    puts "    growthRate               = $blkParams(growthRate)"
    puts "    TRexType                 = $blkParams(TRexType)"
    puts "    volInitialize            = $blkParams(volInitialize)"
    puts "General"
    puts "    SkipMeshing              = $genParams(SkipMeshing)"
    puts "    CAESolver                = $genParams(CAESolver)"
    puts "    outerBoxScale            = $genParams(outerBoxScale)"
    puts "    sourceBoxLengthScale     = $genParams(sourceBoxLengthScale)"
    puts "    sourceBoxDirection       = $genParams(sourceBoxDirection)"
    puts "    sourceBoxAngle           = $genParams(sourceBoxAngle)"
    puts "    sourceGrowthFactor       = $genParams(sourceGrowthFactor)"
    puts "    sourcePCDFile            = $genParams(sourcePCDFile)"
    puts "    ModelSize                = $genParams(ModelSize)"
    puts "    writeGMA                 = $genParams(writeGMA)"
    puts "    assembleTolMult          = $genParams(assembleTolMult)"
    puts "    modelOrientIntMeshVolume = $genParams(modelOrientIntoMeshVolume)"
    puts "Elevate On Export"
    puts "    degree                   = $eoeParams(degree)"
    puts "    costThreshold            = $eoeParams(costThreshold)"
    puts "    maxIncAngle              = $eoeParams(maxIncAngle)"
    puts "    relax                    = $eoeParams(relax)"
    puts "    smoothingPasses          = $eoeParams(smoothingPasses)"
    puts "    WCNWeight                = $eoeParams(WCNWeight)"
    puts "    WCNMode                  = $eoeParams(WCNMode)"
    puts "    writeVTU                 = $eoeParams(writeVTU)"

    # set default parameters
    if { $conParams(MinDim) > $conParams(InitDim) } {
        set conParams(InitDim) $conParams(MinDim)
    }
    if { 2 > $conParams(MinDim) } {
        set conParams(MinDim) 2
    }

    pw::Application setGridPreference Unstructured

    pw::Connector setDefault Dimension $conParams(InitDim)
    pw::Connector setCalculateDimensionMaximum $conParams(MaxDim)

    pw::DomainUnstructured setDefault Algorithm $domParams(Algorithm)
    pw::DomainUnstructured setDefault BoundaryDecay $domParams(Decay)
    pw::DomainUnstructured setDefault IsoCellType $domParams(IsoType)
    pw::DomainUnstructured setDefault TRexCellType $domParams(TRexType)
    if { 0.0 < $domParams(MinEdge) } {
        pw::DomainUnstructured setDefault EdgeMinimumLength $domParams(MinEdge)
    }
    if { 0.0 < $domParams(MaxEdge) } {
        pw::DomainUnstructured setDefault EdgeMaximumLength $domParams(MaxEdge)
    }
    if { 0 < $domParams(FullLayers) } {
        pw::DomainUnstructured setDefault TRexFullLayers $domParams(FullLayers)
    }
    if { 0 < $domParams(MaxLayers) } {
        pw::DomainUnstructured setDefault TRexMaximumLayers $domParams(MaxLayers)
    }
    pw::DomainUnstructured setDefault TRexGrowthRate $domParams(GrowthRate)

    pw::BlockUnstructured setDefault EdgeMaximumGrowthRate $blkParams(edgeMaxGrowthRate)
    pw::GridEntity setDefault SizeFieldDecay $blkParams(boundaryDecay)
    pw::BlockUnstructured setDefault TRexGrowthRate $blkParams(growthRate)
    pw::BlockUnstructured setDefault TRexFullLayers $blkParams(fullLayers)
    pw::BlockUnstructured setDefault TRexCellType $blkParams(TRexType)
    pw::BlockUnstructured setDefault TRexCollisionBuffer $blkParams(collisionBuffer)

    if { ! [HaveDomSkipMeshing] } {
        puts "SkipMeshing not supported in this version of Pointwise."
        set genParams(SkipMeshing) 0
    }
    SetDomSkipMeshing $genParams(SkipMeshing)

    #    ------------------------
    #   |     Load CAD File      |
    #    ------------------------

    puts "CADFile = $CADFile"

    set importer [pw::Application begin DatabaseImport]
        if [catch { $importer initialize -strict -type Automatic $CADFile } msg] {
            puts "Error while reading CADFile:\n$CADFile"
            puts $msg
            catch { $importer abort }
            exit -1
        }
        if { $genParams(ModelSize) > 0.0 } {
            puts "Explicitly setting model size to $genParams(ModelSize)"
            pw::Database setModelSize $genParams(ModelSize)
        } else {
            puts "Determining model size from file."
            if [catch { $importer setAttribute FileModelSizeFromFile true } msg] {
                puts "Error while Model Size attribute."
                puts $msg
                puts "Leaving model size undefined."
            }
        }
        $importer read
        $importer convert
    $importer end

    set dbAssembleTol [maxDBEdgeTolerance]

    # create outer box if requested
    if { 0.0 < $genParams(outerBoxScale) } {
        # compute bounding box geometry with defined scale factor
        createOuterBox $genParams(outerBoxScale)
    }

    puts "Performing model assembly pass on geometry entities."
    set assembleList [concat [pw::Database getAll -type pw::Model] [pw::Database getAll -type pw::Quilt]]

    set modify [pw::Application begin Modify $assembleList]
        pw::Model assemble $assembleList
    $modify end

    # Determine maximum model assembly tolerance
    # and set minimum allowed edge spacing
    set ModelList [pw::Database getAll -type pw::Model]
    setMinEdgeFromModelTolerance $ModelList

    pw::Display update

    #    ------------------------
    #   | Assemble tagged quilts |
    #    ------------------------
    assembleTaggedQuilts

    #    ----------------------------
    #   | Create domains from quilts |
    #    ----------------------------
    set QuiltList [pw::Database getAll -type pw::Quilt]

    if 0 {
        puts "Dumping imported attributes for [pw::Database getCount] database entities"
        foreach dbEnt [pw::Database getAll] {
            dumpAttrs $dbEnt
        }
    }

    puts "Quilt list has [llength $QuiltList] entries."

    # Iteratively create domains from quilts.  If unsuccessful for a given
    # domain, then reset all, increase the initial connector dimension and try again.
    set flag 0
    while { 0 == $flag } {
        pw::Connector setDefault Dimension $conParams(InitDim)

        set flag 1
        set i 0
        foreach qlt $QuiltList {

            pw::DomainUnstructured createOnDatabase -splitConnectors $conParams(SplitAngle) \
                    -parametricConnectors EndToEnd -merge 0 -reject unusedSurfs $qlt
            if { [llength $unusedSurfs] > 0 } {
                puts "Quilt [expr $i+1], unused surfaces exist."
                foreach surf $unusedSurfs {
                    puts "  [$surf getName]"
                }
                incr conParams(InitDim) 1
                puts "Increasing base connector dimension to $conParams(InitDim)"
                if { $conParams(MinDim) < $conParams(InitDim) } {
                    set conParams(MinDim) $conParams(InitDim)
                }

                break
            }

            pw::Display update
            incr i 1
        }

        if { 0 == $flag } {
            # Reset system by force deleting all connectors
            set conList [pw::Grid getAll -type pw::Connector]
            set numcon [llength $conList]
            puts "Deleting all $numcon connectors"
            foreach con $conList {
                $con delete -force
            }
            puts "Deleted all $numcon connectors"
        }
    }

    pw::Display update

    # compute tolerance for connector operations
    set conList [pw::Grid getAll -type pw::Connector]
    puts "Original connector list has [llength $conList] entries."

    set minConLen 1.0e20
    foreach con $conList {
        set minConLen [expr min($minConLen, [$con getLength -parameter 1.0])]
    }
    puts [format "Minimum connector length = %.6g" $minConLen]
    set tol [expr $minConLen / $conParams(InitDim) * 0.5]
    if { 0.0 < $dbAssembleTol } {
        set tol $dbAssembleTol
    }
    puts [format "Tolerance = %.6g" $tol]

    #    ----------------------------
    #   | Merge duplicate connectors |
    #    ----------------------------

    if { $conParams(InitDim) > 3 } {
        # Perform merge operation to eliminate duplicate connectors
        if { 0 < [connectorMergeUsingEndpoints $conList] } {
            set conList [pw::Grid getAll -type pw::Connector]
            puts "After endpoint based merge, connector list has [llength $conList] entries."
        }
    }

    # Performing regular merge on connectors
    set mergeMode [pw::Application begin Merge]
        $mergeMode mergeConnectors -visibleOnly -exclude None -tolerance $tol
    $mergeMode end

    puts "After regular merge, connector list has [pw::Grid getCount -type pw::Connector] entries."

    #    -------------------------
    #   |     Join connectors    |
    #    -------------------------

    # perform join operation and eliminate breakpoints
    if { 1 == $conParams(JoinCons) } {
        if { 0 < [joinConnectors $conParams(SplitAngle)] } {
            set conList [pw::Grid getAll -type pw::Connector]
            puts "After join, connector list has [llength $conList] entries."
        }
    }

    pw::Display update

    # Create list of unique connector endpoints and spacing values.
    # This will be adjusted in several subsequent functions.
    set nodeList [list]
    set nodeSpacing [list]

    #    -----------------------------
    #   | Reduce connector dimension  |
    #   | using avg sp and minDim     |
    #    -----------------------------

    set conList [pw::Grid getAll -type pw::Connector]
    reduceConnectorDimensionFromAvgSpacing $conParams(MinDim) $conParams(MaxDim) $conList nodeList nodeSpacing

    puts "Number of unique endpoints = [llength $nodeList]"

    pw::Display update

    #    -----------------------------
    #   |  Process baffle geometries  |
    #    -----------------------------

    if { 0 < [processBaffleIntersections $tol] } {
        puts "Error processing baffles. Volume will not be initialized."
        set blkParams(volInitialize) 0
    }

    #  initialize domain T-Rex flag for connectors
    set softconTRex [list]
    set hardconTRex [list]

    #    ------------------------------
    #   | Increase connector dimension |
    #   | using deviation or turning   |
    #   | Set connector TRex flag      |
    #    ------------------------------

    set conList [pw::Grid getAll -type pw::Connector]
    if { 0.0 < $conParams(Deviation) || 0.0 < $conParams(TurnAngle) || 0.0 < $conParams(TurnAngleHard) } {
        increaseConnectorDimensionFromAngleDeviation $conList $conParams(MaxDim) $conParams(TurnAngle) \
            $conParams(Deviation) $conParams(TurnAngleHard) nodeList nodeSpacing softconTRex hardconTRex
    }

    pw::Display update

    #    ------------------------------
    #   | Increase connector dimension |
    #   | using proximity test         |
    #    ------------------------------

    # Perform proximity refinement from local connectors
    puts "Performing Local Proximity Refinement"
    connectorProximitySpacing $conParams(ProxGrowthRate) $conParams(MinDim) $conParams(MaxDim) \
        $conList nodeList nodeSpacing

    pw::Display update

    # conMaxDS will hold the desired maximum delta-S on each connector.  It is
    # initialized to the length of the connector.  If connector geometry
    # attributes are specified in adjustNodeSpacingFromGeometry it is adjusted.
    # Then it is passed to the final connector process connectorDimensionFromEndSpacing.
    set conMaxDS [list]
    foreach con $conList {
        set len [$con getLength -parameter 1.0]
        lappend conMaxDS $len
    }

    #    ----------------------------
    #   | Apply connector attributes |
    #    ----------------------------

    adjustNodeSpacingFromGeometry $blkParams(edgeMaxGrowthRate) $conParams(MinDim) $conParams(MaxDim) \
        conMaxDS nodeList nodeSpacing

    set i 0
    foreach con $conList {
        set len [$con getLength -parameter 1.0]
        set ds [lindex $conMaxDS $i]
        if { $ds < $len } {
            puts "Connector [$con getName]: Max DS changed $len to $ds"
        }
        incr i
    }

    pw::Display update

    #    ------------------------------
    #   | Adjust connector dimension   |
    #   | using applied end spacing    |
    #    ------------------------------

    # Adjust connector dimension based on endpoint spacing
    # and apply spacing values for tanh distribution
    if { 0 < $domParams(MaxLayers) && 0.0 < $domParams(TRexARLimit) } {
        set tAR $domParams(TRexARLimit)
    } else {
        set tAR 0.0
    }

    connectorDimensionFromEndSpacing $blkParams(edgeMaxGrowthRate) $conParams(MinDim) $conParams(MaxDim) \
        $conList $conMaxDS $tAR $nodeList $nodeSpacing $softconTRex $hardconTRex

    pw::Display update

    set domList [pw::Grid getAll -type pw::DomainUnstructured]
    puts "Domain list has [llength $domList] entries."

    #    ------------------------------
    #   |   Setup periodic domains     |
    #    ------------------------------

    set targetDomList [list]
    setupPeriodicDomains $tol targetDomList
    puts "Number of periodic target domains = [llength $targetDomList]"

    if { 0 < [llength $targetDomList] } {
        # reconstruct domain list minus target domain list
        set alldoms [pw::Grid getAll -type pw::DomainUnstructured]
        set domList [list]
        foreach dom $alldoms {
            set i [lsearch $targetDomList $dom]
            if { -1 == $i } {
                lappend domList $dom
            }
        }
        puts "After removing target domains, domains list has [llength $domList] entries."

        # Performing regular merge on connectors
        set mergeMode [pw::Application begin Merge]
            $mergeMode mergeConnectors -visibleOnly -exclude None -tolerance $tol
        $mergeMode end

        puts "After another regular merge, connector list has [pw::Grid getCount -type pw::Connector] entries."
        set conList [pw::Grid getAll -type pw::Connector]
    }

    if { [llength $domList] == 0 } {
        exit -1
    }

    pw::Display update

    #    -------------------------
    #   | Apply domain attributes |
    #    -------------------------

    # search for domain attributes from geometry
    loadDomainAttributes

    pw::Display update

    #    --------------------------
    #   |   Set up domain T-Rex    |
    #    --------------------------

    # set up domain T-Rex using end point spacing values
    if { 0 < $domParams(MaxLayers) } {
        setup2DTRexBoundaries $domList $domParams(TRexARLimit) $softconTRex $hardconTRex
    }

    #    ----------------------------------
    #   |  Adapt connectors using sources  |
    #    ----------------------------------

    if { 0 != $conParams(SourceSpacing) } {
        # Preform source refinement from connector spacing
        puts "Performing Source Cloud Refinement"
        connectorSourceSpacing $blkParams(boundaryDecay)
    }

    foreach dom $domList {
        $dom setUnstructuredSolverAttribute SwapCellsWithNoInteriorPoints True
        $dom setUnstructuredSolverAttribute TRexIsoTropicHeight [expr sqrt(3.0) / 2.0 ]
    }

    #    --------------------------
    #   |  Domain refinement pass  |
    #    --------------------------

    # do a refinement pass on all domains
    if { 0 == $genParams(SkipMeshing) } {
        puts "Performing refinement pass on all domains."
        foreach dom $domList {
            puts "  Refining domain [$dom getName]"
            set refineMode [pw::Application begin UnstructuredSolver [list $dom]]
            if { 0 != [catch { $refineMode run Refine } ] } {
                puts "  Running initialize instead for domain [$dom getName]"
                if { 0 != [catch { $refineMode run Initialize } ] } {
                    puts "    Initialize failed for domain [$dom getName]"
                    set blkParams(volInitialize) 0
                }
            }
            $refineMode end
            pw::Display update
        }
    } else {
        SetDomSkipMeshing false
        puts "Performing initialization pass on all domains."
        foreach dom $domList {
            puts "Initializing domain [$dom getName]"
            set initMode [pw::Application begin UnstructuredSolver [list $dom]]
            if { 0 != [catch { $initMode run Initialize } ] } {
                puts "Initialize failed for domain [$dom getName]"
                set blkParams(volInitialize) 0
            }
            $initMode end
            pw::Display update
        }
    }

    pw::Display update

    #    --------------------------
    #   |  Assemble single block   |
    #    --------------------------

    # create single unstructured block
    puts "Total number of domains = [llength $domList]"

    # assembling domain list that excludes baffles
    set domList [pw::Grid getAll -type pw::DomainUnstructured]
    puts "Domain list for assembly has [llength $domList] entries."
    set bdomList [list]
    set rdomList [list]
    set unusedDoms [list]
    set numBaffles 0
    foreach dom $domList {
        set baffle [domAttributeFromGeometry $dom "PW:Baffle"]
        if { "Baffle" == $baffle } {
            incr numBaffles 1
            puts "Baffle domain [$dom getName]"
            lappend bdomList $dom
        } else {
            lappend rdomList $dom
        }
    }
    if { 0 < $numBaffles } {
        puts "Number of baffles = $numBaffles"
    }

    puts "Creating unstructured block from [llength $rdomList] domains."

    # Attempt to create block
    set uBlk [AssembleBlockFromDomains $rdomList]

    # Generate source box if requested.
    if { 0.0 < $genParams(sourceBoxLengthScale) } {
        set bgsp [pw::GridEntity getDefault SizeFieldBackgroundSpacing]
        createSourceBox $genParams(sourceBoxLengthScale) $genParams(sourceBoxDirection) $genParams(sourceBoxAngle) \
            $genParams(sourceGrowthFactor) $blkParams(boundaryDecay) $bgsp
    }

    # Read PCD source file if specified
    if { "" != $genParams(sourcePCDFile) } {
        puts "Loading source file: $genParams(sourcePCDFile)"
        if { [catch { set smode [pw::Application begin SourceImport] } ] } {
            puts " Source Import not supported in this version of Pointwise."
        } else {
            $smode initialize -strict -type Automatic $genParams(sourcePCDFile)
            $smode read
            $smode convert
            $smode end
            unset smode
        }
    }

    pw::Display update

    # Add baffles to block
    if { 0 < $numBaffles } {
        set bmode [pw::Application begin Modify -notopology $uBlk]
        set bsub [subst {$uBlk}]
        foreach dom $bdomList {
            set ftmp [pw::FaceUnstructured create]
            $ftmp addDomain $dom
            $ftmp setBaffle true
            $bsub addFace $ftmp
        }
        $bmode end
    }

    pw::Display update

    set nblk [llength $uBlk]
    if { 1 > $nblk } {
        puts "Number of blocks = $nblk"
    }

    if { 1 == $nblk } {
        $uBlk setName "auto-blk"
        puts "Block = [$uBlk getName]"

        # set up unstructured block
        puts "  Setting up unstructured block."
        set solveMode [pw::Application begin UnstructuredSolver [list $uBlk]]

            if { 0.0 < $blkParams(maxSkewAngle) && 180.0 > $blkParams(maxSkewAngle) } {
                $uBlk setUnstructuredSolverAttribute TRexSkewCriteriaMaximumAngle $blkParams(maxSkewAngle)
            }
            if { [catch { $uBlk setUnstructuredSolverAttribute InteriorAlgorithm $blkParams(Algorithm) }] } {
                puts "  Solver InteriorAlgorithm not supported in this version of Pointwise."
            }
            if { [catch { $uBlk setUnstructuredSolverAttribute VoxelTransitionLayers $blkParams(VoxelLayers) }] } {
                puts "  Solver Voxel Transition Layers attribute not supported in this version of Pointwise."
            }

            #    --------------------------
            #   |  Apply block attributes  |
            #    --------------------------

            loadBlockAttributes $uBlk

            #    --------------------------
            #   |     Set up 3D T-Rex      |
            #    --------------------------

            set numLayers 0
            if { 0 < $blkParams(maxLayers) } {
                if { 1 == [setupTRexBoundaries $uBlk $domList] } {
                    set numLayers $blkParams(maxLayers)
                }
            }

            $uBlk setUnstructuredSolverAttribute TRexMaximumLayers $numLayers
            if { 1 < $blkParams(fullLayers) } {
                $uBlk setUnstructuredSolverAttribute TRexSkewCriteriaDelayLayers $blkParams(fullLayers)
            }
            $uBlk setUnstructuredSolverAttribute TRexIsotropicHeight [expr sqrt(2.0) / sqrt(3.0)]
            $uBlk setUnstructuredSolverAttribute IterationCount 11
            $uBlk setSizeFieldCalculationMethod MinimumValue

            if { 0 != $domParams(Adapt) } {
                setupDomAdapt $uBlk $domParams(Adapt)
            }

        $solveMode end
    }

    # initialize error count in case block initialization was not requested
    # this prevents exporting of CAE data
    set errorCount 1

    #    --------------------------
    #   |     Initialize Block     |
    #    --------------------------

    if { 1 == $nblk && 1 == $blkParams(volInitialize) } {
        puts "  Initializing unstructured block."
        set solveMode [pw::Application begin UnstructuredSolver $uBlk]
            $solveMode setStopWhenFullLayersNotMet true
            $solveMode run Initialize
        $solveMode end
        set errorCount [$uBlk getInitializationErrorCount]
    }

    pw::Display update

    #    --------------------------
    #   |     Export CAE File      |
    #    --------------------------

    if { "" != $genParams(CAESolver) } {
        if { 0 <= [lsearch [pw::Application getCAESolverNames] $genParams(CAESolver)] } {
            pw::Application setCAESolver $genParams(CAESolver)
            puts "CAE Solver set to $genParams(CAESolver)"
        } else {
            set genParams(CAESolver) ""
        }
        if { ! [catch { pw::Application setCAESolverAttribute ExportPolynomialDegree Q1 }] } {
            puts "Mesh order initialized to Q1"
        }
    }

    puts "GeomToMesh finished!"
    timestamp
    puts "Run Time: [convSeconds [pwu::Time elapsed $tBegin]]"

    set fname "[file rootname $CADFile].GeomToMesh.pw"
    puts "Pointwise save filename = $fname"
    pw::Application save $fname

    # export solver data
    if { 0 < $errorCount } {
        puts "Block not initialized."
    }

    if { "" != $genParams(CAESolver) && 0 == $errorCount } {

        # Start I/O Time
        timestamp
        set tBegin [clock seconds]

        set basename "[file rootname $CADFile].GeomToMesh"

        switch -- $genParams(CAESolver) {
            CGNS {
                if { "Q1" != $eoeParams(degree) } {
                    puts "Mesh order set to $eoeParams(degree)"
                    pw::Application setCAESolverAttribute {ExportPolynomialDegree} $eoeParams(degree)
                    pw::Application setCAESolverAttribute {ExportConvergenceCostThreshold} $eoeParams(costThreshold)
                    pw::Application setCAESolverAttribute {ExportMaxIncludedAngle} $eoeParams(maxIncAngle)
                    pw::Application setCAESolverAttribute {ExportStepSizeRelaxationFactor} $eoeParams(relax)
                    pw::Application setCAESolverAttribute {ExportWCNSmoothingPasses} $eoeParams(smoothingPasses)
                    pw::Application setCAESolverAttribute {ExportWCNWeightingFactor} $eoeParams(WCNWeight)
                    pw::Application setCAESolverAttribute {ExportWCNWeightingFactorMode} $eoeParams(WCNMode)
                    pw::Application setCAESolverAttribute {ExportWriteHOVTU} $eoeParams(writeVTU)
                }
                set fname "[file rootname $CADFile].GeomToMesh.cgns"
                puts "Exporting in CGNS format to filename = $fname"
                pw::Application export -precision Double $uBlk $fname
            }
            Gmsh {
                if { "Q1" != $eoeParams(degree) } {
                    puts "Mesh order set to $eoeParams(degree)"
                    pw::Application setCAESolverAttribute {ExportPolynomialDegree} $eoeParams(degree)
                    pw::Application setCAESolverAttribute {ExportConvergenceCostThreshold} $eoeParams(costThreshold)
                    pw::Application setCAESolverAttribute {ExportMaxIncludedAngle} $eoeParams(maxIncAngle)
                    pw::Application setCAESolverAttribute {ExportStepSizeRelaxationFactor} $eoeParams(relax)
                    pw::Application setCAESolverAttribute {ExportWCNSmoothingPasses} $eoeParams(smoothingPasses)
                    pw::Application setCAESolverAttribute {ExportWCNWeightingFactor} $eoeParams(WCNWeight)
                    pw::Application setCAESolverAttribute {ExportWCNWeightingFactorMode} $eoeParams(WCNMode)
                    pw::Application setCAESolverAttribute {ExportWriteHOVTU} $eoeParams(writeVTU)
                }
                set fname "[file rootname $CADFile].GeomToMesh.msh"
                puts "Exporting in Gmsh format to filename = $fname"
                pw::Application export -precision Double $uBlk $fname
            }
            UGRID {
                puts "Exporting in UGRID format to file basename = $basename"
                pw::Application export -precision Double -format Binary $uBlk $basename
            }
        }

        if { $genParams(writeGMA) } {
            set fname "[file rootname $CADFile].GeomToMesh.gma"
            writeEgadsAssocFile $uBlk $fname
        }

        puts "GeomToMesh I/O finished!"
        timestamp
        puts "Export Time: [convSeconds [pwu::Time elapsed $tBegin]]"
    }

    #    --------------------------
    #   |          FINISH          |
    #    --------------------------

    exit
}

# ----------------------------------------------
# Initialize Pointwise
# ----------------------------------------------

pw::Application reset
pw::Application clearModified

# ----------------------------------------------
# Define working directory and load scripts
# ----------------------------------------------

set scriptDir [file dirname [info script]]
source [file join $scriptDir "GMDatabaseUtility.glf"]
source [file join $scriptDir "GMMeshParamCoords.glf"]
source [file join $scriptDir "GMSafe.glf"]
source [file join $scriptDir "GMUtility.glf"]

#     --------------------------
#    | Load GeomToMesh Defaults |
#     --------------------------

SafeSource [file join $scriptDir "GeomToMeshDefaults.glf"]

# Initialize user defined files
set CADFile ""
set UserDefaultsFileName ""

if [pw::Application isInteractive] {
    # Interactive session uses GUI to specify files
    pw::Script loadTK

    set cwd [pwd]

    # ----------------------------------------------
    # select user defaults file
    # ----------------------------------------------

    proc setUserDefaults { } {
        global cwd UserDefaultsFileName

        cd $cwd
        if { $UserDefaultsFileName == "" } {
            set fullname [getFileNameDlg UserDefaultsFileName { {{GLYPH files} {.glf}} }]
            set UserDefaultsFileName $fullname
            set cwd [file dirname $fullname]
        }
    }

    # ----------------------------------------------
    # select CAD file
    # ----------------------------------------------

    proc setCAD { } {
        global cwd CADFile

        cd $cwd
        #puts "setCAD: cwd = $cwd"
        if { $CADFile != "" } {
            validateFile $CADFile exists
            puts "CADFile = $CADFile"
        } else {
            set ftypes {
                {{All CAD files} {.igs .iges .stp .step .nmb .egads}}
                {{IGES files}    {.igs}}
                {{IGES files}    {.iges}}
                {{STEP files}    {.stp}}
                {{STEP files}    {.step}}
                {{NMB files}     {.nmb}}
                {{EGADS files}   {.egads}}
            }
            set fullname [getFileNameDlg CADFile $ftypes]
            set CADFile $fullname
            set cwd [file dirname $fullname]
        }
    }

    # Configure Tk dialog
    wm title . "GeomToMesh"
    grid [ttk::frame .c -padding "5 5 5 5"] -column 0 -row 0 -sticky nwes
    grid columnconfigure . 0 -weight 1; grid rowconfigure . 0 -weight 1
    grid [ttk::labelframe .c.lf -padding "5 5 5 5" -text "Automatic Meshing"]

    grid [ttk::button .c.lf.udftl -text "User Defaults File" -command setUserDefaults]        -row 1 -column 1 -sticky e
    grid [ttk::entry .c.lf.udfte -width 40 -textvariable UserDefaultsFileName]                -row 1 -column 2 -sticky e
    grid [ttk::button .c.lf.cadfl -text "CAD File" -command setCAD]                           -row 2 -column 1 -sticky e
    grid [ttk::entry .c.lf.cadfe -width 40 -textvariable CADFile]                             -row 2 -column 2 -sticky e
    grid [ttk::button .c.lf.gob -text "Start" -command geomtomesh]                            -row 3 -column 2 -sticky e
    foreach w [winfo children .c.lf] {grid configure $w -padx 5 -pady 5}
    focus .c.lf.udftl
    ::tk::PlaceWindow . widget
    bind . <Return> { geomtomesh }

} else {
    # Batch: retrieve files from command line arguments
    set numargs [llength $argv]
    puts "Number of command line arguments = $numargs"
    if { 1 != $numargs && 2 != $numargs } {
        puts "Invalid command line input!"
        puts "Usage: tclsh GeomToMesh.glf <GeomFile> ?UserDefaultsFile?"
        exit -1
    }

    # first argument is the CAD file
    set CADFile [lindex $argv 0]

    if { 2 == [llength $argv] } {
        # second argument is the optional defaults file
        set UserDefaultsFileName [lindex $argv 1]
    }

    geomtomesh
}

# END SCRIPT

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
