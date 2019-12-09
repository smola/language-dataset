#
# Copyright 2013 (c) Pointwise, Inc.
# All rights reserved.
#
# This sample script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#

if { ![namespace exists pwio] } {

package require PWI_Glyph

#puts "pwio scriptdir='[file dirname [info script]]'"

namespace eval pwio {

  #========================================================================
  #                           utils namespace
  #========================================================================
  namespace eval utils {

    proc assert { cond msg {exitVal -1} } {
      if { ![expr $cond] } {
        puts "assert failed: ($cond)"
        puts "message      : $msg"
        if { 0 != $exitVal } {
          exit $exitVal
        }
      }
    }

    proc entBaseType { ent {subTypeVarName ""} } {
      if { "" != $subTypeVarName } {
        upvar 1 $subTypeVarName subType
      }
      if { 0 == [regexp {\mpw::(Block|Domain|Connector|Node*)(Structured|Unstructured|Extruded)*} [$ent getType] -> baseType subType] } {
        set baseType ""
        set subType ""
      } elseif { "" == $subType } {
        set subType $baseType
      }
      return $baseType
    }

    proc getBlockFaces { blk } {
      set faces [list]
      for {set ii 1} {$ii <= [$blk getFaceCount]} {incr ii} {
        lappend faces [$blk getFace $ii]
      }
      return $faces
    }

    proc getBlockDomains { blk } {
      set doms [list]
      foreach face [getBlockFaces $blk] {
        set doms [concat $doms [getFaceDomains $face]]
      }
      return $doms
    }

    proc getFaceDomains { face } {
      set doms [list]
      for {set ii 1} {$ii <= [$face getDomainCount]} {incr ii} {
        lappend doms [$face getDomain $ii]
      }
      return $doms
    }

    proc getFaceEdges { face } {
      set edges [list]
      for {set ii 1} {$ii <= [$face getEdgeCount]} {incr ii} {
        lappend edges [$face getEdge $ii]
      }
      return $edges
    }

    proc getEdgeConnectors { edge } {
      set cons [list]
      for {set ii 1} {$ii <= [$edge getConnectorCount]} {incr ii} {
        lappend cons [$edge getConnector $ii]
      }
      return $cons
    }

    proc getFaceEdgeConnectors { face } {
      set cons [list]
      foreach edge [getFaceEdges $face] {
        set cons [concat $cons [getEdgeConnectors $edge]]
      }
      return $cons
    }

    proc getPerimeterPointCount { ent } {
      set ret 0
      switch -exact [$ent getType] {
      pw::Node {
        set ret 0
      }
      pw::Connector {
        set ret 2
      }
      pw::DomainUnstructured {
        set ret [$ent getPerimeterPointCount]
      }
      pw::DomainStructured {
        lassign [$ent getDimensions] i j
        set ret [expr {($i * 2) + ($j * 2) - 4}]
      }
      pw::FaceUnstructured {
        foreach edge [getFaceEdges $ent] {
          incr ret [expr {[$edge getPointCount] - 1}]
        }
      }
      pw::FaceStructured {
        lassign [$ent getDimensions] i j
        set ret [expr {($i * 2) + ($j * 2) - 4}]
      }
      pw::BlockExtruded {
        lassign [$ent getDimensions] numBasePts one numExtrudePts
        set ret [expr {($numBasePts * 2) + (($numExtrudePts - 2) * \
                  [getPerimeterPointCount [$ent getFace JMinimum]])}]
      }
      pw::BlockUnstructured {
        if { [getSupportEnts $ent supEnts] } {
          foreach supEnt $supEnts {
            incr ret [getOwnedPointCount $supEnt]
          }
        }
      }
      pw::BlockStructured {
        lassign [$ent getDimensions] i j k
        set ret [expr {(($i * $j) + ($i * ($k - 2)) + (($j - 2) * ($k - 2))) * 2}]
      }
      default {
        assert 0 "getPerimeterPointCount: Bad Entity '[$ent getType]'"
      } }
      return $ret
    }

    proc getOwnedPointCount { ent } {
      set ret 0
      switch -exact [$ent getType] {
      pw::Node {
        set ret 1
      }
      pw::Connector {
        set ret [expr {[$ent getPointCount] - 2}]
      }
      pw::DomainUnstructured {
        set ret [expr {[$ent getPointCount] - [$ent getPerimeterPointCount]}]
      }
      pw::DomainStructured {
        lassign [$ent getDimensions] i j
        set ret [expr {($i - 2) * ($j - 2)}]
      }
      pw::BlockUnstructured {
        set ret [expr {[$ent getPointCount] - [getPerimeterPointCount $ent]}]
      }
      pw::BlockStructured {
        lassign [$ent getDimensions] i j k
        set ret [expr {($i - 2) * ($j - 2) * ($k - 2)}]
      }
      pw::BlockExtruded {
        set ret [expr {[$ent getPointCount] - [getPerimeterPointCount $ent]}]
      }
      }
      return $ret
    }

    proc isBndryEnt { ent allEnts } {
      # getRegisterBoundaryConditions returns a list of {{register} parentEntity} lists
      #   { {::pw::BlockStructured_2 ::pw::DomainStructured_283 Opposite} ::pw::BoundaryCondition_46 }
      #   { {::pw::BlockStructured_3 ::pw::DomainStructured_283 Same}     ::pw::BoundaryCondition_46 }
      set regBCs [$ent getRegisterBoundaryConditions]
      set numParents [llength $regBCs]
      if {1 == $numParents} {
        return true ;# hard bndry ents have only one parent
      } elseif { $numParents > 2 } {
        # non-manifold topology!
        assert 0 "$ent $numParents has NONMANIFOLD CONNECTIVITY!"
      }
      # ent has 2 parents and is a cnxn UNLESS BC is applied
      set ret false ;# assume cnxn
      foreach regBC $regBCs {
        lassign $regBC reg bc
        lassign $reg parentEnt cnxnEnt orient
        if { -1 == [lsearch -exact $allEnts $parentEnt] } {
          # reg parent ent is not part of the export. ent is a bndry
          set ret true
          break
        }
        set bcName [$bc getName]
        if { $bcName != "Unspecified" } {
          # cnxn ent has BCs assigned it is a bndry
          set ret true
          #break
        }
      }
      return $ret
    }

    proc getNodeDbEnt { node dbEntVarName } {
      upvar 1 $dbEntVarName dbEnt
      set dbEnt [lindex [$node getPoint] 2]
      if { "::pw::" != [string range $dbEnt 0 5] } {
        set dbEnt ""
      }
      return [expr {0 != [string length $dbEnt]}]
    }

    proc entLockInterior { ent } {
      switch -exact [$ent getType] {
      pw::Node -
      pw::Connector -
      pw::DomainUnstructured -
      pw::DomainStructured -
      pw::BlockUnstructured -
      pw::BlockExtruded {
        # do nothing
      }
      pw::BlockStructured {
        while { [$ent getInteriorState] == "Locked" } {
          $ent unlockInterior
        }
        $ent lockInterior
      }
      }
    }

    proc entUnlockInterior { ent {clearAllLocks 0} } {
      switch -exact [$ent getType] {
      pw::Node -
      pw::Connector -
      pw::DomainUnstructured -
      pw::DomainStructured -
      pw::BlockUnstructured -
      pw::BlockExtruded {
        # do nothing
      }
      pw::BlockStructured {
        $ent unlockInterior
        while { $clearAllLocks && [$ent getInteriorState] == "Locked" } {
          $ent unlockInterior
        }
      }
      }
    }

    proc entGetName { ent } {
      set baseType [entBaseType $ent]
      switch -exact $baseType {
      Block -
      Domain -
      Connector {
        set name [$ent getName]
      }
      Node {
        # ::pw::Node_195
        set name [string range $ent 6 end]
      } }
      return $name
    }

    proc entGetDimensions { ent } {
      switch -exact [entBaseType $ent] {
      Block     { set dim [$ent getDimensions] }
      Domain    { set dim [$ent getDimensions] ; lappend dim 1 }
      Connector { set dim [list [$ent getDimension] 1 1] }
      Node      { set dim [list 1 1 1] }
      default   { assert false "Invalid Ent Type ([entBaseType $ent])" }
      }
      return $dim ;# returns {i j k}
    }

    proc entIjkToIndex { ent ijk } {
      set ret 0
      set baseType [entBaseType $ent subType]
      switch -exact $subType {
      Node -
      Connector -
      Unstructured {
        set ret [lindex $ijk 0] ;# grab the i
      }
      Structured -
      Extruded {
        if { "Domain" == $baseType } {
          set ijk [lrange $ijk 0 1] ;# convert to ij
        }
        set ret [$ent getLinearIndex $ijk]
      }
      default {
        assert false "bad subType '$subType'"
      }
      }
      return $ret
    }

    proc ijkToIndexStructured { ijk ijkdim } {
      lappend ijk 1 1 ;# handle an i or ij value
      lassign $ijk i j k
      lappend ijkdim 0 0 ;# handle an i or ij dim value
      lassign $ijkdim idim jdim kdim
      # ijk are 1-based. Do not subtract from $i to return a 1-based index
      return [expr {($k - 1) * ($idim * $jdim) + ($j - 1) * $idim + $i}]
    }

    proc indexToIjkStructured { ndx ijkdim } { ;# ndx must be 1-based
      lappend ijkdim 1 ;# add a null kdim to pad an ij-only value
      lassign $ijkdim idim jdim kdim
      assert "$ndx > 0 && $ndx <= ($idim * $jdim * $kdim)" \
             "indexToIjkStructured: Invalid ndx ($ndx)"
      incr ndx -1 ;# convert to 0-based index
      if { $kdim != 0 } { ;# ijk
        set k [expr {$ndx / ($idim * $jdim)}]
        incr ndx [expr {-$k * $idim * $jdim}]
      } else { ;# ij
        set k 0
      }
      set j [expr {$ndx / $idim}]
      incr ndx [expr {-$j * $idim + 1}] ;# +1 makes ndx 1-based
      return [list $ndx [incr j] [incr k]] ;# return 1-based ijk's
    }

    proc entIndexToIjk { ent entNdx1 } { ;# entNdx1 must be 1-based
        assert "$entNdx1 >= 1 && $entNdx1 <= [$ent getPointCount]" \
               "entIndexToIjk: Bad Index $entNdx1 for $ent"
        set ret 0
        set baseType [entBaseType $ent subType]
        switch -exact $subType {
        Node -
        Connector -
        Unstructured {
          set ret [list $entNdx1 1 1]
        }
        Structured -
        Extruded {
          set ret [indexToIjkStructured $entNdx1 [$ent getDimensions]]
        }
        default {
          assert false "bad subType '$subType'"
        }
        }
        return $ret
    }

    proc makeCoord { ent ijk } { ;# 1-based ijk
      return [lappend ijk $ent]
    }

    proc makeCoordFromIjkVals { ent i j k } { ;# 1-based i j k
      return [makeCoord $ent [list $i $j $k]]
    }

    proc makeCoordFromEntIndex { ent ndx } { ;# 1-based ndx relative to ent's pt space
      return [makeCoord $ent [entIndexToIjk $ent $ndx]]
    }

    proc sortEntsByType { ents } {
      proc compareBaseTypes {type1 type2} {
        array set rank {Block 0 Domain 1 Connector 2 Node 3}
        return [expr {$rank([entBaseType $type1]) - $rank([entBaseType $type2])}]
      }
      # sort ents in Block/Domain/Connector/Node order
      return [lsort -command compareBaseTypes $ents]
    }

    proc pointToString { pt } {
      # pt == {u v ::pw::Surface_1}  or  {x y z}
      lassign $pt xu yv zdb
      set prec "%8.5f"
      if { [string equal -length 6 "::pw::" $zdb] } {
        set fmt "%s"
      } else {
        set fmt $prec
      }
      return [format "\{$prec $prec $fmt\}" $xu $yv $zdb]
    }

    proc xyzEqual { xyz1 xyz2 {tol 1.0e-8} } {
      lassign $xyz1 x1 y1 z1
      lassign $xyz2 x2 y2 z2
      return [expr {[valEqual $x1 $x2 $tol] && [valEqual $y1 $y2 $tol] && [valEqual $z1 $z2 $tol]}]
    }

    proc valEqual { val1 val2 {tol 1.0e-8} } {
      return [expr {abs($val1 - $val2) < $tol}]
    }

    proc coordToPtString { coord } {
      # pt == {u v ::pw::Surface_1}  or  {x y z}
      set pt [pw::Grid getPoint $coord]
      set ret [pointToString $pt]
      if { -1 != [string first "::pw::" $ret] } {
        set xyz [pw::Grid getXYZ $coord]
        set ret "[pointToString $xyz] @ $ret"
      }
      return $ret
    }

    proc vcToString { vc } {
      set vcName [$vc getName]
      if {$vcName == "Unspecified" } {
        return $vcName
      }
      return "$vcName [$vc getId] [$vc getPhysicalType]"
    }

    proc labelPt { ndx pt } {
      lassign $pt x y z
      set noteHt 0.04
      set note [pw::Note create]
      $note setText "$ndx"
      $note setPosition [list $x $y $z]
      $note setSize $noteHt
      $note setColor 0x0000FF00
      $note setRenderAttribute ColorMode Entity
    }

    proc printEntInfo { title ents {dim 0} {allEnts {}} } {
      proc compareBaseTypes {type1 type2} {
        array set rank {Block 0 Domain 1 Connector 2 Node 3}
        return [expr {$rank([entBaseType $type1]) - $rank([entBaseType $type2])}]
      }
      # sort ents in Block/Domain/Connector/Node order
      set ents [lsort -command compareBaseTypes $ents]

      set fmt "| %-30.30s | %-20.20s | %10.10s | %6.6s | %-13.13s | %-10.10s | %-5.5s |"
      puts "$title"
      puts [format $fmt "Entity" "Name" "NumPts" "DbPts" "Dim" "BaseType" "BorC"]
      set tmp [string repeat "-" 50]
      puts [format $fmt $tmp $tmp $tmp $tmp $tmp $tmp $tmp]
      foreach ent $ents {
        array set eInfo [list Name "[entGetName $ent]" NumPts "" DbPts "" Dim "" Bndry ""]
        set baseType [entBaseType $ent]
        switch -exact $baseType {
        Block -
        Domain {
          #set eInfo(Name)     [$ent getName]
          set eInfo(NumPts)   [$ent getPointCount -constrained eInfo(DbPts)]
          set eInfo(Dim)      [$ent getDimensions]
          if { $baseType == "Domain" && $dim == 3 } {
            if { [isBndryEnt $ent $allEnts] } {
              set eInfo(Bndry)   "Bndry"
            } else {
              set eInfo(Bndry)   "Cnxn"
            }
          }
        }
        Connector {
            #set eInfo(Name)     [$ent getName]
            set eInfo(NumPts)   [$ent getPointCount -constrained eInfo(DbPts)]
            set eInfo(Dim)      [$ent getDimension]
            if { $dim == 2 } {
              if { [isBndryEnt $ent $allEnts] } {
                set eInfo(Bndry)   "Bndry"
              } else {
                set eInfo(Bndry)   "Cnxn"
              }
            }
        }
        Node {
          #set eInfo(Name)     ""
          set eInfo(NumPts)   1
          set eInfo(DbPts)    [getNodeDbEnt $ent ignore]
          set eInfo(Dim)      1
        } }
        if { $eInfo(DbPts) == 0 } {
          set eInfo(DbPts) ""
        }
        puts [format $fmt $ent $eInfo(Name) $eInfo(NumPts) $eInfo(DbPts) $eInfo(Dim) $baseType $eInfo(Bndry)]
      }
    }

    proc getSelection { selType selectedVarName errMsgVarName } {
      upvar 1 $selectedVarName selected
      upvar 1 $errMsgVarName errMsg
      array set validSelTypes { \
        Connector 0 \
        Domain    1 \
        Block     2 \
        Database  3 \
        Spacing   4 \
        Boundary  5 \
      }
      array set typeFilters { \
        Connector			{Dimensioned} \
        Domain				{Defined} \
        Block				{Defined} \
        Database			{Models Quilts} \
        Spacing				{Begin End} \
        DatabaseBoundary	{} \
      }
      set ret 0
      set selected {}

      set allEnts [pw::Grid getAll -type "pw::$selType"]
      set gridCnt 0
      foreach ent $allEnts {
        if { [$ent getEnabled] && [pw::Display isLayerVisible [$ent getLayer]] } {
          incr gridCnt
          lappend selected $ent
        }
      }

      if { "" == [array get validSelTypes $selType] } {
        set errMsg "Invalid Selection Type '$selType'"
      } elseif { 0 == $gridCnt } {
        set errMsg "No appropriate $selType entities are available for selection!"
      } elseif { 1 == $gridCnt } {
        # force selection of only $selType ent available
        set ret 1
      } else {
		set filter $typeFilters($selType)
        # set selection based on current 2D/3D setting
        set mask [pw::Display createSelectionMask -require$selType $filter]
        if { [pw::Display selectEntities \
              -description "Select $selType\s" \
              -selectionmask $mask \
              picks] } {
          set selected $picks($selType\s)
          set ret 1
        } else {
          set errMsg "$selType selection aborted!"
        }
      }
      return $ret
    }

    # returns true/false
    # support ents placed in supEntsVarName
    # if addEnts is true, ents will also be added to supEntsVarName
    proc getSupportEnts { ents supEntsVarName {addEnts false}} {
      upvar 1 $supEntsVarName supEnts
      set supEnts [list]
      array set unique {} ;# empty array - used to track unique ent names
      foreach ent $ents {
        if { "" == [set base [entBaseType $ent]] } {
          continue ; # BAD!
        }
        set funcName "getSupportEnts_Private::push$base\AndSupportEnts"
        $funcName $ent unique $addEnts
      }
      # extract the list of unique entity names
      set supEnts [array names unique]
      return [expr {0 != [llength $supEnts]}]
    }

    namespace eval getSupportEnts_Private {

      proc pushBlockAndSupportEnts { blk uniqueVarName {addBlk true}} {
        upvar 1 $uniqueVarName unique
        if { "" == [array names unique -exact $blk] } {
          if { $addBlk } {
            set unique($blk) 1 ;# add to array
          }
          set faceCnt [$blk getFaceCount]
          for {set ii 1} {$ii <= $faceCnt} {incr ii} {
            set face [$blk getFace $ii]
            set domCnt [$face getDomainCount]
            for {set jj 1} {$jj <= $domCnt} {incr jj} {
              pushDomainAndSupportEnts [$face getDomain $jj] unique
            }
          }
        }
      }

      proc pushDomainAndSupportEnts { dom uniqueVarName {addDom true}} {
        upvar 1 $uniqueVarName unique
        if { "" == [array names unique -exact $dom] } {
          if { $addDom } {
            set unique($dom) 1 ;# add to array
          }
          set edgeCnt [$dom getEdgeCount]
          for {set ii 1} {$ii <= $edgeCnt} {incr ii} {
            set edge [$dom getEdge $ii]
            set conCnt [$edge getConnectorCount]
            for {set jj 1} {$jj <= $conCnt} {incr jj} {
              pushConnectorAndSupportEnts [$edge getConnector $jj] unique
            }
          }
        }
      }

      proc pushConnectorAndSupportEnts { con uniqueVarName } {
        upvar 1 $uniqueVarName unique
        # if con is not in array, process it
        if { "" == [array names unique -exact $con] } {
          set unique($con) 1 ;# add to array
          pushNodeAndSupportEnts [$con getNode Begin] unique
          pushNodeAndSupportEnts [$con getNode End] unique
        }
      }

      proc pushNodeAndSupportEnts { node uniqueVarName } {
        upvar 1 $uniqueVarName unique
        # if node is not in array, process it
        if { "" == [array names unique -exact $node] } {
          set unique($node) 1 ;# add to array
          # nodes do NOT have support ents - all done!
        }
      }
    } ;# namespace eval getSupportEnts_Private
  }



    #========================================================================
    #                           cell namespace
    #========================================================================
    namespace eval cell {

        # key = "dim,numCellPts"
        # val = "edge1p1 edge1p2  edge2p1 edge2p2 ... edgeNp1 edgeNp2"
        variable edgeMap ; array set edgeMap {
          "2e3" { 0 1  1 2  2 0 } ;# tri
          "2e4" { 0 1  1 2  2 3  3 0 } ;# quad
          "3e4" { 0 1  1 2  2 0  0 3  1 3  2 3 } ;# tet
          "3e5" { 0 1  1 2  2 3  3 0  0 4  1 4  2 4  3 4 } ;# pyramid
          "3e6" { 0 1  1 2  2 0  3 4  4 5  5 3  0 3  1 4  2 5 } ;# prism
          "3e8" { 0 1  1 2  2 3  3 0  4 5  5 6  6 7  7 4  0 4  1 5  2 6  3 7 } ;# hex
        }
        # key = "dim,numCellPts"
        # val = "{face1} {face2} ... {faceN}"
        variable faceMap ; array set faceMap {
          "2f3" { {0 1}   {1 2}   {2 0} } ;# tri
          "2f4" { {0 1}   {1 2}   {2 3}   {3 0} } ;# quad
          "3f4" { {0 1 2}   {0 3 1}   {1 3 2}   {2 3 0} } ;# tet
          "3f5" { {0 1 2 3} {0 4 1}   {1 4 2}   {2 4 3}   {3 4 0} } ;# pyramid
          "3f6" { {0 1 2}   {3 5 4}   {0 3 4 1} {1 4 5 2} {2 5 3 0} } ;# prism
          "3f8" { {0 1 2 3} {4 7 6 5} {0 4 5 1} {1 5 6 2} {2 6 7 3} {3 7 4 0} } ;# hex
        }

        proc getEdges { cell {minFirstOrder 0} {revVarName ""} } {
          if { "" != $revVarName } {
            upvar 1 $revVarName rev
          }
          set key "$pwio::caeDim\e[llength $cell]"

          variable edgeMap
          pwio::utils::assert "\"\" != \"[array names edgeMap $key]\"" "Invalid cell edgeMap key: '$key'"
          set ret [list]
          set rev [list]
          set map $edgeMap($key)
          foreach {i1 i2} $map {
            set v1 [lindex $cell $i1]
            set v2 [lindex $cell $i2]
            if { $minFirstOrder && ($v2 < $v1) } {
              lappend ret [list $v2 $v1]
              lappend rev 1
            } else {
              lappend ret [list $v1 $v2]
              lappend rev 0
            }
          }
          return $ret
        }

        proc getFaceEdges { face {minFirstOrder 0} {revVarName ""} } {
          if { "" != $revVarName } {
            upvar 1 $revVarName rev
          }
          set key "2\e[llength $face]"

          variable edgeMap
          pwio::utils::assert "\"\" != \"[array names edgeMap $key]\"" "Invalid cell edgeMap key: '$key'"
          set ret [list]
          set rev [list]
          set map $edgeMap($key)
          foreach {i1 i2} $map {
            set v1 [lindex $face $i1]
            set v2 [lindex $face $i2]
            if { $minFirstOrder && ($v2 < $v1) } {
              lappend ret [list $v2 $v1]
              lappend rev 1
            } else {
              lappend ret [list $v1 $v2]
              lappend rev 0
            }
          }
          return $ret
        }

        proc getFaces { cell {minFirstOrder 0} } {
          variable faceMap
          set f f
          set key "$pwio::caeDim$f[llength $cell]"
          pwio::utils::assert "\"\" != \"[array names faceMap $key]\"" "Invalid cell faceMap key: '$key'"
          set ret [list]
          set map $faceMap($key)
          foreach faceIndices $map { ;# faceIndices is a list of cell local indices
            set face [list]
            foreach ndx $faceIndices {
              lappend face [lindex $cell $ndx]
            }

            if {$minFirstOrder} {
              set minIdx 0
              set minVal [lindex $face $minIdx]

              for {set j 1} {$j < [llength $face]} {incr j} {
                if {[set val [lindex $face $j]] < $minVal} {
                  set minVal $val
                  set minIdx $j
                }
              }

              if {$minIdx > 0} {
                set minFirstFace [concat [lrange $face $minIdx end] \
                         [lrange $face 0 [expr {$minIdx - 1}]]]
              } else {
                set minFirstFace $face
              }
              lappend ret $minFirstFace
            } else {
              lappend ret $face
            }
          }
          return $ret
        }
    }


    #========================================================================
    #                           pwio variables
    #========================================================================

    # misc variables
    variable selEnts [list]
    variable caeDim  [pw::Application getCAESolverDimension]
    variable perimPtCountCache      ;# maps an ent to its num of perim points
        array set perimPtCountCache {}

    # coords db variables
    variable coordSingleEnt ""
    variable entToCoordTotalsOffset ; array set entToCoordTotalsOffset {} ;# maps an ent to its offset into coordTotals list
    variable coordTotals [list]     ;# list of {entOwnedPts totalOwnedPts ent} lists
    variable totalOwnedPts 0
    variable ndxCoordTotals 0       ;# cached index into coordTotals list
    variable offsetGlobToLocalGetCoord 0    ;# cached global to owned index offset: globNdx = ownedNdx + offsetGlobToLocalGetCoord
    variable prevCoordEntName "@null"    ;# cached name of previous coord entity enumerated
    variable offsetGlobToLocalGetCoordIndex 0   ;# cached global to owned index offset: globNdx = ownedNdx + offsetGlobToLocalGetCoordIndex

    # cells db variables
    variable cellSingleEnt ""
    variable totalCellCount 0
    variable cellTotals [list]       ;# list of {entCellCount totalEntCellCount ent} lists
    variable entToCellTotalsOffset   ;# maps an ent to its offset into cellTotals list
        array set entToCellTotalsOffset {}
    variable ndxCellTotals 0         ;# cached index into cellTotals list
    variable offsetGlobToLocalGetCell 0      ;# cached global to cell index offset: globCellNdx = entCellNdx + offsetGlobToLocalGetCell
    variable prevCellEntName "@null" ;# cached name of previous cell entity enumerated
    variable offsetGlobToLocalGetCellIndex 0 ;# cached global to cell index offset: globNdx = cellNdx + offsetGlobToLocalGetCellIndex



#public

    #========================================================================
    namespace export beginIO
    proc beginIO { ents } {
        variable selEnts
        reset
        set selEnts $ents
        build
        foreach ent $selEnts {
            utils::entLockInterior $ent
        }
    }

    #========================================================================
    namespace export endIO
    proc endIO { {clearAllLocks 0} } {
        variable selEnts
        foreach ent $selEnts {
            utils::entUnlockInterior $ent $clearAllLocks
        }
    }

    #========================================================================
    namespace export getCoordCount
    proc getCoordCount {} {
        variable totalOwnedPts
        return $totalOwnedPts
    }

    #========================================================================
    namespace export getCoord
    proc getCoord { enumNdx } { ;# 1-based index
        variable coordSingleEnt
        variable entToCoordTotalsOffset
        variable coordTotals
        variable totalOwnedPts
        variable ndxCoordTotals
        variable offsetGlobToLocalGetCoord

        utils::assert "$enumNdx >=1 && $enumNdx <= $totalOwnedPts" \
                "Invalid global index in pwio::getCoord($enumNdx)"

        set ret {} ;# GgGridCoord

        if { "" != $coordSingleEnt } {
            return [utils::makeCoordFromEntIndex $coordSingleEnt $enumNdx]
        }

        incr enumNdx -1 ;# convert to 0-based index

        # indexes will typically be looped over from 0 to NumPts, so we cache
        # the ndxCoordTotals index into the coordTotals list and offsetGlobToLocalGetCoord
        if { $enumNdx == [lindex $coordTotals $ndxCoordTotals 1] } {
            # moving to the next position in the coordTotals array
            set offsetGlobToLocalGetCoord [lindex $coordTotals $ndxCoordTotals 1]
            incr ndxCoordTotals
        } elseif { $enumNdx < $offsetGlobToLocalGetCoord || $enumNdx >= [lindex $coordTotals $ndxCoordTotals 1] } {
            # not in the cached range, so do a binary search to find the correct range
            set lo 0
            set hi [llength $coordTotals]
            while { $lo < $hi } {
                set mid [expr {($lo + $hi) / 2}]
                if { [lindex $coordTotals $mid 1] <= $enumNdx } {
                    set lo [incr mid];
                } else {
                    set hi $mid
                }
            }
            set ndxCoordTotals $lo
            if { $ndxCoordTotals == 0 } {
                set offsetGlobToLocalGetCoord 0
            } else {
                set offsetGlobToLocalGetCoord [lindex $coordTotals [incr lo -1] 1]
            }
        }

        incr enumNdx ;# convert back to 1-based index

        set ent [lindex $coordTotals $ndxCoordTotals 2]
        set ownedNdx [expr {$enumNdx - $offsetGlobToLocalGetCoord}]
        set entIjk [entOwnedIndexToIjk $ent $ownedNdx]
        return [utils::makeCoord $ent $entIjk]
    }

    #========================================================================
    namespace export getCoordIndex
    proc getCoordIndex { coord {mapCoordToOwner 1} } {
        variable coordSingleEnt
        variable entToCoordTotalsOffset
        variable coordTotals
        variable prevCoordEntName
        variable offsetGlobToLocalGetCoordIndex

        if { "" != $coordSingleEnt } {
            if { $coordSingleEnt == [coordGetEntity $coord] } {
                return [utils::entIjkToIndex $coordSingleEnt [coordGetIjk $coord]]
            } elseif { [coordMapToEntity $coord $coordSingleEnt coords] > 0 } {
                return [utils::entIjkToIndex $coordSingleEnt [coordGetIjk [lindex $coords 0]]]
            } else {
                utils::assert false "pwio::getCoordIndex: Could not map coord ($coord) to coordSingleEnt"
            }
        }

        if { $mapCoordToOwner } {
            set ownerCoord [mapToOwner $coord]
            set ent [coordGetEntity $ownerCoord]
            set ijk [coordGetIjk $ownerCoord]
        } else {
            set ent [coordGetEntity $coord]
            set ijk [coordGetIjk $coord]
        }

        if { $ent != $prevCoordEntName } {
            set match [array get entToCoordTotalsOffset $ent]
            if { 2 != [llength $match] } {
                utils::assert false "pwio::getCoordIndex: Entity not found ($ent) '$match'"
            }
            # match is {ent offset} list
            lassign $match -> totalsOffset
            set prevCoordEntName $ent
            set offsetGlobToLocalGetCoordIndex [lindex $coordTotals [expr {$totalsOffset - 1}] 1]
        }

        set ret [entIjkToOwnedIndex $ent $ijk]
        if { $offsetGlobToLocalGetCoordIndex > 0 } {
            # owned indices after the first range must be offset to the enum's
            # index space
            incr ret $offsetGlobToLocalGetCoordIndex
        }
        return $ret
    }

    #========================================================================
    namespace export getCellCount
    proc getCellCount {} {
        variable totalCellCount
        return $totalCellCount
    }

    #========================================================================
    namespace export getCell
    proc getCell { enumNdx {vcVarName ""} } { ;# 1-based index
        if { "" != $vcVarName } {
            upvar 1 $vcVarName vc
        }
        variable cellSingleEnt
        variable entToCellTotalsOffset
        variable cellTotals
        variable totalCellCount
        variable ndxCellTotals
        variable offsetGlobToLocalGetCell

        utils::assert "$enumNdx >=1 && $enumNdx <= $totalCellCount" \
                "Invalid global index in pwio::getCell($enumNdx)"

        set ret [list] ;# cell indices

        if { "" != $cellSingleEnt } {
            set vc [$cellSingleEnt getVolumeCondition]
            return [getEntityCell $cellSingleEnt $enumNdx]
        }

        incr enumNdx -1 ;# convert to 0-based index

        # indexes will typically be looped over from 0 to NumCells, so we cache
        # the prior index into the cellTotals list (ndxCellTotals) and the prior
        # index offset (offsetGlobToLocalGetCell)
        if { $enumNdx == [lindex $cellTotals $ndxCellTotals 1] } {
            # move to the next position in the cellTotals array
            set offsetGlobToLocalGetCell [lindex $cellTotals $ndxCellTotals 1]
            incr ndxCellTotals
            #puts "ndx=$enumNdx next block \$offsetGlobToLocalGetCell=$offsetGlobToLocalGetCell \$ndxCellTotals=$ndxCellTotals"
        } elseif { $enumNdx < $offsetGlobToLocalGetCell || $enumNdx >= [lindex $cellTotals $ndxCellTotals 1] } {
            # not in the cached range, so do a binary search to find the correct range
            set lo 0
            set hi [llength $cellTotals]
            while { $lo < $hi } {
                set mid [expr {($lo + $hi) / 2}]
                if { [lindex $cellTotals $mid 1] <= $enumNdx } {
                    set lo [incr mid];
                } else {
                    set hi $mid
                }
            }
            set ndxCellTotals $lo
            if { $ndxCellTotals == 0 } {
                set offsetGlobToLocalGetCell 0
            } else {
                set offsetGlobToLocalGetCell [lindex $cellTotals [incr lo -1] 1]
            }
            #puts "ndx=$enumNdx find block \$offsetGlobToLocalGetCell=$offsetGlobToLocalGetCell \$ndxCellTotals=$ndxCellTotals"
        } else {
            #puts "ndx=$enumNdx using block \$offsetGlobToLocalGetCell=$offsetGlobToLocalGetCell \$ndxCellTotals=$ndxCellTotals"
        }
        set vc [[set ent [lindex $cellTotals $ndxCellTotals 2]] getVolumeCondition]
        # +1 converts enumNdx back to 1-based index
        # return [getEntityCell ent entNdx]
        return [getEntityCell $ent [expr {$enumNdx + 1 - $offsetGlobToLocalGetCell}]]
    }

    #========================================================================
    namespace export getCellIndex
    proc getCellIndex { ent entNdx } { ;# 1-based index into ent's cell space
        variable cellSingleEnt
        variable entToCellTotalsOffset
        variable cellTotals
        variable prevCellEntName
        variable offsetGlobToLocalGetCellIndex

        if { "" != $cellSingleEnt } {
            if { $cellSingleEnt == $ent } {
                return $entNdx
            } else {
                utils::assert false "pwio::getCellIndex: Invalid entity ($ent)"
            }
        }

        if { $ent != $prevCellEntName } {
            set match [array get entToCellTotalsOffset $ent]
            if { 2 != [llength $match] } {
                utils::assert false "pwio::getCellIndex: Entity not found ($ent) '$match'"
            }
            # match is {ent offset} list
            lassign $match -> totalsOffset
            set prevCellEntName $ent
            set offsetGlobToLocalGetCellIndex [lindex $cellTotals [expr {$totalsOffset - 1}] 1]
        }

        if { $offsetGlobToLocalGetCellIndex > 0 } {
            # cell indices after the first range must be offset to the enum's
            # index space
            incr entNdx $offsetGlobToLocalGetCellIndex
        }
        return $entNdx
    }

    #========================================================================
    namespace export getCellEdges
    proc getCellEdges { enumNdx {cellVarName ""} {minFirstOrder 0} {revVarName ""} } {
        if { "" != $cellVarName } {
            upvar 1 $cellVarName cell
        }
        if { "" != $revVarName } {
            upvar 1 $revVarName rev
        }
        return [cell::getEdges [set cell [pwio::getCell $enumNdx]] $minFirstOrder rev]
    }

    #========================================================================
    namespace export getMinFirstCellEdges
    proc getMinFirstCellEdges { enumNdx {cellVarName ""} {revVarName ""} } {
        if { "" != $cellVarName } {
            upvar 1 $cellVarName cell
        }
        if { "" != $revVarName } {
            upvar 1 $revVarName rev
        }

        return [cell::getEdges [set cell [pwio::getCell $enumNdx]] 1 rev]
    }

    #========================================================================
    namespace export getFaceEdges
    proc getFaceEdges { face {cellVarName ""} {minFirstOrder 0} {revVarName ""} } {
        if { "" != $cellVarName } {
            upvar 1 $cellVarName cell
        }
        if { "" != $revVarName } {
            upvar 1 $revVarName rev
        }

        set cell $face
        return [cell::getFaceEdges $face $minFirstOrder rev]
    }

    #========================================================================
    namespace export getMinFirstFaceEdges
    proc getMinFirstFaceEdges { face {cellVarName ""} {revVarName ""} } {
        if { "" != $cellVarName } {
            upvar 1 $cellVarName cell
        }
        if { "" != $revVarName } {
            upvar 1 $revVarName rev
        }

        set cell $face
        return [cell::getFaceEdges $face 1 rev]
    }

    #========================================================================
    namespace export getCellFaces
    proc getCellFaces { enumNdx {cellVarName ""} {minFirstOrder 0} } {
        if { "" != $cellVarName } {
            upvar 1 $cellVarName cell
        }
        return [cell::getFaces [set cell [pwio::getCell $enumNdx]] $minFirstOrder]
    }

    #========================================================================
    namespace export getMinFirstCellFaces
    proc getMinFirstCellFaces { enumNdx {cellVarName ""} } {
        if { "" != $cellVarName } {
            upvar 1 $cellVarName cell
        }
        return [cell::getFaces [set cell [pwio::getCell $enumNdx]] 1]
    }

    #========================================================================
    namespace export getCellType
    proc getCellType {enumNdx} {

        array set cellMap {
            "2e3" "tri"
            "2e4" "quad"
            "3e4" "tet"
            "3e5" "pyramid"
            "3e6" "prism"
            "3e8" "hex"
        }

        set cell [pwio::getCell $enumNdx]
        set key "$pwio::caeDim\e[llength $cell]"

        return $cellMap($key)

    }

    #========================================================================
    namespace export getFaceType
    proc getFaceType {face} {

        if {[llength $face] == 3} {
            return "tri"
        } elseif {[llength $face] == 4} {
            return "quad"
        }

    }

    #========================================================================
    namespace export getEntityCell
    proc getEntityCell { ent ndx {localCellVarName ""} } {
        if { "" != $localCellVarName } {
            upvar 1 $localCellVarName localCell
        }
        #puts "--- $ndx"
        set ret [list]
        foreach locNdx [set localCell [$ent getCell $ndx]] {
            lappend ret [pwio::getCoordIndex [utils::makeCoordFromEntIndex $ent $locNdx]]
        }
        return $ret
    }

    #========================================================================
    namespace export getEntityCellEdges
    proc getEntityCellEdges { ent ndx {cellVarName ""} {minFirstOrder 0} {revVarName ""} } {
        if { "" != $cellVarName } {
            upvar 1 $cellVarName cell
        }
        if { "" != $revVarName } {
            upvar 1 $revVarName rev
        }
        return [cell::getEdges [set cell [getEntityCell $ent $ndx]] $minFirstOrder rev]
    }

    #========================================================================
    namespace export getMinFirstEntityCellEdges
    proc getMinFirstEntityCellEdges { ent ndx {cellVarName ""} {revVarName ""} } {
        if { "" != $cellVarName } {
            upvar 1 $cellVarName cell
        }
        if { "" != $revVarName } {
            upvar 1 $revVarName rev
        }
        return [cell::getEdges [set cell [getEntityCell $ent $ndx]] 1 rev]
    }

    #========================================================================
    namespace export dim
    proc getCaeDim {} {
        variable caeDim
        return $caeDim
    }

    #========================================================================
    namespace export getSelectType
    proc getSelectType {} {
        variable caeDim
        array set selInfo { \
            2 Domain \
            3 Block \
        }
        return $selInfo($caeDim)
    }

    #========================================================================
    namespace export reset
    proc reset {} {
        # misc variables
        variable selEnts
        variable caeDim
        variable perimPtCountCache

        set selEnts [list]
        set caeDim  [pw::Application getCAESolverDimension]
        array set perimPtCountCache {}

        # coords db variables
        variable coordSingleEnt
        variable entToCoordTotalsOffset
        variable coordTotals
        variable totalOwnedPts
        variable ndxCoordTotals
        variable offsetGlobToLocalGetCoord
        variable prevCoordEntName
        variable offsetGlobToLocalGetCoordIndex

        set coordSingleEnt ""
        array set entToCoordTotalsOffset {}
        set coordTotals [list]
        set totalOwnedPts 0
        set ndxCoordTotals 0
        set offsetGlobToLocalGetCoord 0
        set prevCoordEntName "null"
        variable offsetGlobToLocalGetCoordIndex 0

        # cells db variables
        variable cellSingleEnt
        variable totalCellCount
        variable cellTotals
        variable entToCellTotalsOffset
        variable ndxCellTotals
        variable offsetGlobToLocalGetCell
        variable prevCellEntName
        variable offsetGlobToLocalGetCellIndex

        set cellSingleEnt ""
        set totalCellCount 0
        set cellTotals [list]
        array set entToCellTotalsOffset {}
        set ndxCellTotals 0
        set offsetGlobToLocalGetCell 0
        set prevCellEntName "@null"
        set offsetGlobToLocalGetCellIndex 0
    }

    #========================================================================
    namespace export fixCoord
    proc fixCoord { coordVarName } {
        upvar 1 $coordVarName coord
        # force coord to {i j k ent} format
        switch -exact [llength $coord] {
        2 { set coord [linsert $coord 1 1 1] }
        3 { set coord [linsert $coord 2 1] }
        4 { set dummy 1 }
        default { utils::assert false "pwio::fixCoord: Bad coord value ($coord)" }
        }
        return $coord
    }

    #========================================================================
    namespace export coordMapLower
    proc coordMapLower { coord } {
        set ent [coordGetEntity $coord]
        pwio::fixCoord coord
        switch -exact [$ent getType] {
        pw::Node {
            return [list]
        }
        pw::Connector {
            set i [lindex $coord 0]
            if { $i == 1 } {
                return [list [list 1 1 1 [$ent getNode Begin]]]
            } elseif { $i == [$ent getPointCount] } {
                return [list [list 1 1 1 [$ent getNode End]]]
            }
            return [list]
        }
        }
        return [pw::Grid mapLower $coord]
    }

    #========================================================================
    namespace export mapToOwner
    proc mapToOwner { coord {trace 0} } {
        set conTol [pw::Grid getConnectorTolerance]
        set origXyz [pw::Grid getXYZ $coord]
        set lowerCoords [coordMapLower $coord]
        while { [llength $lowerCoords] > 0 } {
            if { $trace } {
                puts "\{$coord\} [entGetName [pw::Grid getEntity $coord]] ([pw::Grid getXYZ $coord])"
                foreach crd $lowerCoords {
                    puts "---> $crd [entGetName [pw::Grid getEntity $crd]] ([pw::Grid getXYZ $crd])"
                }
            }
            # If more than one, just grab first. Eventually, only one, owning
            # ent will be returned and next call to coordMapLower will return an
            # empty list ending the loop
            #set coord [lindex $lowerCoords 0]
            #set lowerCoords [coordMapLower $coord]

            # BUG WORKAROUND - sometimes an invalid lower con is returned!
            # Search returned lower entities making sure lower ent has same xyz
            set hadMismatch 0
            foreach coord $lowerCoords {
                if { [utils::xyzEqual $origXyz [pw::Grid getXYZ $coord] $conTol] } {
                    if { $hadMismatch } {
                        #puts "Using coord \{$coord\} after mismatch"
                        set hadMismatch 0
                    }
                    break
                }
                set hadMismatch 1
                #puts "mismatch! \{$coord\} [entGetName [pw::Grid getEntity $coord]] ([pw::Grid getXYZ $coord]) != $origXyz"
            }
            utils::assert !$hadMismatch "Could not find matching coord! \{$coord\}"
            set lowerCoords [coordMapLower $coord]
        }
        return $coord
    }

    #========================================================================
    namespace export coordGetEntity
    proc coordGetEntity { coord } {
        return [lindex $coord end]
    }

    #========================================================================
    namespace export coordGetIjk
    proc coordGetIjk { coord } {
        return [lrange [pwio::fixCoord coord] 0 2]
    }

    #========================================================================
    namespace eval Level {
        variable Block     3
        variable Domain    2
        variable Connector 1
        variable Node      0
    }

    #========================================================================
    namespace export entGetLevel
    proc entGetLevel { entOrBaseType } {
        # usage:
        #   entGetLevel Block              ;# returns 3
        #   entGetLevel ::pw::Connector_3  ;# returns 1
        array set rank {Block 3 Domain 2 Connector 1 Node 0}
        if { "" == [array get rank $entOrBaseType] } {
            # ent is NOT a base ent type. Assume it is an entity
            set entOrBaseType [utils::entBaseType $entOrBaseType]
        }
        return $rank($entOrBaseType)
    }

    #========================================================================
    namespace export coordGetLevel
    proc coordGetLevel { coord } {
        return [entGetLevel [coordGetEntity $coord]]
    }

    #========================================================================
    namespace export coordMapToEntity
    proc coordMapToEntity { fromCoord toEnt coordsVarName } {
        upvar 1 $coordsVarName coords
        set coords {}
        coordMapToLevel $fromCoord [entGetLevel $toEnt] locCoords
        foreach coord $locCoords {
            if { [coordGetEntity $coord] == $toEnt } {
                lappend coords [pwio::fixCoord coord]
            }
        }
        return [llength $coords]
    }

    #========================================================================
    namespace export coordMapToLevel
    proc coordMapToLevel { coord toLevel coordsVarName } {
        upvar 1 $coordsVarName coords
        set coords [list]
        array set uniqueCoords {} ;# tracks unique coords
        set coordLevel [coordGetLevel $coord]
        if { $toLevel == $coordLevel } {
            # Simple case of the coordinate already at the correct toLevel
            lappend coords [pwio::fixCoord coord]
        } else {
            set currCoords [list $coord] ;# prime the pump
            set nextCoords [list]
             if { $coordLevel > $toLevel } {
                # map coord to a lower level coord
                while { [llength $currCoords] > 0 } {
                    # For each higher toLevel coordinate, map down and store in nextCoords
                    set nextCoords [list]
                    foreach coord $currCoords {
                        set nextCoords [concat $nextCoords [coordMapLower $coord]]
                    }
                    # Clear out currCoords and append the appropriate
                    # coordinates from nextCoords to currCoords
                    set currCoords [list]
                    foreach coord $nextCoords {
                        set coordLevel [coordGetLevel $coord]
                        if { $coordLevel > $toLevel } {
                            # Still too high. Append coord to currCoords so while
                            # loop will process on next pass
                            lappend currCoords $coord
                        } elseif { $coordLevel == $toLevel } {
                            # found ent at target toLevel - keep it
                            pwio::fixCoord coord
                            # set uniqueCoords(1,1,1,pw::entname) $coord
                            set uniqueCoords([join $coord ,]) $coord
                        }
                    }
                }
            } else {
                # map coord to a higher level coord
                while { [llength $currCoords] > 0 } {
                    set nextCoords [list]
                    foreach coord $currCoords {
                        set nextCoords [concat $nextCoords [pw::Grid mapHigher $coord]]
                    }
                    # Clear out currCoords and append the appropriate
                    # coordinates from nextCoords to currCoords
                    set currCoords [list]
                    foreach coord $nextCoords {
                        set coordLevel [coordGetLevel $coord]
                        if { $coordLevel < $toLevel } {
                            # Still too low. Append coord to currCoords so while
                            # loop will process on next pass
                            lappend currCoords $coord
                        } elseif { $coordLevel == $toLevel } {
                            # found ent at target toLevel - keep it
                            pwio::fixCoord coord
                            # set uniqueCoords(1,1,1,pw::entname) $coord
                            set uniqueCoords([join $coord ,]) $coord
                        }
                    }
                }
            }
            # load unique coordinates into coords
            foreach {key coord} [array get uniqueCoords] {
               lappend coords $coord
            }
        }
        return [llength $coords]
    }


#--------------- PRIVATE -------------------

    #========================================================================
    proc build { } {
        return [expr {[buildCoords] && [buildCells]}]
    }

    #========================================================================
    proc buildCoords { } {
        variable selEnts
        variable coordSingleEnt
        variable entToCoordTotalsOffset
        variable coordTotals
        variable totalOwnedPts
        variable ndxCoordTotals
        variable offsetGlobToLocalGetCoord
        variable prevCoordEntName
        variable offsetGlobToLocalGetCoordIndex

        # load $selEnts and all their lower level entities into allEnts
        set ret [utils::getSupportEnts $selEnts allEnts true]
        if { $ret } {
            if { 1 == [llength $selEnts] } {
                set coordSingleEnt [lindex $selEnts 0]
                # check if the entity pt count is equal to the total number of owned
                # points.  if so, then we can use the single entity optimization
                set totalOwnedPts 0
                foreach ent $allEnts {
                    incr totalOwnedPts [utils::getOwnedPointCount $ent]
                }
                if { $totalOwnedPts != [$coordSingleEnt getPointCount] } {
                    set coordSingleEnt ""
                }
            }

            if { "" == $coordSingleEnt } {
                set allEnts [utils::sortEntsByType $allEnts]
                # Go through the entities and:
                #    Add them to the entToCoordTotalsOffset array
                #    Add the runningTotal+ent pair to the coordTotals list
                set totalOwnedPts 0
                foreach ent $allEnts {
                    set ownedPts [utils::getOwnedPointCount $ent]
                    if { $ownedPts > 0 } {
                        incr totalOwnedPts $ownedPts
                        set offset [llength $coordTotals]
                        set entToCoordTotalsOffset($ent) $offset
                        lappend coordTotals [list $ownedPts $totalOwnedPts $ent]
                    }
                }
            }

            # initialize the cache to the beginning of the coordTotals array
            # used by getCoord
            set ndxCoordTotals 0
            set offsetGlobToLocalGetCoord 0

            # initialize the entities cache to the beginning of the entToCoordTotalsOffset array
            # used by getIndex
            set prevCoordEntName "@null" ;# lastEnt_
            set offsetGlobToLocalGetCoordIndex 0
        } else {
            utils::assert false "getSupportEnts FAILED"
        }
        return $ret
    }

    #========================================================================
    proc buildCells { } {
        variable selEnts
        variable cellSingleEnt
        variable totalCellCount
        variable cellTotals
        variable entToCellTotalsOffset
        variable ndxCellTotals
        variable offsetGlobToLocalGetCell
        variable prevCellEntName
        variable offsetGlobToLocalGetCellIndex

        set ret 1
        if { 1 == [llength $selEnts] } {
            set cellSingleEnt [lindex $selEnts 0]
            set totalCellCount [$cellSingleEnt getCellCount]
        } else {
            # Go through the selected entities and:
            #    Add them to the entToCellTotalsOffset array
            #    Add the runningTotal+ent pair to the cellTotals list
            set totalCellCount 0
            foreach ent $selEnts {
                set cellCount [$ent getCellCount]
                if { $cellCount > 0 } {
                    incr totalCellCount $cellCount
                    set offset [llength $cellTotals]
                    set entToCellTotalsOffset($ent) $offset
                    lappend cellTotals [list $cellCount $totalCellCount $ent]
                }
            }

            # initialize the cache to the beginning of the cellTotals array
            # used by pwio::getCell
            set ndxCellTotals 0
            set offsetGlobToLocalGetCell 0

            # initialize the cells cache to the beginning of the cells array
            # used by pwio::getCellIndex
            set prevCellEntName "@null"
            set offsetGlobToLocalGetCellIndex 0
        }
        return $ret
    }

    #========================================================================
    proc getCoordEnumEntCount {} {
        variable coordTotals
        variable coordSingleEnt
        if { "" != $coordSingleEnt } {
            set ret 1
        } else {
            set ret [llength $coordTotals]
        }
        return $ret
    }

    #========================================================================
    proc getCoordEnumEnt { ndx } { ;# 0 to getCoordEnumEntCount-1
        variable coordTotals
        variable coordSingleEnt
        if { "" != $coordSingleEnt } {
            utils::assert "$ndx == 0 " "getCoordEnumEnt: Invalid single ent index ($ndx)"
            set ret $coordSingleEnt
        } else {
            utils::assert "$ndx >= 0 && $ndx < [getCoordEnumEntCount]" \
                    "getCoordEnumEnt: Invalid multi ent index ($ndx)"
            set ret [lindex $coordTotals $ndx 2]
        }
        return $ret
    }

    #========================================================================
    proc getCoordEnumOffset { ndx } { ;# 0 to getCoordEnumEntCount-1
        utils::assert "$ndx >= 0 && $ndx < [getCoordEnumEntCount]" \
                "getCoordEnumOffset: Invalid index ($ndx)"
        variable coordTotals
        variable coordSingleEnt
        if { 0 == $ndx } {
            set ret 0
        } elseif { "" != $coordSingleEnt } {
            # 0 is the only valid index for single ent
            utils::assert false "getCoordEnumOffset: Invalid single ent index ($ndx)"
        } else {
            # if here, ndx >= 1 and multi ent pwio
            # offset is running owned total of previous item in coordTotals
            set ret [lindex $coordTotals [incr ndx -1] 1]
        }
        return $ret
    }

    #========================================================================
    proc getCoordEnumRange { ndx } { ;# 0 to getCoordEnumEntCount-1
        variable coordTotals
        variable coordSingleEnt
        if { 0 == $ndx } {
            set offset 0
        } elseif { "" != $coordSingleEnt } {
            # 0 is the only valid index for single ent
            utils::assert false "getCoordEnumRange: Invalid single ent index ($ndx)"
        } else {
            utils::assert "$ndx > 0 && $ndx < [getCoordEnumEntCount]" \
                    "getCoordEnumRange: Invalid multi ent index ($ndx)"
            set offset [getCoordEnumOffset $ndx]
        }
        # if here, ndx is valid
        # set to local entity owned indices
        set startNdx 1
        set endNdx [getCoordEnumNumOwnedPts $ndx]
        # incr by offset to make global enum indices
        return [list [incr startNdx $offset] [incr endNdx $offset]]
    }

    #========================================================================
    proc getCoordEnumNumOwnedPts { ndx } { ;# 0 to getCoordEnumEntCount-1
        utils::assert "$ndx >= 0 && $ndx < [getCoordEnumEntCount]" \
                "getCoordEnumNumOwnedPts: Invalid index ($ndx)"
        variable coordTotals
        variable coordSingleEnt
        if { "" != $coordSingleEnt } {
            set ret [$coordSingleEnt getPointCount]
        } else {
            set ret [lindex $coordTotals $ndx 0]
        }
        return $ret
    }



    #========================================================================
    proc getCellEnumEntCount {} {
        variable cellTotals
        variable cellSingleEnt
        if { "" != $cellSingleEnt } {
            set ret 1
        } else {
            set ret [llength $cellTotals]
        }
        return $ret
    }

    #========================================================================
    proc getCellEnumEnt { ndx } { ;# 0 to getCellEnumEntCount-1
        variable cellTotals
        variable cellSingleEnt
        if { "" != $cellSingleEnt } {
            utils::assert "$ndx == 0 " "getCellEnumEnt: Invalid single ent index ($ndx)"
            set ret $cellSingleEnt
        } else {
            utils::assert "$ndx >= 0 && $ndx < [getCellEnumEntCount]" \
                    "getCellEnumEnt: Invalid multi ent index ($ndx)"
            set ret [lindex $cellTotals $ndx 2]
        }
        return $ret
    }

    #========================================================================
    proc getCellEnumOffset { ndx } { ;# 0 to getCellEnumEntCount-1
        utils::assert "$ndx >= 0 && $ndx < [getCellEnumEntCount]" \
                "getCellEnumOffset: Invalid index ($ndx)"
        variable cellTotals
        variable cellSingleEnt
        if { 0 == $ndx } {
            set ret 0
        } elseif { "" != $cellSingleEnt } {
            # 0 is the only valid index for single ent
            utils::assert false "getCellEnumOffset: Invalid single ent index ($ndx)"
        } else {
            # if here, ndx >= 1 and multi ent pwio
            # offset is running owned total of previous item in cellTotals
            set ret [lindex $cellTotals [incr ndx -1] 1]
        }
        return $ret
    }

    #========================================================================
    proc getCellEnumRange { ndx } { ;# 0 to getCellEnumEntCount-1
        variable cellTotals
        variable cellSingleEnt
        if { 0 == $ndx } {
            set offset 0
        } elseif { "" != $cellSingleEnt } {
            # 0 is the only valid index for single ent
            utils::assert false "getCellEnumRange: Invalid single ent index ($ndx)"
        } else {
            utils::assert "$ndx > 0 && $ndx < [getCellEnumEntCount]" \
                    "getCellEnumRange: Invalid multi ent index ($ndx)"
            set offset [getCellEnumOffset $ndx]
        }
        # if here, ndx is valid
        # set to local entity owned indices
        set startNdx 1
        set endNdx [getCellEnumNumOwnedPts $ndx]
        # incr by offset to make global enum indices
        return [list [incr startNdx $offset] [incr endNdx $offset]]
    }

    #========================================================================
    proc getCellEnumNumOwnedPts { ndx } { ;# 0 to getCellEnumEntCount-1
        utils::assert "$ndx >= 0 && $ndx < [getCellEnumEntCount]" \
                "getCellEnumNumOwnedPts: Invalid index ($ndx)"
        variable cellTotals
        variable cellSingleEnt
        if { "" != $cellSingleEnt } {
            set ret [$cellSingleEnt getPointCount]
        } else {
            set ret [lindex $cellTotals $ndx 0]
        }
        return $ret
    }

    #========================================================================
    proc entGetCoordEnumNumOwnedPts { ent } {
        variable coordSingleEnt
        variable entToCoordTotalsOffset
        if { $ent == $coordSingleEnt } {
            set ret [$coordSingleEnt getPointCount]
        } elseif { "" != $coordSingleEnt } {
            utils::assert false "entGetCoordEnumNumOwnedPts: Single entity mismatch ($coordSingleEnt != $ent)"
        } elseif { "" == [array get entToCoordTotalsOffset $ent] } {
            utils::assert false "entGetCoordEnumNumOwnedPts: Invalid entity ($ent)"
        } else {
            set ret [getCoordEnumNumOwnedPts $entToCoordTotalsOffset($ent)]
        }
        return $ret
    }

    #========================================================================
    proc entGetCachedNumPerimeterPts { ent } {
        variable perimPtCountCache
        if { "" == [array get perimPtCountCache $ent] } {
            set perimPtCountCache($ent) [utils::getPerimeterPointCount $ent]
        }
        return $perimPtCountCache($ent)
    }

    #========================================================================
    proc entIjkToOwnedIndex { ent ijk } { ;# ijk must be 1-based
        variable coordSingleEnt
        if { "" != $coordSingleEnt } {
            utils::assert "[string equal $coordSingleEnt $ent]" "entOwnedIndexToIjk: Single entity mismatch ($coordSingleEnt != $ent)"
            return [utils::entIjkToIndex $ent $ownedNdx]
        }

        set ret 0
        lassign $ijk i j k
        switch -exact [$ent getType] {
        pw::Node {
            utils::assert "1 == $i" "entIjkToOwnedIndex: Invalid Node ijk \{$ijk\} ($ent)"
            set ret $i
        }
        pw::Connector {
            utils::assert "[$ent isInteriorIndex $i]" \
                    "entIjkToOwnedIndex: Invalid Connector ijk \{$ijk\} ($ent)"
            set ret [incr i -1]
        }
        pw::DomainUnstructured {
            utils::assert "[$ent isInteriorIndex $i]" \
                    "entIjkToOwnedIndex: Invalid DomainUnstructured ijk \{$ijk\} ($ent)"
            set ret [incr i -[entGetCachedNumPerimeterPts $ent]]
        }
        pw::DomainStructured {
            utils::assert "[$ent isInteriorIndex [list $i $j]]" \
                    "entIjkToOwnedIndex: Invalid DomainStructured ijk \{$ijk\} ($ent)"
            set entDims [$ent getDimensions]
            lassign $entDims idim jdim
            # convert ent ij dims to owned ij dims
            set ownedDims [list [incr idim -2] [incr jdim -2]]
            # convert ij to owned ij
            incr i -1
            incr j -1
            set ret [utils::ijkToIndexStructured [list $i $j] $ownedDims]
        }
        pw::BlockUnstructured {
            utils::assert "[$ent isInteriorIndex $i]" \
                    "entIjkToOwnedIndex: Invalid BlockUnstructured ijk \{$ijk\} ($ent)"
            set ret [incr i -[entGetCachedNumPerimeterPts $ent]]
        }
        pw::BlockStructured {
            utils::assert "[$ent isInteriorIndex $ijk]" \
                    "entIjkToOwnedIndex: Invalid BlockStructured ijk \{$ijk\} ($ent)"
            set entDims [$ent getDimensions]
            lassign $entDims idim jdim kdim
            # convert ent ijk dims to owned ijk dims
            set ownedDims [list [incr idim -2] [incr jdim -2] [incr kdim -2]]
            # convert ij to owned ij
            incr i -1
            incr j -1
            incr k -1
            set ret [utils::ijkToIndexStructured [list $i $j $k] $ownedDims]
        }
        pw::BlockExtruded {
            utils::assert "[$ent isInteriorIndex $ijk]" \
                    "entIjkToOwnedIndex: Invalid BlockExtruded ijk \{$ijk\} ($ent)"
            set numBasePerimPts [entGetCachedNumPerimeterPts [$ent getFace JMinimum]]
            set entDims [$ent getDimensions]
            lassign $entDims idim jdim kdim
            # convert ent ijk dims to owned ijk dims
            set ownedDims [list [incr idim -$numBasePerimPts] $jdim [incr kdim -2]]
            # convert ijk to owned ijk
            incr i -$numBasePerimPts
            # j stays 1
            incr k -1
            set ret [utils::ijkToIndexStructured [list $i $j $k] $ownedDims]
        }
        default {
            utils::assert false "entOwnedIndexToIjk: bad etype '[$ent getType]'"
        }
        }
        return $ret
    }

    #========================================================================
    proc entOwnedIndexToIjk { ent ownedNdx } { ;# ownedNdx must be 1-based
        variable coordSingleEnt
        if { "" != $coordSingleEnt } {
            utils::assert "[string equal $coordSingleEnt $ent]" \
                "entOwnedIndexToIjk: Single entity mismatch ($coordSingleEnt != $ent)"
            return [entIndexToIjk $ent $ownedNdx]
        }

        utils::assert "$ownedNdx >= 1 && $ownedNdx <= [pwio::entGetCoordEnumNumOwnedPts $ent]" \
                    "entOwnedIndexToIjk: Bad Owned Index $ownedNdx for $ent"

        set ret 0
        switch -exact [$ent getType] {
        pw::Node {
            set ret [list $ownedNdx 1 1]
        }
        pw::Connector {
            set ret [list [incr ownedNdx 1] 1 1]
        }
        pw::DomainUnstructured {
            set ret [list [expr {$ownedNdx + [entGetCachedNumPerimeterPts $ent]}] 1 1]
        }
        pw::DomainStructured {
            set entDims [$ent getDimensions]
            lassign $entDims idim jdim
            # the -2 will reduce ent ij dims to owned ij dims
            set ownedDims [list [incr idim -2] [incr jdim -2]]
            # convert ownedNdx to owned{i j} and assign to i j vars
            lassign [utils::indexToIjkStructured $ownedNdx $ownedDims] i j
            # ent{i j} = owned{i j}+{1 1}
            set ret [list [incr i] [incr j] 1]
        }
        pw::BlockUnstructured {
            set ret [list [expr {$ownedNdx + [entGetCachedNumPerimeterPts $ent]}] 1 1]
        }
        pw::BlockStructured {
            set entDims [$ent getDimensions]
            lassign $entDims idim jdim kdim
            # the -2 will reduce ent ijk dims to owned ijk dims
            set ownedDims [list [incr idim -2] [incr jdim -2] [incr kdim -2]]
            # convert ownedNdx to owned{i j k} and assign to i j k vars
            lassign [utils::indexToIjkStructured $ownedNdx $ownedDims] i j k
            # ent{i j k} = owned{i j k}+{1 1 1}
            set ret [list [incr i] [incr j] [incr k]]
        }
        pw::BlockExtruded {
            set numBasePerimPts [entGetCachedNumPerimeterPts [$ent getFace JMinimum]]
            set entDims [$ent getDimensions]
            lassign $entDims idim jdim kdim
            # reduce ent ijk dims to owned ijk dims
            set ownedDims [list [incr idim -$numBasePerimPts] $jdim [incr kdim -2]]
            # convert ownedNdx to owned{i j k}
            lassign [utils::indexToIjkStructured $ownedNdx $ownedDims] i j k
            # ent{i j k} = owned{i j k}+{numBasePerimPts 0 1}
            set ret [list [incr i $numBasePerimPts] $j [incr k]]
        }
        default {
            utils::assert false "entOwnedIndexToIjk: bad etype '[$ent getType]'"
        }
        }
        return $ret
    }


#--------------- DEBUG -------------------

    #========================================================================
    proc printSummary {} {
        variable selEnts
        variable caeDim
        variable entToCoordTotalsOffset
        variable coordTotals
        variable coordSingleEnt
        variable entToCellTotalsOffset
        variable cellTotals
        variable cellSingleEnt

        puts "pwio Summary:"
        puts "totalPts   = [getCoordCount]"
        puts "totalCells = [getCellCount]"
        puts ""
        utils::printEntInfo "Selected Entities:" $selEnts
        puts ""
        puts "Grid Points:"
        if { "" != $coordSingleEnt } {
            puts "\$coordSingleEnt=[utils::entGetName $coordSingleEnt] ($coordSingleEnt)"
            #getSupportEnts $coordSingleEnt supEnts
            #printEntInfo "Single Ent: Lower Level Entities" [utils::sortEntsByType $supEnts] $caeDim $selEnts
        } else {
            puts "| ndx | ownPts | runTot | Range           | EntDims    | Name (entity)                            |"
            puts "|-----|--------|--------|-----------------|------------|------------------------------------------|"
            foreach tot $coordTotals {
                lassign $tot entTot runTot ent
                set totalsOffset $entToCoordTotalsOffset($ent)
                set range [getCoordEnumRange $totalsOffset]
                set entDims [utils::entGetDimensions $ent]
                set name "[utils::entGetName $ent] ($ent)"
                puts [format "| %3d | %6d | %6d | %15.15s | %10.10s | %-40.40s |" \
                      $totalsOffset $entTot $runTot $range $entDims $name]
            }
        }
        puts ""
        puts "Grid Cells:"
        if { "" != $cellSingleEnt } {
            puts "\$cellSingleEnt=[utils::entGetName $cellSingleEnt] ($cellSingleEnt)"
        } else {
            puts "| ndx |  Cells | runTot | Range           | EntDims    | Name (entity)                            |"
            puts "|-----|--------|--------|-----------------|------------|------------------------------------------|"
            foreach tot $cellTotals {
                lassign $tot entTot runTot ent
                set totalsOffset $entToCellTotalsOffset($ent)
                set range [getCellEnumRange $totalsOffset]
                set entDims [utils::entGetDimensions $ent]
                set name "[utils::entGetName $ent] ($ent)"
                puts [format "| %3d | %6d | %6d | %15.15s | %10.10s | %-40.40s |" \
                      $totalsOffset $entTot $runTot $range $entDims $name]
            }
        }
    }
}
}

# END SCRIPT

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
