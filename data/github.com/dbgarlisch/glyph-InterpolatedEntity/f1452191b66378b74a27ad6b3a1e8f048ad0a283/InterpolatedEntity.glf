#
# Copyright 2016 (c) Pointwise, Inc.
# All rights reserved.
#
# This sample script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#

if { [namespace exists ::pw::InterpolatedEntity] } return

package require PWI_Glyph

source [file join [file dirname [info script]] .. tcl-Utils ProcAccess.tcl]
source [file join [file dirname [info script]] .. tcl-Utils Debug.tcl]


#####################################################################
#####################################################################

namespace eval ::pw::InterpolatedEntity {
  variable cnt_ 0

  public proc new { ent mult } {
    set ret "::pw::InterpolatedEntity::_[incr ::pw::InterpolatedEntity::cnt_]"
    namespace eval $ret $::pw::InterpolatedEntity::InterpolatedEntityProto_
    switch [$ent getType] {
    pw::Connector {
      namespace eval $ret $::pw::InterpolatedEntity::InterpolatedConProto_
      set dimty 1
    }
    pw::DomainStructured {
      namespace eval $ret $::pw::InterpolatedEntity::InterpolatedDomProto_
      set dimty 2
    }
    pw::BlockStructured {
      namespace eval $ret $::pw::InterpolatedEntity::InterpolatedBlkProto_
      set dimty 3
    }
    default {
      error "Unsuported entity type [$ent getType]"
    }}
    ${ret}::ctor $ret $ent $mult $dimty
    namespace eval $ret {
      namespace ensemble create
    }
  }

  #------------------------------------------
  variable InterpolatedEntityProto_ {
    variable self_      {}
    variable ent_       {}   ;# the entity being refined
    variable mult_      1.0  ;# refinement multiplier
    variable dimty_     0    ;# ent_ dimensionality
    variable cache_     {}   ;# dict {i j k} --> {x y z}

    private proc ctor { self ent mult dimty } {
      variable self_ $self
      variable ent_ $ent
      variable mult_ $mult
      variable dimty_ $dimty
    }

    public proc getEntity {} {
      variable ent_
      return $ent_
    }

    public proc getMult {} {
      variable mult_
      return $mult_
    }

    public proc getDimensionality {} {
      variable dimty_
      return $dimty_
    }

    public proc getDimensions {} {
      variable ent_
      variable mult_
      set ret {}
      ::foreach numPts [$ent_ getDimensions] {
        lappend ret [origToIntpNdx $numPts]
      }
      return $ret
    }

    public proc getPointCount {} {
      set ret 1
      ::foreach numPts [getDimensions] {
        set ret [expr {$ret * $numPts}]
      }
      return $ret
    }

    public proc getXYZ { ndx {isDbVar {::pw::InterpolatedEntity::__isDb}} \
        {uvDbVar {::pw::InterpolatedEntity::__uvDb}} } {
      upvar $uvDbVar uvDb
      upvar $isDbVar isDb
      set uvDb [getPoint $ndx isDb xyz]
      return $xyz
    }

    public proc isInteriorIndex { ndx } {
      set dims [getDimensions]
      ::foreach n $ndx dim $dims {
        if { 1 == $n || $dim == $n } {
          return 0
        }
      }
      return 1
    }

    public proc delete {} {
      variable self_
      namespace delete $self_
    }

    private proc getBay { ndx sVar } {
      # returns pair of original ent indices that bracket ndx
      # ndx is 1-based
      upvar $sVar s
      variable mult_
      set orig2 [set orig1 [expr {($ndx - 1) / $mult_ + 1}]]
      set ndx1 [origToIntpNdx $orig1]
      # TODO: use precomputed lookup table for s as:
      #       set s [lindex $sTable [expr {$ndx - $ndx1}]]
      #       Is this really faster??
      set s [expr {double($ndx - $ndx1) / $mult_}]
      if { $ndx != $ndx1 } {
        # ndx lies between original indices orig1 and orig2
        incr orig2
      }
      #else ndx and orig1 are coincident - leave orig2 == orig1
      return [list $orig1 $orig2] ;# return 1-based indices
    }

    private proc getCache { ndx xyzVar uvDbVar isDbVar } {
      variable cache_
      if { [dict exists $cache_ $ndx] } {
        upvar $xyzVar xyz
        upvar $uvDbVar uvDb
        upvar $isDbVar isDb
        lassign [dict get $cache_ $ndx] xyz isDb uvDb
        return 1
      }
      return 0
    }

    private proc addCache { ndx xyz {isDb 0} {uvDb {}} } {
      variable cache_
      dict set cache_ $ndx [list $xyz $isDb $uvDb]
      return $xyz
    }

    private proc doProject { xyzVar uvDb1 uvDb2 } {
      upvar $xyzVar xyz
      set db1 [pw::Database getEntity $uvDb1]
      set db2 [pw::Database getEntity $uvDb2]
      if { "$db1" == "$db2" } {
        set ret [$db1 closestPoint -distance dist $xyz]
      } else {
        set ret [$db1 closestPoint -distance dist $xyz]
        set ret2 [$db2 closestPoint -distance dist2 $xyz]
        if { $dist2 < $dist1 } {
          set ret $ret2
          set from $from2
        }
      }
      set xyz [pw::Database getXYZ $ret]
      return $ret
    }

    private proc origToIntpNdx { origNdx } {
      variable mult_
      # maps orig grid coord to its eqiv interp grid coord
      return [expr {($origNdx - 1) * $mult_ + 1}]
    }

    private proc interpProject { ndx1 ndx2 s uvDbVar xyzVar } {
      upvar $uvDbVar uvDb
      upvar $xyzVar xyz
      variable ent_
      set uvDb1 [$ent_ getPoint -constrained isDb1 $ndx1]
      set uvDb2 [$ent_ getPoint -constrained isDb2 $ndx2]
      return [set isDb \
        [interpProjectUV [$ent_ getXYZ -grid $ndx1] $isDb1 $uvDb1 \
          [$ent_ getXYZ -grid $ndx2] $isDb2 $uvDb2 $s uvDb xyz]]
    }

    private proc interpProjectUV { xyz1 isDb1 uvDb1 xyz2 isDb2 uvDb2 s uvDbVar \
        xyzVar } {
      upvar $uvDbVar uvDb
      upvar $xyzVar xyz
      variable ent_
      set xyz [pwu::Vector3 affine $s $xyz1 $xyz2]
      if { [set isDb [expr {$isDb1 && $isDb2}]] } {
        set uvDb [doProject xyz $uvDb1 $uvDb2]
      } else {
        set uvDb {}
      }
      return $isDb
    }

    private proc interpProjectUVI { ndx bayI1 bayJ1 bayK1 bayI2 bayJ2 bayK2 \
        sJK sI uvDbVar xyzVar } {
      # interpolates a point along an I segment on a J or K boundary face
      upvar $uvDbVar uvDb
      upvar $xyzVar xyz
      lassign $ndx i j k
      # convert I1 in orig grid to I1 in interp grid
      set intp1 [origToIntpNdx $bayI1]
      set ndx1 [list $intp1 $j $k]
      if { ![getCache $ndx1 xyz1 uvDb1 isDb1] } {
        set isDb1 [interpProject [list $bayI1 $bayJ1 $bayK1] \
                                 [list $bayI1 $bayJ2 $bayK2] $sJK uvDb1 xyz1]
        addCache $ndx1 $xyz1 $isDb1 $uvDb1
      }
      # convert I2 in orig grid to I2 in interp grid
      set intp2 [origToIntpNdx $bayI2]
      set ndx2 [list $intp2 $j $k]
      if { ![getCache $ndx2 xyz2 uvDb2 isDb2] } {
        set isDb2 [interpProject [list $bayI2 $bayJ1 $bayK1] \
                                 [list $bayI2 $bayJ2 $bayK2] $sJK uvDb2 xyz2]
        addCache $ndx2 $xyz2 $isDb2 $uvDb2
      }
      set isDb [interpProjectUV $xyz1 $isDb1 $uvDb1 \
                                $xyz2 $isDb2 $uvDb2 $sI uvDb xyz]
      addCache $ndx $xyz $isDb $uvDb
        return $isDb
  }

    private proc interpProjectUVJ { ndx bayI1 bayJ1 bayK1 bayI2 bayJ2 bayK2 \
        sIK sJ uvDbVar xyzVar } {
      # interpolates a point along a J segment on an I or K boundary face
      upvar $uvDbVar uvDb
      upvar $xyzVar xyz
      lassign $ndx i j k
      # convert J1 in orig grid to J1 in interp grid
      set intp1 [origToIntpNdx $bayJ1]
      set ndx1 [list $i $intp1 $k]
      if { ![getCache $ndx1 xyz1 uvDb1 isDb1] } {
        set isDb1 [interpProject [list $bayI1 $bayJ1 $bayK1] \
                                 [list $bayI2 $bayJ1 $bayK2] $sIK uvDb1 xyz1]
        addCache $ndx1 $xyz1 $isDb1 $uvDb1
      }
      # convert J2 in orig grid to J2 in interp grid
      set intp2 [origToIntpNdx $bayJ2]
      set ndx2 [list $i $intp2 $k]
      if { ![getCache $ndx2 xyz2 uvDb2 isDb2] } {
        set isDb2 [interpProject [list $bayI1 $bayJ2 $bayK1] \
                                 [list $bayI2 $bayJ2 $bayK2] $sIK uvDb2 xyz2]
        addCache $ndx2 $xyz2 $isDb2 $uvDb2
      }
      set isDb [interpProjectUV $xyz1 $isDb1 $uvDb1 \
                                $xyz2 $isDb2 $uvDb2 $sJ uvDb xyz]
      addCache $ndx $xyz $isDb $uvDb
      return $isDb
    }

    #-------------------------------------------------
    #------------------ DEBUG STUFF ------------------
    #-------------------------------------------------

    public proc dump {} {
      variable self_
      variable ent_
      set dashes [string repeat - 50]
      set fmt "| %-20.20s | %-30.30s |"
      puts {}
      puts "Processing: [$ent_ getName]"
      puts [format $fmt $dashes $dashes]
      puts [format $fmt "\$self_" $self_]
      set cmds {getEntity getMult getDimensions getXyzCaching}
      ::foreach cmd $cmds {
        puts [format $fmt $cmd [$self_ $cmd]]
      }
      puts [format $fmt $dashes $dashes]
    }

    public proc dumpCache {} {
      variable cache_
      dict for {ndx data} $cache_ {
        puts [format "%-12.12s ==> \{%s\}" $ndx $data]
      }
    }

    private proc xyzStr { xyz } {
      lassign $xyz x y z
      return [format "\{%6.3f %6.3f %6.3f\}" $x $y $z]
    }

    private proc uvDbStr { uvDb } {
      if { 0 == [llength $uvDb]} {
        return "\{\}"
      }
      lassign $uvDb u v db
      if { [string is double -strict $db] } {
        return "\{\}"
      }
      return [format "\{%6.3f %6.3f %s\}" $u $v [$db getName]]
    }

    private proc dblStr { s } {
      return [format "%6.3f" $s]
    }
  }

  #------------------------------------------
  variable InterpolatedConProto_ {

    public proc foreach { ndxVar xyzVar isDbVar uvDbVar body } {
      upvar $ndxVar ndx
      upvar $xyzVar xyz
      upvar $isDbVar isDb
      upvar $uvDbVar uvDb
      variable self_
      lassign [$self_ getDimensions] iDim
      for {set ndx 1} {$ndx <= $iDim} {incr ndx} {
        set uvDb [getPoint $ndx isDb xyz]
        if { [catch {uplevel 1 $body} ret opts] } {
          # 1 (TCL_ERROR)
          # 2 (TCL_RETURN)
          # 3 (TCL_BREAK)
          # 4 (TCL_CONTINUE)
          switch -- [dict get $opts {-code}] {
          1 { return {*}$opts }
          2 { return }
          3 { return }
          4 {}
          default { return {*}$opts }
          }
        }
      }
    }

    public proc getPoint { ndx isDbVar xyzVar } {
      upvar $isDbVar isDb
      upvar $xyzVar xyz
      lassign $ndx ndx ;# ignore extra, trailing indices
      if { [getCache $ndx xyz uvDb isDb] } {
        return [expr {$isDb ? $uvDb : $xyz}]
      }
      # point for $ndx NOT cached - need to compute
      variable ent_
      lassign [getBay $ndx s] bayI1 bayI2
      if { $bayI1 == $bayI2 } {
        # ndx is coincident with an original grid point. Get xyz directly!
        set uvDb [$ent_ getPoint -constrained isDb $bayI1]
        if { $isDb } {
          set xyz [addCache $ndx [pw::Database getXYZ $uvDb] 1 $uvDb]
        } else {
          set xyz [addCache $ndx $uvDb]
        }
      } else {
        set isDb [interpProject $bayI1 $bayI2 $s uvDb xyz]
        addCache $ndx $xyz $isDb $uvDb
      }
      return [expr {$isDb ? $uvDb : $xyz}]
    }
  }

  #------------------------------------------
  variable InterpolatedDomProto_ {

    public proc foreach { ndxVar xyzVar isDbVar uvDbVar body } {
      upvar $ndxVar ndx
      upvar $xyzVar xyz
      upvar $isDbVar isDb
      upvar $uvDbVar uvDb
      variable self_
      lassign [$self_ getDimensions] iDim jDim
      for {set ii 1} {$ii <= $iDim} {incr ii} {
        for {set jj 1} {$jj <= $jDim} {incr jj} {
          set ndx [list $ii $jj]
          set uvDb [getPoint $ndx isDb xyz]
          if { [catch {uplevel 1 $body} ret opts] } {
            # 1 (TCL_ERROR)
            # 2 (TCL_RETURN)
            # 3 (TCL_BREAK)
            # 4 (TCL_CONTINUE)
            switch -- [dict get $opts {-code}] {
            1 { return {*}$opts }
            2 { return }
            3 { return }
            4 {}
            default { return {*}$opts }
            }
          }
        }
      }
    }

    public proc getPoint { ndx isDbVar xyzVar } {
      upvar $isDbVar isDb
      upvar $xyzVar xyz
      set ndx [lrange $ndx 0 1] ;# ignore extra, trailing indices
      if { [getCache $ndx xyz uvDb isDb] } {
        return [expr {$isDb ? $uvDb : $xyz}]
      }
      # point for $ndx NOT cached - need to compute
      variable ent_
      lassign $ndx i j
      lassign [getBay $i sI] bayI1 bayI2
      lassign [getBay $j sJ] bayJ1 bayJ2
      if { $bayI1 == $bayI2 && $bayJ1 == $bayJ2 } {
        # ndx is coincident with an original grid point. Get xyz directly!
        set uvDb [$ent_ getPoint -constrained isDb [list $bayI1 $bayJ1]]
        if { $isDb } {
          set xyz [addCache $ndx [pw::Database getXYZ $uvDb] 1 $uvDb]
        } else {
          set xyz [addCache $ndx $uvDb]
        }
      } elseif { $bayI1 == $bayI2 } {
        set isDb [interpProject [list $bayI1 $bayJ1] \
                                [list $bayI1 $bayJ2] $sJ uvDb xyz]
        addCache $ndx $xyz $isDb $uvDb
      } elseif { $bayJ1 == $bayJ2 } {
        set isDb [interpProject [list $bayI1 $bayJ1] \
                                [list $bayI2 $bayJ1] $sI uvDb xyz]
        addCache $ndx $xyz $isDb $uvDb
      } else {
        # convert J1 in orig grid to J1 in interp grid
        set intpJ1 [origToIntpNdx $bayJ1]
        if { ![getCache [set ndxJ1 [list $i $intpJ1]] xyzJ1 uvDbJ1 isDbJ1] } {
          set isDbJ1 [interpProject [list $bayI1 $bayJ1] \
                                    [list $bayI2 $bayJ1] $sI uvDbJ1 xyzJ1]
          addCache $ndxJ1 $xyzJ1 $isDbJ1 $uvDbJ1
        }
        # convert J2 in orig grid to J2 in interp grid
        set intpJ2 [origToIntpNdx $bayJ2]
        if { ![getCache [set ndxJ2 [list $i $intpJ2]] xyzJ2 uvDbJ2 isDbJ2] } {
          set isDbJ2 [interpProject [list $bayI1 $bayJ2] \
                                    [list $bayI2 $bayJ2] $sI uvDbJ2 xyzJ2]
          addCache $ndxJ2 $xyzJ2 $isDbJ2 $uvDbJ2
        }
        set isDb [interpProjectUV $xyzJ1 $isDbJ1 $uvDbJ1 \
                                  $xyzJ2 $isDbJ2 $uvDbJ2 $sJ uvDb xyz]
        addCache $ndx $xyz $isDb $uvDb
      }
      return [expr {$isDb ? $uvDb : $xyz}]
    }
  }

  #------------------------------------------
  variable InterpolatedBlkProto_ {

    public proc foreach { ndxVar xyzVar isDbVar uvDbVar body } {
      upvar $ndxVar ndx
      upvar $xyzVar xyz
      upvar $isDbVar isDb
      upvar $uvDbVar uvDb
      variable self_
      lassign [$self_ getDimensions] iDim jDim kDim
      for {set ii 1} {$ii <= $iDim} {incr ii} {
        for {set jj 1} {$jj <= $jDim} {incr jj} {
          for {set kk 1} {$kk <= $kDim} {incr kk} {
            set ndx [list $ii $jj $kk]
            set uvDb [$self_ getPoint $ndx isDb xyz]
            if { [catch {uplevel 1 $body} ret opts] } {
              # 1 (TCL_ERROR)
              # 2 (TCL_RETURN)
              # 3 (TCL_BREAK)
              # 4 (TCL_CONTINUE)
              switch -- [dict get $opts {-code}] {
              1 { return {*}$opts }
              2 { return }
              3 { return }
              4 {}
              default { return {*}$opts }
              }
            }
          }
        }
      }
    }

    public proc getPoint { ndx isDbVar xyzVar } {
      upvar $isDbVar isDb
      upvar $xyzVar xyz
      set ndx [lrange $ndx 0 2] ;# ignore extra, trailing indices
      if { [getCache $ndx xyz uvDb isDb] } {
        return $xyz
      }
      # point for $ndx NOT cached - need to compute
      variable ent_
      lassign $ndx i j k
      lassign [getBay $i sI] bayI1 bayI2
      lassign [getBay $j sJ] bayJ1 bayJ2
      lassign [getBay $k sK] bayK1 bayK2
      set eqI [expr {$bayI1 == $bayI2}]
      set eqJ [expr {$bayJ1 == $bayJ2}]
      set eqK [expr {$bayK1 == $bayK2}]
      if { $eqI && $eqJ && $eqK } {
        # ndx is coincident with an original grid point. Get xyz directly!
        set uvDb [$ent_ getPoint -constrained isDb [list $bayI1 $bayJ1 $bayK1]]
        if { $isDb } {
          set xyz [addCache $ndx [pw::Database getXYZ $uvDb] 1 $uvDb]
        } else {
          set xyz [addCache $ndx $uvDb]
        }
      } elseif { $eqI && $eqJ } {
        # Along original K edge, Interpolate xyz between bayK1 and bayK2
        set isDb [interpProject [list $bayI1 $bayJ1 $bayK1] \
                                [list $bayI1 $bayJ1 $bayK2] $sK uvDb xyz]
        addCache $ndx $xyz $isDb $uvDb
      } elseif { $eqI && $eqK } {
        # Along original J edge, Interpolate xyz between bayJ1 and bayJ2
        set isDb [interpProject [list $bayI1 $bayJ1 $bayK1] \
                                [list $bayI1 $bayJ2 $bayK1] $sJ uvDb xyz]
        addCache $ndx $xyz $isDb $uvDb
      } elseif { $eqJ && $eqK } {
        # Along original I edge, Interpolate xyz between bayI1 and bayI2
        set isDb [interpProject [list $bayI1 $bayJ1 $bayK1] \
                                [list $bayI2 $bayJ1 $bayK1] $sI uvDb xyz]
        addCache $ndx $xyz $isDb $uvDb
      } elseif { [isInteriorIndex $ndx] } {
        # In block interior - never db constrained
        set xyz [pwu::Vector3 affine $sK [getKFaceXYZ $i $j $bayK1] \
          [getKFaceXYZ $i $j $bayK2]]
        addCache $ndx $xyz [set isDb 0] [set uvDb {}]
      } elseif { $eqI } {
        # point on I boundary face - may be db constrained
        set isDb [interpProjectUVJ $ndx $bayI1 $bayJ1 $bayK1 $bayI2 $bayJ2 \
          $bayK2 $sK $sJ uvDb xyz]
      } elseif { $eqJ } {
        # point on J boundary - may be db constrained
        set isDb [interpProjectUVI $ndx $bayI1 $bayJ1 $bayK1 $bayI2 $bayJ2 \
          $bayK2 $sK $sI uvDb xyz]
      } elseif { $eqK } {
        # point on K boundary - may be db constrained
        set isDb [interpProjectUVJ $ndx $bayI1 $bayJ1 $bayK1 $bayI2 $bayJ2 \
          $bayK2 $sI $sJ uvDb xyz]
      }
      return [expr {$isDb ? $uvDb : $xyz}]
    }

    private proc getKFaceXYZ { i j origK } {
      variable mult_
      # convert origK coord to interp K coord
      set intpK [origToIntpNdx $origK]
      # Get xyz at {$i,$j} in origK face of block
      if { ![getCache [list $i $j $intpK] xyz uvDb isDb] } {
        lassign [getBay $j sJ] bayJ1 bayJ2
        set xyz [pwu::Vector3 affine $sJ \
          [getJKEdgeXYZ $i $bayJ1 $origK] \
          [getJKEdgeXYZ $i $bayJ2 $origK]]
        addCache [list $i $j $intpK] $xyz
      }
      return $xyz
    }

    private proc getJKEdgeXYZ { i origJ origK } {
      # Get xyz at $i along origJK edge of block
      variable mult_
      set intpJ [origToIntpNdx $origJ]
      set intpK [origToIntpNdx $origK]
      if { ![getCache [list $i $intpJ $intpK] xyz uvDb isDb] } {
        variable ent_
        lassign [getBay $i sI] bayI1 bayI2
        set xyz [pwu::Vector3 affine $sI \
          [$ent_ getXYZ -grid [list $bayI1 $origJ $origK]] \
          [$ent_ getXYZ -grid [list $bayI2 $origJ $origK]]]
        addCache [list $i $intpJ $intpK] $xyz
      }
      return $xyz
    }
  }

  namespace ensemble create
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
