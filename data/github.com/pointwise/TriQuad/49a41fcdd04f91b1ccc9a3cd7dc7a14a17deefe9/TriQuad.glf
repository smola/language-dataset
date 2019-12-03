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
## TriQuad.glf
##
## CREATE STRUCTURED TOPOLOGY FROM THREE SELECTED CONNECTORS
## 
## This script automates the creation of three structured domains from three 
## user-specified connectors. In addition to creating the new topology when 
## possible (based upon the edge dimensions), the elliptic solver is run
## for 10 iterations, allowing the newly generated domains to relax to an
## optimal configuration.
## 
## For maximum productivity, a GUI was excluded. Simply select three connectors
## and then run the script, or run the script and then select the connectors.
## 
#############################################################################

package require PWI_Glyph 2.3

set cwd [file dirname [info script]]

set input(Solve) 1

## Switch that interpolates gridline angles on outer edges, should remain 
## set to 1 for most applications.
set interpAngles 1

## Check that three connectors form singly-connected loop
proc isLoop { conList } {
    global nodes cons
    
    set order [list 0]
    
    ## Pick first connector
    set cons(1) [lindex $conList 0]
    
    ## Remove first connector from list for cross-referencing
    set t1 [lreplace $conList 0 0]
    
    ## Align all other connectors to con1
    $cons(1) alignOrientation $t1
    
    ## Find ends of first connector
    set nodes(0) [$cons(1) getNode Begin]
    set nodes(1) [$cons(1) getNode End]
    
    ## Identify connectors adjacent to end of first connector (node1)
    set adjCon1 [$nodes(1) getConnectors]
    
    ## Check list of adjacent connectors to find next connector in loop
    set chkCon 0
    foreach con $adjCon1 {
        set intersect [lsearch $t1 $con]
        if {$intersect != -1} {
            set chkCon [expr $chkCon + 1]
            set ind2 $intersect
        }
    }
    
    ## Error checking
    if {$chkCon > 1} {
        puts "Degenerate junction"
        return 0
    } elseif {$chkCon == 0} {
        puts "Bad connectivity"
        return 0
    }
    
    ## Identify second connector
    set cons(2) [lindex $t1 $ind2]
    set ind2 [lsearch $conList $cons(2)]
    set nodes(2) [$cons(2) getNode End]
    
    ## Identify connectors adjacent to end of second connector (node2)
    set adjCon2 [$nodes(2) getConnectors]
    
    ## Remove second connector from list for cross-referencing
    set t2 [lreplace $conList $ind2 $ind2]
    
    ## Check list of adjacent connectors to find next connector in loop
    set chkCon 0
    foreach con $adjCon2 {
        set intersect [lsearch $t2 $con]
        if {$intersect != -1} {
            set chkCon [expr $chkCon + 1]
            set ind3 $intersect
        }
    }
    
    ## Error checking
    if {$chkCon > 1} {
        puts "Degenerate junction"
        return 0
    } elseif {$chkCon == 0} {
        puts "Bad connectivity"
        return 0
    } elseif {[lindex $t2 $ind3] == $cons(1)} {
        puts "Two-connector loop."
        return 0
    }

    ## Identify third connector
    set cons(3) [lindex $t2 $ind3]
    
    return 1
}

proc splitTri { } {
    global cons
    
    set L1 [expr [$cons(1) getDimension] - 1 ]
    set L2 [expr [$cons(2) getDimension] - 1 ]
    set L3 [expr [$cons(3) getDimension] - 1 ]
    
    if { $L1 < [expr $L2 + $L3] } {
        set cond1 1
    } else { set cond1 0 }
    if { $L2 < [expr $L1 + $L3] } {
        set cond2 1
    } else { set cond2 0 }
    if { $L3 < [expr $L1 + $L2] } {
        set cond3 1
    } else { set cond3 0 }
    
    
    if { $cond1 && $cond2 && $cond3 } {
        set a [expr {($L1+$L3-$L2)/2. + 1}]
        set b [expr {($L1+$L2-$L3)/2. + 1}]
        set c [expr {($L2+$L3-$L1)/2. + 1}]
    
        if { $a == [expr int($a)] } {
            set cc1 1
            set a [expr int($a)]
        } else { set cc1 0 }
        if { $b == [expr int($b)] } {
            set cc2 1
            set b [expr int($b)]
        } else { set cc2 0 }
        if { $c == [expr int($c)] } {
            set cc3 1
            set c [expr int($c)]
        } else { set cc3 0 }
        
        if { $cc1 && $cc2 && $cc3 } {
            clearDom [list $cons(1) $cons(2) $cons(3)]

            set pt1 [$cons(1) getXYZ -grid $b]
            set pt2 [$cons(2) getXYZ -grid $c]
            set pt3 [$cons(3) getXYZ -grid $a]
            
            lappend splCon [$cons(1) split -I $b]
            lappend splCon [$cons(2) split -I $c]
            lappend splCon [$cons(3) split -I $a]
            
            return [list [list $a $b $c] [list $pt1 $pt2 $pt3] $splCon]
        } else { 
            ## dimensions not even
            return -code error "Sum of all connector dimensions must be odd."
        }
    } else {
        ## One dimension is too large
        return -code error "No connector may have a dimension larger\
            than the sum of the other two."
    }
}

## Create two point connector given two points
proc createTwoPt { pt1 pt2 dim } {
    set creator [pw::Application begin Create]
        set con [pw::Connector create]
        set seg [pw::SegmentSpline create]
        $seg addPoint $pt1
        $seg addPoint $pt2
        $con addSegment $seg
    $creator end
    $con setDimension $dim
    return $con
}

## Run elliptic solver for 10 interations with floating BC on interior lines to 
## smooth grid
proc solve_Grid { cntr doms } {
    global interpAngles
    
    set solver_mode [pw::Application begin EllipticSolver $doms]
        if {$interpAngles == 1} {
            foreach ent $doms {
                foreach bc [list 1 2 3 4] {
                    $ent setEllipticSolverAttribute -edge $bc \
                        EdgeAngleCalculation Interpolate
                }
            }
        }
        
        for {set ii 0} {$ii<3} {incr ii} {
            set tempDom [lindex $doms $ii]
            set inds [list]
            for {set jj 1 } {$jj <= 4 } {incr jj} {
                set tmpEdge [$tempDom getEdge $jj]
                set n1 [$tmpEdge getNode Begin]
                set n2 [$tmpEdge getNode End]
                set c1 [pwu::Vector3 equal -tolerance 1e-6 [$n1 getXYZ] $cntr]
                set c2 [pwu::Vector3 equal -tolerance 1e-6 [$n2 getXYZ] $cntr]
                if { $c1 || $c2 } {
                    lappend inds [list $jj]
                }
            }
            set temp_list [list]
            for {set jj 0} {$jj < [llength $inds] } {incr jj} {
                lappend temp_list [list $tempDom]
            }
            foreach ent $temp_list bc $inds {
                $ent setEllipticSolverAttribute -edge $bc \
                    EdgeConstraint Floating
                $ent setEllipticSolverAttribute -edge $bc \
                    EdgeAngleCalculation Orthogonal
            }
        }
        
        $solver_mode run 10
    $solver_mode end
}

## Remove existing domain, if it exists between specified connectors
proc clearDom { cons } {
    set existDoms [pw::Domain getDomainsFromConnectors \
        [lindex $cons 0]]
    
    foreach con [lrange $cons 1 2] {
        set tempExist [list]
        set tempDoms [pw::Domain getDomainsFromConnectors $con]
        foreach tD $tempDoms {
            if {[lsearch $existDoms $tD] != -1} {
                lappend tempExist $tD
            }
        }
        set existDoms $tempExist
    }
    
    if { [llength $existDoms]==1 } {
        puts "Deleting existing grid."
        pw::Entity delete $existDoms
    }
    
    return
}

## Create domains
proc createTopo { pts dims outerCons } {
    global input

    set pt0 [lindex $pts 0]
    set pt1 [lindex $pts 1]
    set pt2 [lindex $pts 2]
    
    set temp1 [pwu::Vector3 add $pt0 $pt1]
    set temp2 [pwu::Vector3 add $temp1 $pt2]
    set cntr [pwu::Vector3 divide $temp2 3.0]
    
    set nc1 [createTwoPt $pt0 $cntr [lindex $dims 2]]
    set nc2 [createTwoPt $pt1 $cntr [lindex $dims 0]]
    set nc3 [createTwoPt $pt2 $cntr [lindex $dims 1]]
    
    set conList [list $nc1 $nc2 $nc3]
    foreach oc $outerCons {
        foreach c $oc {
            lappend conList $c
        }
    }
    
    set doms [pw::DomainStructured createFromConnectors $conList]
    
    if $input(Solve) {
        solve_Grid $cntr $doms
    }
    
    return
}

set text1 "Please select three connectors to create Tri-Quad topology."
set mask [pw::Display createSelectionMask -requireConnector {}]

###############################################
## NOTE: This script uses the getSelectedEntities command added in 17.2R2
## Replace following block of code with single line < set N_con 0 > for previous
## versions if desired
if { [catch {pw::Display getSelectedEntities -selectionmask $mask curSelection}] } {
    puts {Command "getSelectedEntities" does not exist}
    set N_con 0

    ## Loop until three connectors are selected or the script is aborted
    while {$N_con != 3} {
        set picked [pw::Display selectEntities -description $text1 -selectionmask $mask curSelection]

        set N_con [llength $curSelection(Connectors)]
        
        if {$picked} {
            if {$N_con != 3} {
                puts "$N_con connectors selected. Please select 3"
            }
        } else {
            puts "Script aborted."
            exit
        }
    }
} elseif { [llength $curSelection(Connectors)] != 3 } {
    puts "Exactly 3 connectors are required."
    exit
}
###############################################

## Determine if connectors form a valid triangular loop
if { ! [isLoop $curSelection(Connectors)] } {
    puts "No loop present, script aborted"
    exit
}

## Attempt splitting operation
#~ set temp [splitTri]

## Check results of split
if { [catch { splitTri } results ] } {
    puts "Unable to match topology, check edge dimensions."
    puts $results
} else {
    set dims [lindex $results 0]
    set pts [lindex $results 1]
    set splCons [lindex $results 2]
    
    createTopo $pts $dims $splCons
}

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

