#
# Copyright 2018 (c) Pointwise, Inc.
# All rights reserved.
#
# This sample Pointwise script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#

#############################################################################
##
## CreateOH.glf
##
## CREATE OH TOPOLOGY FROM FOUR SELECTED CONNECTORS
## 
## This script automates the creation of an OH topology from four user-specified
## connectors. In addition to creating the new topology, the elliptic solver can
## be run for 10 iterations, allowing the newly generate geometry to relax to an
## optimal configuration.
## 
#############################################################################

package require PWI_Glyph 2.3

set cwd [file dirname [info script]]

## Default values for variables also visible in TK widget
set input(solveGrid) 1
set input(alpha) 0.6
set input(sDim) 11

## Switch that interpolates gridline angles on outer edges, should remain 
## set to 1 for most applications.
set interpAngles 1

## Check that four connectors form singly-connected loop
proc isLoop { conList } {
    set order [list 0]
    
    ## Pick first connector
    set con1 [lindex $conList 0]
    
    ## Find ends of first connector
    set node0 [$con1 getNode Begin]
    set node1 [$con1 getNode End]
    
    ## Identify connectors adjacent to end of first connector (node1)
    set adjCon1 [$node1 getConnectors]
    
    ## Remove first connector from list for cross-referencing
    set t1 [lreplace $conList 0 0]
    
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
        return -1
    } elseif {$chkCon == 0} {
        puts "Bad connectivity"
        return -1
    }
    
    ## Identify second connector
    set con2 [lindex $t1 $ind2]
    set ind2 [lsearch $conList $con2]
    set beginNode2 [$con2 getNode Begin]
    set endNode2 [$con2 getNode End]
    if {$beginNode2 == $node1} {
        set node2 $endNode2
    } else {
        set node2 $beginNode2
    }
    
    ## Identify connectors adjacent to end of second connector (node2)
    set adjCon2 [$node2 getConnectors]
    
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
        return -1
    } elseif {$chkCon == 0} {
        puts "Bad connectivity"
        return -1
    } elseif {[lindex $t2 $ind3] == $con1} {
        puts "Two-connector loop."
        return -1
    }

    ## Identify third connector
    set con3 [lindex $t2 $ind3]
    set ind3 [lsearch $conList $con3]
    set beginNode3 [$con3 getNode Begin]
    set endNode3 [$con3 getNode End]
    if {$beginNode3 == $node2} {
        set node3 $endNode3
    } else {
        set node3 $beginNode3
    }
    
    set adjCon3 [$node3 getConnectors]
    
    set t3 [lreplace $conList $ind3 $ind3]
    
    set chkCon 0
    foreach con $adjCon3 {
        set intersect [lsearch $t3 $con]
        if {$intersect != -1} {
            set chkCon [expr $chkCon + 1]
            set ind4 $intersect
        }
    }
    
    ## Error checking
    if {$chkCon > 1} {
        puts "Degenerate junction"
        return -1
    } elseif {$chkCon == 0} {
        puts "Bad connectivity"
    } elseif {[lindex $t3 $ind3] == $con1} {
        puts "Three-connector loop."
        return -1
    }
    
    set con4 [lindex $t3 $ind4]
    
    ## Return ordered list of nodes and connectors
    set nodes [list $node0 $node1 $node2 $node3]
    set cons [list $con1 $con2 $con3 $con4]
    
    return [list $nodes $cons]
}

## Remove existing domain, if it exists between specified connectors
proc clearDom { cons } {
    set existDoms [pw::Domain getDomainsFromConnectors \
        [lindex $cons 0]]
    
    foreach con [lrange $cons 1 3] {
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
        puts "Deleting existing H-grid."
        pw::Entity delete $existDoms
    }
    
    return
}

## Create two point connector given two points
proc createTwoPt { pt1 pt2 } {
    set creator [pw::Application begin Create]
        set con [pw::Connector create]
        set seg [pw::SegmentSpline create]
        $seg addPoint $pt1
        $seg addPoint $pt2
        $con addSegment $seg
    $creator end
    return $con
}

## Create structured domain from list of four connectors
proc makeStructDom { conList } {
    set NCons [llength $conList]
    if {$NCons != 4} {  
        puts "Incorrect number of connectors"
        exit
    } else {
        set DomCreate [pw::Application begin Create]
            set dom [pw::DomainStructured create]
            foreach CL $conList {
                set edge [pw::Edge create]
                foreach c $CL {
                    $edge addConnector $c
                }
                $dom addEdge $edge
            }
        $DomCreate end
        return $dom
    }
}

## Find geometric center of four nodes
proc getCenter { nodes } {
    set pt0 [[lindex $nodes 0] getXYZ]
    set pt1 [[lindex $nodes 1] getXYZ]
    set pt2 [[lindex $nodes 2] getXYZ]
    set pt3 [[lindex $nodes 3] getXYZ]
    
    set temp1 [pwu::Vector3 add $pt0 $pt1]
    set temp2 [pwu::Vector3 add $pt2 $pt3]
    set cntr [pwu::Vector3 divide \
        [pwu::Vector3 add $temp1 $temp2] 4.0]
    
    return $cntr
}

## Find weighted average of two points
proc avgPoint { pt1 pt2 } {
    global input
    
    set offset [pwu::Vector3 scale \
        [pwu::Vector3 subtract $pt2 $pt1] [expr 1-$input(alpha)]]
    
    return [pwu::Vector3 add $offset $pt1]
}

## Find locations and create new nodes and connectors for OH topology
proc newTopo { nodes } {
    puts "Creating new topology."

    set centerPt [getCenter $nodes]
    
    set pt0 [[lindex $nodes 0] getXYZ]
    set pt1 [[lindex $nodes 1] getXYZ]
    set pt2 [[lindex $nodes 2] getXYZ]
    set pt3 [[lindex $nodes 3] getXYZ]
    
    set np0 [avgPoint $pt0 $centerPt]
    set np1 [avgPoint $pt1 $centerPt]
    set np2 [avgPoint $pt2 $centerPt]
    set np3 [avgPoint $pt3 $centerPt]

    set con0 [createTwoPt $np0 $np1]
    set con1 [createTwoPt $np1 $np2]
    set con2 [createTwoPt $np2 $np3]
    set con3 [createTwoPt $np3 $np0]
    
    set square [list $con0 $con1 $con2 $con3]
    
    set con4 [createTwoPt $np0 $pt0]
    set con5 [createTwoPt $np1 $pt1]
    set con6 [createTwoPt $np2 $pt2]
    set con7 [createTwoPt $np3 $pt3]
    
    set spokes [list $con4 $con5 $con6 $con7]
    
    return [list $square $spokes]
    
}

## Run elliptic solver for 10 interations with floating BC on interior lines to 
## smooth grid
proc solve_Grid { doms } {
    global interpAngles
    
    set solver_mode [pw::Application begin EllipticSolver $doms]
        for {set ii 0} {$ii<5} {incr ii} {
            set temp_dom [lindex $doms $ii]
            if {$ii != 0} {
                set inds [list 2 3 4]
            } else {
                set inds [list 1 2 3 4]
            }
            set temp_list [list]
            for {set jj 0} {$jj < [llength $inds] } {incr jj} {
                lappend temp_list [list $temp_dom]
            }
            foreach ent $temp_list bc $inds {
                $ent setEllipticSolverAttribute -edge $bc \
                    EdgeConstraint Floating
            }
        }
        
        if {$interpAngles == 1} {
            set edgeDoms [lreplace $doms 0 0]
            foreach ent $edgeDoms bc [list 1 1 1 1] {
                $ent setEllipticSolverAttribute -edge $bc \
                    EdgeAngleCalculation Interpolate
            }
        }
        
        $solver_mode run 10
    $solver_mode end
}

## Main process called by TK widget to select connectors and create topology
proc selectCons {} {
    global w input pickedCons curSelection infoMessage
    
    wm withdraw .

    set text1 "Please select four connectors to create OH topology."
    set mask [pw::Display createSelectionMask -requireConnector {}]
    set N_con 0
    
    puts "Select connectors and press Done."

    while {$N_con != 4} {
        set picked [pw::Display selectEntities -description $text1 \
            -selectionmask $mask curSelection]

        set N_con [llength $curSelection(Connectors)]
        
        if {$picked} {
            if {$N_con != 4} {
                puts "$N_con connectors selected. Please select 4."
            }
        } else {
            puts "No connectors selected, click Cancel to quit or \
                select 4 connectors."
            set infoMessage "Invalid loop, press Pick Connectors"
            wm deiconify .
            return
        }
    }

    set temp [isLoop $curSelection(Connectors)]
    set curSelection(nodes) [lindex $temp 0]
    set curSelection(cons) [lindex $temp 1]

    if {$curSelection(nodes) == -1} {
        puts "No loop present, please select a closed loop of 4 connectors."
        set infoMessage "Invalid loop, press Pick Connectors"
    } else {
        set pickedCons 1
        $w(EntryDimension) configure -state normal
        $w(EntryExtent) configure -state normal
        $w(EntrySolve) configure -state normal
        updateButtons
    }
    wm deiconify .
}

# PROC: retrieveCons
# Proc to attempt to retrieve selected connectors if applicable 
proc retrieveCons {} {
    global pickedCons curSelection infoMessage

    set picked [pw::Display getSelectedEntities curSelection]
    set N_con [llength $curSelection(Connectors)]

    if { $picked && $N_con == 4 } {
        set temp [isLoop $curSelection(Connectors)]
        set curSelection(nodes) [lindex $temp 0]
        set curSelection(cons) [lindex $temp 1]

        if {$curSelection(nodes) == -1} {
            puts "No loop present, please select a closed loop of 4 connectors."
            set infoMessage "Invalid loop, press Pick Connectors"
        } else {
            set pickedCons 1
            return true
        }
    } elseif $picked {
        puts "No loop present, please select a closed loop of 4 connectors."
        set infoMessage "Invalid loop, press Pick Connectors"
    }
    return false
}


## Process called by TK widget to create topology
proc createOH {} {
    global input curSelection
    
    wm withdraw .

    clearDom $curSelection(Connectors)

    set newCons [newTopo $curSelection(nodes) ]

    set square [lindex $newCons 0]
    set spokes [lindex $newCons 1]

    for {set ii 0} {$ii < 4} {incr ii} {
        set outerDim [[lindex $curSelection(cons) $ii] getDimension]
        [lindex $square $ii] setDimension $outerDim
        [lindex $spokes $ii] setDimension $input(sDim)
    }

    set con(1) [lindex $curSelection(cons) 0]
    set con(2) [lindex $curSelection(cons) 1]
    set con(3) [lindex $curSelection(cons) 2]
    set con(4) [lindex $curSelection(cons) 3]

    set con(5) [lindex $square 0]
    set con(6) [lindex $square 1]
    set con(7) [lindex $square 2]
    set con(8) [lindex $square 3]

    set con(9) [lindex $spokes 0]
    set con(10) [lindex $spokes 1]
    set con(11) [lindex $spokes 2]
    set con(12) [lindex $spokes 3]

    ## Create new domains from newly created connectors
    set dom(1) [makeStructDom $square]
    set dom(2) [makeStructDom [list $con(1) $con(10) \
        $con(5) $con(9)]]
    set dom(3) [makeStructDom [list $con(2) $con(11) \
        $con(6) $con(10)]]
    set dom(4) [makeStructDom [list $con(3) $con(12) \
        $con(7) $con(11)]]
    set dom(5) [makeStructDom [list $con(4) $con(9) \
        $con(8) $con(12)]]

    set doms [list]
    for {set ii 1} {$ii < 6} { incr ii} {
        lappend doms [list $dom($ii)]
    }
        
    ## If solveGrid is not set to 1, ask if the grid should be smoothed
    if {$input(solveGrid) == 1} {
        solve_Grid $doms
    }

    exit
}

###########################################################################
## GUI 
###########################################################################
## Load TK
pw::Script loadTk

# Initialize globals
set infoMessage ""
set pickedCons -1

set color(Valid)   "white"
set color(Invalid) "misty rose"

set w(LabelTitle)           .title
set w(FrameMain)          .main
  set w(ButtonSelect)       $w(FrameMain).select
  set w(LabelDimension)     $w(FrameMain).ldim
  set w(EntryDimension)     $w(FrameMain).edim
  set w(LabelExtent)          $w(FrameMain).lext
  set w(EntryExtent)          $w(FrameMain).eext
  set w(LabelSolve)            $w(FrameMain).lslv
  set w(EntrySolve)            $w(FrameMain).eslv
  set w(ButtoncOH)            $w(FrameMain).doit
set w(FrameButtons)      .fbuttons
  set w(Logo)                   $w(FrameButtons).pwlogo
  set w(ButtonCancel)        $w(FrameButtons).bcancel
set w(Message)             .msg

# dimension field validation
proc validateDim { dim widget } {
  global w color
  if { [string is integer -strict $dim] && ($dim == 0 || $dim > 1) } {
    $w($widget) configure -background $color(Valid)
  } else {
    $w($widget) configure -background $color(Invalid)
  }
  updateButtons
  return 1
}

# extent field validation
proc validateAlpha { alpha widget } {
  global w color
  if { [string is double -strict $alpha] && ($alpha > 0 && $alpha < 1) } {
    $w($widget) configure -background $color(Valid)
  } else {
    $w($widget) configure -background $color(Invalid)
  }
  updateButtons
  return 1
}

# return true if none of the entry fields are marked invalid
proc canCreate { } {
  global w color pickedCons
  return [expr \
    [string equal -nocase [$w(EntryDimension) cget -background] $color(Valid)] && \
    [string equal -nocase [$w(EntryExtent) cget -background] $color(Valid)] && \
    $pickedCons == 1]
}

# enable/disable action buttons based on current settings
proc updateButtons { } {
  global w infoMessage

  if { [canCreate] } {
    $w(ButtoncOH) configure -state normal
    set infoMessage "Press Create OH"
  } else {
    $w(ButtoncOH) configure -state disabled
    set infoMessage "Invalid parameter"
  }
  update
}

# set the font for the input widget to be bold and 1.5 times larger than
# the default font
proc setTitleFont { l } {
  global titleFont
  if { ! [info exists titleFont] } {
    set fontSize [font actual TkCaptionFont -size]
    set titleFont [font create -family [font actual TkCaptionFont -family] \
        -weight bold -size [expr {int(1.5 * $fontSize)}]]
  }
  $l configure -font $titleFont
}

# Build the user interface
proc makeWindow { haveSelection } {
  global w input color

  # Ceate the widgets
  label $w(LabelTitle) -text "Create OH\nInput Parameters"
  setTitleFont $w(LabelTitle)

  frame $w(FrameMain)

  button $w(ButtonSelect) -text "Pick Connectors" -command { selectCons }

  label $w(LabelDimension) -text "Radial dimension:" -anchor e
  entry $w(EntryDimension) -width 6 -bd 2 -textvariable input(sDim)
  $w(EntryDimension) configure -background $color(Valid)

  label $w(LabelExtent) -text "Radial extent:" -padx 2 -anchor e
  entry $w(EntryExtent) -width 10 -bd 2 -textvariable input(alpha)
  $w(EntryExtent) configure -background $color(Valid)

  label $w(LabelSolve) -text "Run solver?" -padx 2 -anchor e
  checkbutton $w(EntrySolve) -variable input(solveGrid)
  
  button $w(ButtoncOH) -text "Create OH" -command { createOH }

  message $w(Message) -textvariable infoMessage -background beige \
                      -bd 2 -relief sunken -padx 5 -pady 5 -anchor w \
                      -justify left -width 300

  frame $w(FrameButtons) -relief sunken

  button $w(ButtonCancel) -text "Cancel" -command { destroy . }
  label $w(Logo) -image [pwLogo] -bd 0 -relief flat

  # set up validation after all widgets are created so that they all exist when
  # validation fires the first time; if they don't all exist, updateButtons
  # will fail
  $w(EntryDimension) configure -validate key \
    -vcmd { validateDim %P EntryDimension }
  $w(EntryExtent) configure -validate key \
    -vcmd { validateAlpha %P EntryExtent }

  # lay out the form
  pack $w(LabelTitle) -side top
  pack [frame .sp -bd 1 -height 2 -relief sunken] -pady 4 -side top -fill x
  pack $w(FrameMain) -side top -fill both -expand 1

  # lay out the form in a grid
  grid $w(ButtonSelect) -columnspan 2 -pady 3
  grid $w(LabelDimension) $w(EntryDimension) -sticky ew -pady 3 -padx 3
  grid $w(LabelExtent) $w(EntryExtent) -sticky ew -pady 3 -padx 3
  grid $w(LabelSolve) $w(EntrySolve) -sticky ew -pady 3 -padx 3
  grid $w(ButtoncOH) -columnspan 2 -pady 3

  # give all extra space to the second (last) column
  grid columnconfigure $w(FrameMain) 1 -weight 1

  pack $w(Message) -side bottom -fill x -anchor s
  pack $w(FrameButtons) -fill x -side bottom -padx 2 -pady 4 -anchor s
  pack $w(ButtonCancel) -side right -padx 2
  pack $w(Logo) -side left -padx 5

  bind . <Key-Escape> { $w(ButtonCancel) invoke }
  bind . <Control-Key-Return> { $w(ButtonSelect) invoke }
  bind . <Control-Key-f> { $w(ButtoncOH) invoke }
  bind $w(EntryExtent) <Key-Return> { $w(ButtoncOH) invoke }

  # move keyboard focus to the first entry
  focus $w(ButtonSelect)
  raise .

  if { $haveSelection } {
    $w(EntryDimension) configure -state normal
    $w(EntryExtent) configure -state normal
    $w(EntrySolve) configure -state normal
    $w(ButtoncOH) configure -state normal
    updateButtons
  } else {
    $w(EntryDimension) configure -state disabled
    $w(EntryExtent) configure -state disabled
    $w(EntrySolve) configure -state disabled
    $w(ButtoncOH) configure -state disabled
  }
}

###############################################################################
# pwLogo: Define pointwise logo
###############################################################################
proc pwLogo {} {
  set logoData {
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
0hkKgcy9HHEQDcRyAr0ChAWWucwNMIJZ5KilNGvpADtt5JrYzKY2t8nNbnrzm+B8SEAAADs=}

  return [image create photo -format GIF -data $logoData]
}

makeWindow [retrieveCons]

tkwait window .

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
