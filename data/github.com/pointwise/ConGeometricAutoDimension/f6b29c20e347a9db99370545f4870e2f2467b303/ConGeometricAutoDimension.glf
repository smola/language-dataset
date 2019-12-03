# 
# This sample script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.  
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#
############################################################################
# Concept by: Paul Ferlemann, NASA Langley Research Center
# Majority of programming by: Travis Carrigan, Pointwise, Inc.
############################################################################
# A Pointwise Glyph script to auto-dimension a selected connector with
#   a geometric distribution function.
#
# After selecting a connector, user inputs:
#   the end spacings, max spacing, a growth rate, and desired number of grid
#   levels.
#
# The objective is to produce a distribution which increases from
#   the end spacings up to the max spacing from either or both ends
#   of the connector.
#
# This requires determining the number of layers from each end and the
#   connector dimension; a tedious trial and error activity if done
#   interactively.  In addition, a calculation with logarithms is required
#   for the number of layers.
#
# The required dimension will be determined and then increased, if requested,
#   to facilitate grid sequencing.
#   
# There is no "Cancel" or undo internal to the script.
#   If you don't like what happens, click "Done" and undo the script
#   execution directly in Pointwise.
############################################################################

package require PWI_Glyph
pw::Script loadTK

############################################################################
# GUI
############################################################################
wm title . "Geometric Auto Dimension"
grid [ttk::frame .f -padding "5 5 5 5"] -column 0 -row 0 -sticky nwes
grid columnconfigure . 0 -weight 1
grid rowconfigure    . 0 -weight 1

set beginspacing  ""
set endspacing    ""
set maxspacing    ""
set growthratio   ""
set gridlevels    "1"

lappend infoMessages "Begin by selecting a connector.\n"
lappend infoMessages "To undo, click \"Done\" and undo in PW."
set infoMessage [join $infoMessages ""]

set labelWidth       20
set entryWidth       10
set buttonWidthSmall 10
set buttonWidthBig   40

grid [ttk::button .f.scb -text "Select Connector" -width $buttonWidthBig -command pickCon] -column 0 -row 0 -columnspan 2

grid [ttk::label .f.bspcl -text "Begin Spacing" -width $labelWidth] -column 0 -row 1 -sticky w
grid [ttk::label .f.espcl -text "End Spacing"   -width $labelWidth] -column 0 -row 2 -sticky w
grid [ttk::label .f.mspcl -text "Max Spacing"   -width $labelWidth] -column 0 -row 3 -sticky w
grid [ttk::label .f.grl   -text "Growth Ratio"  -width $labelWidth] -column 0 -row 4 -sticky w
grid [ttk::label .f.gll   -text "Grid Levels"   -width $labelWidth] -column 0 -row 5 -sticky w

grid [tk::entry .f.bspce -width $entryWidth -textvariable beginspacing] -column 1 -row 1 -sticky e
grid [tk::entry .f.espce -width $entryWidth -textvariable endspacing  ] -column 1 -row 2 -sticky e
grid [tk::entry .f.mspce -width $entryWidth -textvariable maxspacing  ] -column 1 -row 3 -sticky e
grid [tk::entry .f.gre   -width $entryWidth -textvariable growthratio ] -column 1 -row 4 -sticky e
grid [tk::entry .f.gle   -width $entryWidth -textvariable gridlevels  ] -column 1 -row 5 -sticky e

grid [tk::message .f.m -textvariable infoMessage -background beige -bd 2 -relief sunken -padx 5 -pady 5 -anchor w -justify left -width 250] -column 0 -row 6 -columnspan 2 -sticky ew

grid [ttk::separator .f.s -orient horizontal] -column 0 -row 7 -columnspan 2 -sticky ew

grid [ttk::button .f.ab -text "Apply"  -width $buttonWidthSmall -command autoDim ] -column 0 -row 8 -sticky w
grid [ttk::button .f.db -text "Done"   -width $buttonWidthSmall -command done    ] -column 1 -row 8 -sticky e

foreach w [winfo children .f] {grid configure $w -padx 5 -pady 5}

::tk::PlaceWindow . widget
wm resizable . 0 0

############################################################################
# Update Button and Entry States
############################################################################
proc updateStates {} {

    global Con

    if [info exists Con] {

        .f.scb   configure -state enabled
        .f.bspce configure -state normal
        .f.espce configure -state normal
        .f.mspce configure -state normal
        .f.gre   configure -state normal
        .f.gle   configure -state normal
    
        if [colorCheck] {
            .f.ab configure -state enabled
        } else {
            .f.ab configure -state disabled
        }
    
    } else {
    
        .f.scb   configure -state enabled
        .f.bspce configure -state disabled
        .f.espce configure -state disabled
        .f.mspce configure -state disabled
        .f.gre   configure -state disabled
        .f.gle   configure -state disabled
        .f.ab    configure -state disabled
    
    }

}

############################################################################
# Check Entry Color
############################################################################
proc colorCheck {} {

    set test 0

    if {![string equal [.f.bspce cget -background] #EBAD99]} {incr test}
    if {![string equal [.f.espce cget -background] #EBAD99]} {incr test}
    if {![string equal [.f.mspce cget -background] #EBAD99]} {incr test}
    if {![string equal [.f.gre   cget -background] #EBAD99]} {incr test}
    if {![string equal [.f.gle   cget -background] #EBAD99]} {incr test}

    # Only passes if all entry boxes are not red
    if {$test == 5} {
        return true
    } else {
        return false
    }

}

############################################################################
# Real Time Parameter Validation: only used when editing parameters
############################################################################
proc validateParams {u widget} {

    global infoMessage

    # Begin spacing
    if {$widget == ".f.bspce"} {
        if {[llength $u] == 1 && [string is double -strict $u] && $u >= 0} {
            $widget configure -background white
            set infoMessage "Enter begin spacing."
        } else {
            $widget configure -background #EBAD99
            set infoMessage "Begin spacing must be >= 0"
        }
    }

    # End spacing
    if {$widget == ".f.espce"} {
        if {[llength $u] == 1 && [string is double -strict $u] && $u >= 0} {
            $widget configure -background white
            set infoMessage "Enter end spacing."
        } else {
            $widget configure -background #EBAD99
            set infoMessage "End spacing must be >= 0"
        }
    }

    # Max spacing
    if {$widget == ".f.mspce"} {
        if {[llength $u] == 1 && [string is double -strict $u] && $u > 0} {
            $widget configure -background white
            set infoMessage "Enter max spacing."
        } else {
            $widget configure -background #EBAD99
            set infoMessage "Max spacing must be > 0"
        }
    }

    # Growth ratio
    if {$widget == ".f.gre"} {
        if {[llength $u] == 1 && [string is double -strict $u] && $u > 1} {
            $widget configure -background white
            set infoMessage "Enter growth ratio."
        } else {
            $widget configure -background #EBAD99
            set infoMessage "Growth ratio must be > 1"
        }
    }

    # Grid levels
    if {$widget == ".f.gle"} {
        if {[llength $u] == 1 && [string is double -strict $u] && 
            (($u == 1) || ($u == 2) || ($u == 3) || ($u == 4))} {
            $widget configure -background white
            set infoMessage "Enter number of grid levels."
        } else {
            $widget configure -background #EBAD99
            set infoMessage "Acceptable levels = 1, 2, 3, or 4"
        }
    }

    updateStates
    return true
} 

############################################################################
# pickCon: select connector to dimension and distribute
############################################################################
proc pickCon { { firstRun false } } {

    global Con PoleCon beginspacing endspacing maxspacing infoMessage
    
    # Turn off nodes if a connector is already selected
    if [info exists Con] { $Con setRenderAttribute PointMode None }

    # If a pole had been created (for the previous connector), delete it
    if [info exists PoleCon] { $PoleCon delete }

    # If this is run at startup and a valid connector was selected, don't run this piece 
    if { !$firstRun } {
        wm state . withdrawn
        set conMask [pw::Display createSelectionMask -requireConnector {} -blockConnector {Pole}]
        pw::Display selectEntities -selectionmask $conMask -description "Select a connector." -single results
        set Con $results(Connectors); # Get the connector from the resultVar array
    }

    if {[llength $Con] == 1} {

        set length [$Con getTotalLength]
        set dim [$Con getDimension]

        set conStart [$Con getXYZ -arc 0.0]
            set conStartX [lindex $conStart 0]
            set conStartY [lindex $conStart 1]
            set conStartZ [lindex $conStart 2]
        set conEnd [$Con getXYZ -arc 1.0]
            set conEndX [lindex $conEnd 0]
            set conEndY [lindex $conEnd 1]
            set conEndZ [lindex $conEnd 2]

        if { $dim > 2 } {                          ; # If the connector was previously dimensioned, determine
            for {set i 2} {$i <= $dim} {incr i} {  ; # begin, end, and max spacings to 5 decimal places
                set a [$Con getXYZ -grid $i]
                set b [$Con getXYZ -grid [expr $i-1]]
                set spacing [pwu::Vector3 length [pwu::Vector3 subtract $a $b]]
                if { $i == 2 }    { set beginspacing [expr (int($spacing*100000))/100000.0] }
                if { $i == $dim } { set endspacing   [expr (int($spacing*100000))/100000.0] }
                if { $spacing > $maxspacing } { set maxspacing [expr (int($spacing*100000))/100000.0] }
            }
        }

        # Turn on nodes for newly selected connector
        $Con setRenderAttribute PointMode All

        # Create pole node at beginning of connector
        set segment [pw::SegmentSpline create]
            $segment addPoint "$conStartX $conStartY $conStartZ"
        set PoleCon [pw::Connector create]
            $PoleCon addSegment $segment

        lappend infoMessages "--------------------------------------\n"
        lappend infoMessages "[$Con getName] selected\n"
        lappend infoMessages "--------------------------------------\n"
        lappend infoMessages "Beginning has been marked.\n"
        lappend infoMessages [format "Connector starts at: %.2f %.2f %.2f\n" $conStartX $conStartY $conStartZ]
        lappend infoMessages [format "Connector ends at: %.2f %.2f %.2f\n" $conEndX $conEndY $conEndZ]
        lappend infoMessages [format "Connector length is: %.2f\n" $length]
        lappend infoMessages [format "Connector dimension is: %i" $dim]

    } else {
        unset Con
        unset PoleCon
        lappend infoMessages "No connector was selected!"
    }

    set infoMessage [join $infoMessages ""]
    wm state . normal
    raise .
    pw::Display update
    updateStates
    set firstRun 0
}

############################################################################
# Dimension and Distribute
############################################################################
proc autoDim { } {

    global Con infoMessage
    
    if {![info exists Con] || ![colorCheck]} { exit }
    
    set length [$Con getTotalLength]
    
    #**********************************
    # Get these user inputs from a Tk interface
    global beginspacing
    global endspacing
    global maxspacing
    global growthratio
    global gridlevels
    #**********************************
    
    set infoMessages ""
    set errorMessages ""
    
    lappend infoMessages "--------------------------------------\n"
    lappend infoMessages "[$Con getName] statistics\n"
    lappend infoMessages "--------------------------------------\n"
    
    # Calculate number of begin layers
    if { $beginspacing > 0.0 && $beginspacing < $maxspacing } {
        set beginlayers [expr 1 + int(log($maxspacing / $beginspacing) / log($growthratio))]
        lappend infoMessages "Begin growth layers (cells) = $beginlayers\n"
    } else {
        set beginlayers 0
        lappend errorMessages "No spacing set at start of connector!\n"
    }
    
    # Calculate number of end layers
    if { $endspacing > 0.0 && $endspacing < $maxspacing } {
        set endlayers [expr 1 + int(log($maxspacing / $endspacing) / log($growthratio))]
        lappend infoMessages "End growth layers (cells) = $endlayers\n"
    } else {
        set endlayers 0
        lappend errorMessages "No spacing set at end of connector!\n"
    }
    
    # Calculate total length of begin layers
    if { $beginlayers > 0 } {
        set beginheight [expr $beginspacing * (1 - pow($growthratio,$beginlayers)) / (1 - $growthratio)]
        lappend infoMessages [format "Begin layer height = %.2f\n" $beginheight]
    } else {
        set beginheight 0
    }
    
    # Calculate total length of end layers
    if { $endlayers > 0 } {
        set endheight [expr $endspacing * (1 - pow($growthratio,$endlayers)) / (1 - $growthratio)]
        lappend infoMessages [format "End layer height = %.2f\n" $endheight]
    } else {
        set endheight 0
    }
    
    # Check validity of inputs relative to the length of the connector
    if { [expr $beginheight + $endheight] > $length } {
        lappend errorMessages "Error: Layer height is greater than connector length!\n"
        lappend errorMessages "Increase growth rate!\n"
    } else {
    
        # Calculate initial connector dimension
        set ConDim [expr 2 + $beginlayers + $endlayers + int(($length-$beginheight-$endheight)/$maxspacing)]
        lappend infoMessages "Initial connector dimension = $ConDim\n"
    
        # Possibly increase the dimension for grid sequencing
        set factor [expr pow(2,($gridlevels-1))]
        while {[expr fmod($ConDim-1,$factor)] != 0} { incr ConDim }
    
        # Include the following information whether or not the dimension was actually adjusted.
        # This informs the user if the final dimension was the optimal dimension (no clustering towards the middle).
        if { $gridlevels > 1 } { lappend infoMessages "Adjusted connector dimension = $ConDim" }
    
        # Dimension the connector
        $Con setDimension $ConDim
    
        # Set the distribution
        $Con setDistribution 1 [pw::DistributionGrowth create]
        if { $beginheight > 0 } {
            [$Con getDistribution 1] setBeginSpacing $beginspacing
            [$Con getDistribution 1] setBeginRate    $growthratio
            [$Con getDistribution 1] setBeginLayers  $beginlayers
        }
        if { $endheight > 0 } {
            [$Con getDistribution 1] setEndSpacing $endspacing
            [$Con getDistribution 1] setEndRate    $growthratio
            [$Con getDistribution 1] setEndLayers  $endlayers
        }
    
    }
    
    if [llength $errorMessages] {
        set allMessages [join [list $errorMessages $infoMessages]]
        set infoMessage [join $allMessages ""]
    } else {
        set infoMessage [join $infoMessages ""]
    }
    
    pw::Display update
}

############################################################################
# Done Logic
############################################################################
proc done {} {

    global Con PoleCon

    if [info exists Con] { $Con setRenderAttribute PointMode None }
    if [info exists PoleCon] { $PoleCon delete }

    exit
}

.f.bspce configure -background #EBAD99
.f.espce configure -background #EBAD99
.f.mspce configure -background #EBAD99
.f.gre   configure -background #EBAD99
#.f.gle   configure -background #EBAD99

.f.bspce configure -validate all -vcmd {validateParams %P %W}
.f.espce configure -validate all -vcmd {validateParams %P %W}
.f.mspce configure -validate all -vcmd {validateParams %P %W}
.f.gre   configure -validate all -vcmd {validateParams %P %W}
.f.gle   configure -validate all -vcmd {validateParams %P %W}

updateStates

##################################################################################################
# Attempt to retrieve any connectors that were selected before script started
##################################################################################################

pw::Display getSelectedEntities ents
if { [llength $ents(Connectors)] == 1} {
  set Con $ents(Connectors)
  pickCon true
}

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
