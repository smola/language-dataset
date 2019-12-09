#
# Copyright 2011 (c) Pointwise, Inc.
# All rights reserved.
# 
# This sample Pointwise script is not supported by Pointwise, Inc.
# It is provided freely for demonstration purposes only.  
# SEE THE WARRANTY DISCLAIMER AT THE BOTTOM OF THIS FILE.
#

# ===============================================
# HYBRID MESH GENERATION SCRIPT FOR A
# VERTICAL AXIS WIND TURBINE
# ===============================================
# Written by Travis Carrigan
#


# Initialization
package require PWI_Glyph 2.4
pw::Script loadTk
pw::Application reset
pw::Application setUndoMaximumLevels 10

# Directory from which script is run
set cwd [file dirname [info script]]


# AIRFOIL GUI CREATION
# -----------------------------------------------
wm title . "VAWT Mesh Generator"
grid [ttk::frame .c -padding "5 5 5 5"] -column 0 -row 0 -columnspan 1 -sticky nwes
grid columnconfigure . 0 -weight 1; grid rowconfigure . 0 -weight 1

# Default airfoil parameters
set naca    0015
set afsol   1.5
set numblds 3

# Create notebook
grid [ttk::notebook .c.nb -padding "5 5 5 5"] -column 0 -row 0 -columnspan 2
    .c.nb add [ttk::frame .c.nb.f1 -padding "5 5 5 5"] -text "Step 1"
    .c.nb add [ttk::frame .c.nb.f2 -padding "5 5 5 5"] -text "Step 2"
    .c.nb add [ttk::frame .c.nb.f3 -padding "5 5 5 5"] -text "Step 3"

# Airfoil labels
grid [ttk::label .c.nb.f1.l1 -text "VAWT Airfoil Generator" -font {-underline 1}] -column 0 -row 0 -columnspan 2 -stick w
grid [ttk::label .c.nb.f1.nacal    -text "NACA 4-Series Cross-Section" -width 25] -column 0 -row 1 -sticky w
grid [ttk::label .c.nb.f1.afsoll   -text "Wind Turbine Solidity"       -width 25] -column 0 -row 2 -sticky w
grid [ttk::label .c.nb.f1.numbldsl -text "Number of Blades"            -width 25] -column 0 -row 3 -sticky w

# Airfoil entry boxes
grid [ttk::entry  .c.nb.f1.nacae    -width 8 -textvariable naca     ] -column 1 -row 1 -sticky e
grid [ttk::entry  .c.nb.f1.afsole   -width 8 -textvariable afsol    ] -column 1 -row 2 -sticky e
grid [ttk::entry  .c.nb.f1.numbldse -width 8 -textvariable numblds  ] -column 1 -row 3 -sticky e
grid [ttk::button .c.nb.f1.gob      -text "Create" -command coordGen] -column 1 -row 4 -sticky e

# Default boundary layer parameters
set initds 0.0001
set cellgr 1.2
set bldist 0.2
set numpts 100

# Boundary layer labels
grid [ttk::label .c.nb.f2.l2 -text "Boundary Layer Parameters" -font {-underline 1}] -column 0 -row 0 -columnspan 2 -sticky w
grid [ttk::label .c.nb.f2.initdsl   -text "Initial Cell Height"   -width 25] -column 0 -row 1 -sticky w
grid [ttk::label .c.nb.f2.cellgrl   -text "Cell Growth Rate"      -width 25] -column 0 -row 2 -sticky w
grid [ttk::label .c.nb.f2.numlayerl -text "Boundary Layer Height" -width 25] -column 0 -row 3 -sticky w
grid [ttk::label .c.nb.f2.numptsl   -text "Points Around Airfoil" -width 25] -column 0 -row 4 -sticky w

# Boundary layer entry boxes
grid [ttk::entry  .c.nb.f2.initdse   -width 8 -textvariable initds ] -column 1 -row 1 -sticky e
grid [ttk::entry  .c.nb.f2.cellgre   -width 8 -textvariable cellgr ] -column 1 -row 2 -sticky e
grid [ttk::entry  .c.nb.f2.numlayere -width 8 -textvariable bldist ] -column 1 -row 3 -sticky e
grid [ttk::entry  .c.nb.f2.numptse   -width 8 -textvariable numpts ] -column 1 -row 4 -sticky e
grid [ttk::button .c.nb.f2.gob       -text "Create" -command blMesh] -column 1 -row 5 -sticky e

# Default farfield parameters
set rotdomdia 50
set rotdomdim 40
set ffdomdia  100
set ffdomdim  50

# Farfield labels
grid [ttk::label .c.nb.f3.l3 -text "Farfield Boundary Parameters" -font {-underline 1}] -column 0 -row 0 -columnspan 2 -sticky w
grid [ttk::label .c.nb.f3.rotdomdial -text "Rotational Domain Diameter" -width 25] -column 0 -row 1 -sticky w
grid [ttk::label .c.nb.f3.rotdomdiml -text "Rotational Domain Points"   -width 25] -column 0 -row 2 -sticky w
grid [ttk::label .c.nb.f3.ffdomdial  -text "Farfield Domain Diameter"   -width 25] -column 0 -row 3 -sticky w
grid [ttk::label .c.nb.f3.ffdomdiml  -text "Farfield Domain Points"     -width 25] -column 0 -row 4 -sticky w

# Farfield entry boxes
grid [ttk::entry  .c.nb.f3.rotdomdiae -width 8 -textvariable rotdomdia] -column 1 -row 1 -sticky e
grid [ttk::entry  .c.nb.f3.rotdomdime -width 8 -textvariable rotdomdim] -column 1 -row 2 -sticky e
grid [ttk::entry  .c.nb.f3.ffdomdiae  -width 8 -textvariable ffdomdia ] -column 1 -row 3 -sticky e
grid [ttk::entry  .c.nb.f3.ffdomdime  -width 8 -textvariable ffdomdim ] -column 1 -row 4 -sticky e
grid [ttk::button .c.nb.f3.gob        -text "Create" -command ffMesh  ] -column 1 -row 5 -sticky e

# Notes
grid [ttk::labelframe .c.lf4 -padding "5 5 5 5" -text "Notes"] -column 0 -row 3 -columnspan 2
grid [ttk::label      .c.lf4.l -width 25] -column 0 -row 0
grid [tk::text .c.lf4.t -width 29 -height 8 -wrap word] -column 0 -row 0 -columnspan 2
.c.lf4.t insert 1.0 "The initial cell height and boundary layer height are factors of the airfoil chord length.\n\nAll farfield boundary dimensions are factors of the VAWT radius (R = 1)."
.c.lf4.t configure -state disabled

# Restart and done button
grid [ttk::button .c.lf4.und -text "Undo"    -command undo] -column 0 -row 4 -sticky w
grid [ttk::button .c.lf4.res -text "Restart" -command rest] -column 0 -row 4 -columnspan 2 
grid [ttk::button .c.lf4.gob -text "Done"    -command exit] -column 1 -row 4 -sticky e

# Clean up spacing
foreach w [winfo children .c    ]   {grid configure $w -padx 5 -pady 5}
foreach w [winfo children .c.nb.f1] {grid configure $w -padx 5 -pady 5}
foreach w [winfo children .c.nb.f2] {grid configure $w -padx 5 -pady 5}
foreach w [winfo children .c.nb.f3] {grid configure $w -padx 5 -pady 5}
foreach w [winfo children .c.lf4]   {grid configure $w -padx 5 -pady 5}

focus .c.nb.f1.nacae
::tk::PlaceWindow . widget


# PROCEDURE FOR GENERATING AIRFOIL COORDINATES
# -----------------------------------------------
proc coordGen {} {

# AIRFOIL INPUTS
# -----------------------------------------------
# m = maximum camber 
# p = maximum camber location 
# t = maximum thickness
set m [expr {[string index $::naca 0]/100.0}]  
set p [expr {[string index $::naca 1]/10.0}] 
set a [string index $::naca 2]
set b [string index $::naca 3]
set c "$a$b"
set t [expr {$c/100.0}]

# GENERATE AIRFOIL COORDINATES
# -----------------------------------------------
# Initialize Arrays
set x {}
set xu {}
set xl {}
set yu {}
set yl {}
set yc {0}
set yt {}

# Airfoil step size
set ds 0.001

# Check if airfoil is symmetric or cambered
if {$m == 0 && $p == 0 || $m == 0 || $p == 0} {set symm 1} else {set symm 0}

# Get x coordinates
for {set i 0} {$i < [expr {1+$ds}]} {set i [expr {$i+$ds}]} {lappend x $i}

# Calculate mean camber line and thickness distribution
foreach xx $x {

	# Mean camber line definition for symmetric geometry
	if {$symm == 1} {lappend yc 0}

	# Mean camber line definition for cambered geometry
	if {$symm == 0 && $xx <= $p} {
		lappend yc [expr {($m/($p**2))*(2*$p*$xx-$xx**2)}]
	} elseif {$symm == 0 && $xx > $p} {
		lappend yc [expr {($m/((1-$p)**2)*(1-2*$p+2*$p*$xx-$xx**2))}]
	}

	# Thickness distribution
	lappend yt [expr {($t/0.20)*(0.29690*sqrt($xx)-0.12600*$xx- \
	                  0.35160*$xx**2+0.28430*$xx**3-0.10150*$xx**4)}]

	# Theta
	set dy [expr {[lindex $yc end] - [lindex $yc end-1]}]
	set th [expr {atan($dy/$ds)}]

	# Upper x and y coordinates
	lappend xu [expr {$xx-[lindex $yt end]*sin($th)}]
	lappend yu [expr {[lindex $yc end]+[lindex $yt end]*cos($th)}]

	# Lower x and y coordinates
	lappend xl [expr {$xx+[lindex $yt end]*sin($th)}]
	lappend yl [expr {[lindex $yc end]-[lindex $yt end]*cos($th)}]

}

# GENERATE AIRFOIL GEOMETRY
# -----------------------------------------------
# Create upper airfoil surface
set airUpper [pw::Application begin Create]
set airUpperPts [pw::SegmentSpline create]

for {set i 0} {$i < [llength $x]} {incr i} {
	$airUpperPts addPoint [list [lindex $xu $i] [lindex $yu $i] 0]
}

set airUpperCurve [pw::Curve create]
	$airUpperCurve addSegment $airUpperPts
$airUpper end

# Create lower airfoil surface
set airLower [pw::Application begin Create]
set airLowerPts [pw::SegmentSpline create]

for {set i 0} {$i < [llength $x]} {incr i} {
	$airLowerPts addPoint [list [lindex $xl $i] [lindex $yl $i] 0]
}

set airLowerCurve [pw::Curve create]
	$airLowerCurve addSegment $airLowerPts
$airLower end

# Create flat trailing edge
set airTrail [pw::Application begin Create]
set airTrailPts [pw::SegmentSpline create]
	$airTrailPts addPoint [list [lindex $xu end] [lindex $yu end] 0]
	$airTrailPts addPoint [list [lindex $xl end] [lindex $yl end] 0]
set airTrailCurve [pw::Curve create]
	$airTrailCurve addSegment $airTrailPts
$airTrail end

# Scale airfoil based on solidity
set afSol   $::afsol
set numBlds $::numblds
set scale   [expr ($afSol*2)/$numBlds]
set afdb    [pw::Database getAll]

pw::Entity transform [pwu::Transform scaling -anchor {0 0 0} \
[list "$scale" "$scale" "$scale"]] $afdb

# Mark and undo level
pw::Application markUndoLevel {coord}

# Zoom to airfoil
pw::Display resetView

}


# PROCEDURE FOR GENERATING BOUNDARY LAYER MESH
# -----------------------------------------------
proc blMesh {} {

# BOUNDARY LAYER INPUTS
# -----------------------------------------------
# afSol    = airfoil solidity
# numbBlds = number of blades
# chord    = airfoil chord length
# initDs   = initial cell height
# cellGr   = cell growth rate
# blDist   = boundary layer distance
# numPts   = number of points around airfoil
set afSol   $::afsol
set numBlds $::numblds
set chord   [expr ($afSol*2)/$numBlds]
set initDs  [expr $::initds*$chord]
set cellGr  $::cellgr
set blDist  [expr $::bldist*$chord]
set numPts  $::numpts

# CONNECTOR CREATION, DIMENSIONING, AND SPACING
# -----------------------------------------------
# Get all database entities
set dbEnts [pw::Database getAll]

# Create connectors on database entities
set cons [pw::Connector createOnDatabase $dbEnts]
set upperSurfCon [lindex $cons 0]
set lowerSurfCon [lindex $cons 1]
set trailSurfCon [lindex $cons 2]

# Calculate main airfoil connector dimensions
foreach con $cons {lappend conLen [$con getLength -arc 1]}
set upperSurfConLen [lindex $conLen 0]
set lowerSurfConLen [lindex $conLen 1]
set trailSurfConLen [lindex $conLen 2]
set conDim [expr int($numPts/2)]

# Dimension upper and lower airfoil surface connectors
$upperSurfCon setDimension $conDim
$lowerSurfCon setDimension $conDim

# Dimension trailing edge airfoil connector
set teDim [expr int($trailSurfConLen/(10*$initDs))+2]
$trailSurfCon setDimension $teDim

# Set leading and trailing edge connector spacings
set ltDs [expr 10*$initDs]

set upperSurfConDis [$upperSurfCon getDistribution 1]
set lowerSurfConDis [$lowerSurfCon getDistribution 1]
set trailSurfConDis [$trailSurfCon getDistribution 1]

$upperSurfConDis setBeginSpacing $ltDs
$upperSurfConDis setEndSpacing $ltDs
$lowerSurfConDis setBeginSpacing $ltDs
$lowerSurfConDis setEndSpacing $ltDs

# Create edges for structured boundary layer extrusion
set afEdge [pw::Edge createFromConnectors -single $cons]
set afDom [pw::DomainStructured create]
$afDom addEdge $afEdge

# Extrude boundary layer using normal hyperbolic extrusion method
set afExtrude [pw::Application begin ExtrusionSolver $afDom]
	$afDom setExtrusionSolverAttribute NormalInitialStepSize $initDs
	$afDom setExtrusionSolverAttribute SpacingGrowthFactor $cellGr
	$afDom setExtrusionSolverAttribute NormalMarchingVector {0 0 -1}
	$afDom setExtrusionSolverAttribute NormalKinseyBarthSmoothing 3
	$afDom setExtrusionSolverAttribute NormalVolumeSmoothing 0.3
	$afDom setExtrusionSolverAttribute StopAtHeight $blDist
	$afExtrude run 1000
$afExtrude end

# CREATE THE THREE BLADES OF THE VAWT
# -----------------------------------------------
# Calculate half chord
set hlfChord [expr $chord/2]

# Cut, paste, translate blade to desired radius
set gridEnts [pw::Grid getAll]
set afEnts [join [list $dbEnts $gridEnts]]
pw::Application setClipboard $afEnts
set rotPt [list [expr -$hlfChord] 1 0]
pw::Entity transform [pwu::Transform translation $rotPt] $afEnts
pw::Application clearClipboard

# Copy, paste, rotate to create blades
for {set i 1} {$i < $numBlds} {incr i} {

	set rotAngle [expr 360/$numBlds]

	pw::Application setClipboard $afEnts
	set afMeshRot [pw::Application begin Paste]
	set afEntsRot [$afMeshRot getEntities]
	set afBegRot [pw::Application begin Modify $afEntsRot]
	pw::Entity transform [pwu::Transform rotation -anchor {0 0 0} {0 0 1} \
                             [expr $i*$rotAngle]] [$afBegRot getEntities]
	$afBegRot end
	$afMeshRot end

	pw::Application clearClipboard

}

# Mark and undo level
pw::Application markUndoLevel {bl}

# Zoom to blades
pw::Display resetView

}


# PROCEDURE FOR GENERATING FARFIELD MESH
# -----------------------------------------------
proc ffMesh {} {

# FARFIELD INPUTS
# -----------------------------------------------
# numBlds   = number of blades
# rotDomDia = rotational domain diameter
# rotDomDim = points around rotational domain
# ffDomDia  = farfield domain diameter
# ffDomDim  = points around farfield domain
set numBlds   $::numblds
set rotDomDia $::rotdomdia
set rotDomDim $::rotdomdim
set ffDomDia  $::ffdomdia
set ffDomDim  $::ffdomdim

# CREATE ROTATIONAL DOMAIN
# -----------------------------------------------
# Create inner circle connectors
set createInnerCircle [pw::Application begin Create]
set innerCircle [pw::SegmentCircle create]
	$innerCircle addPoint [list [expr $rotDomDia/2] 0 0]
	$innerCircle addPoint {0 0 0}
	$innerCircle setEndAngle 360 {0 0 1}
set innerCircleCon [pw::Connector create]
	$innerCircleCon addSegment $innerCircle
	$createInnerCircle end

# Split circle connector at midpoint
set innerCircleConSplit [$innerCircleCon split 0.5]

# Dimension connectors
set innerCircleCon1 [lindex $innerCircleConSplit 0]
set innerCircleCon2 [lindex $innerCircleConSplit 1]

$innerCircleCon1 setDimension $rotDomDim
$innerCircleCon2 setDimension $rotDomDim

# Create interior rotational domain
set gridEnts [pw::Grid getAll]
foreach ent $gridEnts {
        if {[$ent isOfType pw::DomainStructured]} {
                lappend blCons [[$ent getEdge JMaximum] getConnector 1]
        }
}

set createInnerDom [pw::Application begin Create]
set innerDomCircleEdge [pw::Edge create]
	$innerDomCircleEdge addConnector $innerCircleCon1
	$innerDomCircleEdge addConnector $innerCircleCon2

for {set i 0} {$i < $numBlds} {incr i} {
	set innerDomBladeEdge($i) [pw::Edge create]
	$innerDomBladeEdge($i) addConnector [lindex $blCons $i]
}

set innerDom [pw::DomainUnstructured create]
	$innerDom addEdge $innerDomCircleEdge
	for {set i 0} {$i < $numBlds} {incr i} {$innerDom addEdge $innerDomBladeEdge($i)}

$createInnerDom end

set innerDomSolve [pw::Application begin UnstructuredSolver $innerDom]
	$innerDom setUnstructuredSolverAttribute BoundaryDecay 0.985
	$innerDomSolve run Initialize
$innerDomSolve end

# CREATE FARFIELD DOMAIN
# -----------------------------------------------
# Create outer circle connectors
set createOuterCircle [pw::Application begin Create]
set outerCircle [pw::SegmentCircle create]
	$outerCircle addPoint [list 0 [expr $ffDomDia/2] 0]
	$outerCircle addPoint {0 0 0}
	$outerCircle setEndAngle 360 {0 0 1}
set outerCircleCon [pw::Connector create]
	$outerCircleCon addSegment $outerCircle
$createOuterCircle end

# Split connector at midpoint
set outerCircleConSplit [$outerCircleCon split 0.5]

# Dimension connectors
set outerCircleCon1 [lindex $outerCircleConSplit 0]
set outerCircleCon2 [lindex $outerCircleConSplit 1]

$outerCircleCon1 setDimension $ffDomDim
$outerCircleCon2 setDimension $ffDomDim

# Create outer domains inner overlapping connectors
set createInnerOverCircle [pw::Application begin Create]
set innerOverCircle [pw::SegmentCircle create]
	$innerOverCircle addPoint [list 0 [expr $rotDomDia/2] 0]
	$innerOverCircle addPoint {0 0 0}
	$innerOverCircle setEndAngle 360 {0 0 1}
set innerOverCircleCon [pw::Connector create]
	$innerOverCircleCon addSegment $innerOverCircle
$createInnerOverCircle end

# Split connector at midpoint
set innerOverCircleConSplit [$innerOverCircleCon split 0.5]

# Dimension connectors
set innerOverCircleCon1 [lindex $innerOverCircleConSplit 0]
set innerOverCircleCon2 [lindex $innerOverCircleConSplit 1]

$innerOverCircleCon1 setDimension $rotDomDim
$innerOverCircleCon2 setDimension $rotDomDim

# Create farfield domain
set createOuterDom [pw::Application begin Create]
set outerDomOuterEdge [pw::Edge create]
	$outerDomOuterEdge addConnector $outerCircleCon1
	$outerDomOuterEdge addConnector $outerCircleCon2

set outerDomInnerEdge [pw::Edge create]
	$outerDomInnerEdge addConnector $innerOverCircleCon1
	$outerDomInnerEdge addConnector $innerOverCircleCon2
$outerDomInnerEdge reverse

set outerDom [pw::DomainUnstructured create]
	$outerDom addEdge $outerDomOuterEdge
	$outerDom addEdge $outerDomInnerEdge
$createOuterDom end

set outerDomSolve [pw::Application begin UnstructuredSolver $outerDom]
	$outerDom setUnstructuredSolverAttribute BoundaryDecay 0.985
	$outerDomSolve run Initialize
$outerDomSolve end

# Mark and undo level
pw::Application markUndoLevel {ff}

# Zoom out
pw::Display resetView

}


# PROCEDURE FOR UNDOING LAST ACTION
# -----------------------------------------------
proc undo {} {

pw::Application undo

pw::Display resetView

}


# PROCEDURE FOR RESTARTING POINTWISE
# -----------------------------------------------
proc rest {} {

pw::Application reset

pw::Display resetView

}





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
