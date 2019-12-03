#------------------
# Initial settings
#------------------
package require PWI_Glyph 2.17.0
pw::Application setUndoMaximumLevels 5
pw::Application reset
pw::Application markUndoLevel {Journal Reset}
pw::Application clearModified

#--------------------
# Set solver to CGNS
#--------------------
pw::Application setCAESolver {CGNS} 3

#----------------------------------------
# Constants (parameters) for this script
#----------------------------------------
set PI            3.14159265359
set W             1.0   

set L1            1.0
set L2            4.0

set H1            0.5
set H2            0.5

# Resolution in number of cells
set NC1           20
set NC2           80
set NCW            5
set NCH           10

source "delnov.glf"

#---------------
# Define points 
#---------------

# Floor (z min)
Delnov_Create_Point 0                 0   0
Delnov_Create_Point $L1               0   0
Delnov_Create_Point [expr $L1 + $L2]  0   0
Delnov_Create_Point 0                 $W  0
Delnov_Create_Point $L1               $W  0
Delnov_Create_Point [expr $L1 + $L2]  $W  0

# Middle
Delnov_Create_Point 0                 0   $H1
Delnov_Create_Point $L1               0   $H1
Delnov_Create_Point [expr $L1 + $L2]  0   $H1
Delnov_Create_Point 0                 $W  $H1
Delnov_Create_Point $L1               $W  $H1
Delnov_Create_Point [expr $L1 + $L2]  $W  $H1

# Top (z max)
Delnov_Create_Point 0                 0   [expr $H1 + $H2]
Delnov_Create_Point $L1               0   [expr $H1 + $H2]
Delnov_Create_Point [expr $L1 + $L2]  0   [expr $H1 + $H2]
Delnov_Create_Point 0                 $W  [expr $H1 + $H2]
Delnov_Create_Point $L1               $W  [expr $H1 + $H2]
Delnov_Create_Point [expr $L1 + $L2]  $W  [expr $H1 + $H2]

puts "Defined all the points"

#-----------------
# Define segments 
#-----------------

# Floor (z min)
Delnov_Create_Line_From_Points_Name "point-1"  "point-2"  "line-f-01-02"
Delnov_Create_Line_From_Points_Name "point-2"  "point-3"  "line-f-02-03"
Delnov_Create_Line_From_Points_Name "point-4"  "point-5"  "line-f-04-05"
Delnov_Create_Line_From_Points_Name "point-5"  "point-6"  "line-f-05-06"
Delnov_Create_Line_From_Points_Name "point-1"  "point-4"  "line-f-01-04"
Delnov_Create_Line_From_Points_Name "point-2"  "point-5"  "line-f-02-05"
Delnov_Create_Line_From_Points_Name "point-3"  "point-6"  "line-f-03-06"

# Middle 
Delnov_Create_Line_From_Points_Name "point-7"   "point-8"   "line-m-01-02"
Delnov_Create_Line_From_Points_Name "point-8"   "point-9"   "line-m-02-03"
Delnov_Create_Line_From_Points_Name "point-10"  "point-11"  "line-m-04-05"
Delnov_Create_Line_From_Points_Name "point-11"  "point-12"  "line-m-05-06"
Delnov_Create_Line_From_Points_Name "point-7"   "point-10"  "line-m-01-04"
Delnov_Create_Line_From_Points_Name "point-8"   "point-11"  "line-m-02-05"
Delnov_Create_Line_From_Points_Name "point-9"   "point-12"  "line-m-03-06"

# Top floor
Delnov_Create_Line_From_Points_Name "point-13"  "point-14"  "line-t-01-02"
Delnov_Create_Line_From_Points_Name "point-14"  "point-15"  "line-t-02-03"
Delnov_Create_Line_From_Points_Name "point-16"  "point-17"  "line-t-04-05"
Delnov_Create_Line_From_Points_Name "point-17"  "point-18"  "line-t-05-06"
Delnov_Create_Line_From_Points_Name "point-13"  "point-16"  "line-t-01-04"
Delnov_Create_Line_From_Points_Name "point-14"  "point-17"  "line-t-02-05"
Delnov_Create_Line_From_Points_Name "point-15"  "point-18"  "line-t-03-06"

# Floor to middle
Delnov_Create_Line_From_Points_Name "point-1"  "point-7"   "line-fm-01"
Delnov_Create_Line_From_Points_Name "point-2"  "point-8"   "line-fm-02"
Delnov_Create_Line_From_Points_Name "point-3"  "point-9"   "line-fm-03"
Delnov_Create_Line_From_Points_Name "point-4"  "point-10"  "line-fm-04"
Delnov_Create_Line_From_Points_Name "point-5"  "point-11"  "line-fm-05"
Delnov_Create_Line_From_Points_Name "point-6"  "point-12"  "line-fm-06"

# Middle to top
Delnov_Create_Line_From_Points_Name "point-7"   "point-13"  "line-mt-01"
Delnov_Create_Line_From_Points_Name "point-8"   "point-14"  "line-mt-02"
Delnov_Create_Line_From_Points_Name "point-9"   "point-15"  "line-mt-03"
Delnov_Create_Line_From_Points_Name "point-10"  "point-16"  "line-mt-04"
Delnov_Create_Line_From_Points_Name "point-11"  "point-17"  "line-mt-05"
Delnov_Create_Line_From_Points_Name "point-12"  "point-18"  "line-mt-06"

#------------------
# Creating domains
#------------------

# Floor (z min)
Delnov_Create_Structured_Domain_Name "line-f-01-02"  \
                                     "line-f-02-05"  \
                                     "line-f-04-05"  \
                                     "line-f-01-04"  \
                                     "quad-zmin-1"
Delnov_Create_Structured_Domain_Name "line-f-02-03"  \
                                     "line-f-03-06"  \
                                     "line-f-05-06"  \
                                     "line-f-02-05"  \
                                     "quad-zmin-2"

# Middle
Delnov_Create_Structured_Domain_Name "line-m-01-02"  \
                                     "line-m-02-05"  \
                                     "line-m-04-05"  \
                                     "line-m-01-04"  \
                                     "quad-zmid-1"
Delnov_Create_Structured_Domain_Name "line-m-02-03"  \
                                     "line-m-03-06"  \
                                     "line-m-05-06"  \
                                     "line-m-02-05"  \
                                     "quad-zmid-2"

# Top (z max)
Delnov_Create_Structured_Domain_Name "line-t-01-02"  \
                                     "line-t-02-05"  \
                                     "line-t-04-05"  \
                                     "line-t-01-04"  \
                                     "quad-zmax-1"
Delnov_Create_Structured_Domain_Name "line-t-02-03"  \
                                     "line-t-03-06"  \
                                     "line-t-05-06"  \
                                     "line-t-02-05"  \
                                     "quad-zmax-2"

# Left (x min)
Delnov_Create_Structured_Domain_Name "line-f-01-04"  \
                                     "line-fm-01"    \
                                     "line-m-01-04"  \
                                     "line-fm-04"    \
                                     "quad-xmin-1"
Delnov_Create_Structured_Domain_Name "line-m-01-04"  \
                                     "line-mt-01"    \
                                     "line-t-01-04"  \
                                     "line-mt-04"    \
                                     "quad-xmin-2"

# Middle
Delnov_Create_Structured_Domain_Name "line-f-02-05"  \
                                     "line-fm-02"    \
                                     "line-m-02-05"  \
                                     "line-fm-05"    \
                                     "quad-xmed-1"
Delnov_Create_Structured_Domain_Name "line-m-02-05"  \
                                     "line-mt-02"    \
                                     "line-t-02-05"  \
                                     "line-mt-05"    \
                                     "quad-xmed-2"

# Right (x max)
Delnov_Create_Structured_Domain_Name "line-f-03-06"  \
                                     "line-fm-03"    \
                                     "line-m-03-06"  \
                                     "line-fm-06"    \
                                     "quad-xmax-1"
Delnov_Create_Structured_Domain_Name "line-m-03-06"  \
                                     "line-mt-03"    \
                                     "line-t-03-06"  \
                                     "line-mt-06"    \
                                     "quad-xmax-2"

# Front (y min)
Delnov_Create_Structured_Domain_Name "line-f-01-02"  \
                                     "line-fm-02"    \
                                     "line-m-01-02"  \
                                     "line-fm-01"    \
                                     "quad-ymin-1"
Delnov_Create_Structured_Domain_Name "line-f-02-03"  \
                                     "line-fm-03"    \
                                     "line-m-02-03"  \
                                     "line-fm-02"    \
                                     "quad-ymin-2"
Delnov_Create_Structured_Domain_Name "line-m-01-02"  \
                                     "line-mt-02"    \
                                     "line-t-01-02"  \
                                     "line-mt-01"    \
                                     "quad-ymin-3"
Delnov_Create_Structured_Domain_Name "line-m-02-03"  \
                                     "line-mt-03"    \
                                     "line-t-02-03"  \
                                     "line-mt-02"    \
                                     "quad-ymin-4"

# Back (y max)
Delnov_Create_Structured_Domain_Name "line-f-04-05"  \
                                     "line-fm-05"    \
                                     "line-m-04-05"  \
                                     "line-fm-04"    \
                                     "quad-ymax-1"
Delnov_Create_Structured_Domain_Name "line-f-05-06"  \
                                     "line-fm-06"    \
                                     "line-m-05-06"  \
                                     "line-fm-05"    \
                                     "quad-ymax-2"
Delnov_Create_Structured_Domain_Name "line-m-04-05"  \
                                     "line-mt-05"    \
                                     "line-t-04-05"  \
                                     "line-mt-04"    \
                                     "quad-ymax-3"
Delnov_Create_Structured_Domain_Name "line-m-05-06"  \
                                     "line-mt-06"    \
                                     "line-t-05-06"  \
                                     "line-mt-05"    \
                                     "quad-ymax-4"

puts "Defined domains"

#--------------------------------------
# Define resolution on all connections
#--------------------------------------

# Resolution before the obstacle
Delnov_Modify_Dimension_By_Name_Pattern "line-f-01-02" [expr $NC1 + 1]

# Resolution after the obstacle
Delnov_Modify_Dimension_By_Name_Pattern "line-f-02-03" [expr $NC2 + 1]

# Spanwise resolution
Delnov_Modify_Dimension_By_Name_Pattern "line-f-01-04" [expr $NCW + 1]

# Normal to lower wall resolution
Delnov_Modify_Dimension_By_Name_Pattern "line-fm-01" [expr $NCH + 1]
Delnov_Modify_Dimension_By_Name_Pattern "line-mt-01" [expr $NCH + 1]

#---------------
# Create blocks
#---------------
Delnov_Create_Structured_Block [list "quad-xmin-1" "quad-xmed-1"  \
                                     "quad-ymin-1" "quad-ymax-1"  \
                                     "quad-zmin-1" "quad-zmid-1"]

Delnov_Create_Structured_Block [list "quad-xmed-1" "quad-xmax-1"  \
                                     "quad-ymin-2" "quad-ymax-2"  \
                                     "quad-zmin-2" "quad-zmid-2"]

Delnov_Create_Structured_Block [list "quad-xmin-2" "quad-xmed-2"  \
                                     "quad-ymin-3" "quad-ymax-3"  \
                                     "quad-zmid-1" "quad-zmax-1"]

Delnov_Create_Structured_Block [list "quad-xmed-2" "quad-xmax-2"  \
                                     "quad-ymin-4" "quad-ymax-4"  \
                                     "quad-zmid-2" "quad-zmax-2"]

puts "Defined blocks"

#-----------------------------
# Specify boundary conditions 
#-----------------------------
Delnov_Introduce_Bnd_Conds [list "bottom_wall" "upper_wall"  \
                                 "inlet"       "outlet"      \
                                 "period"      "plate"]

set bc [pw::BoundaryCondition getByName "bottom_wall"]
$bc apply [list [pw::GridEntity getByName "blk-1"]        \
                [pw::GridEntity getByName "quad-zmin-1"]  ]
$bc apply [list [pw::GridEntity getByName "blk-2"]        \
                [pw::GridEntity getByName "quad-zmin-2"]  ]

set bc [pw::BoundaryCondition getByName "upper_wall"]
$bc apply [list [pw::GridEntity getByName "blk-3"]        \
                [pw::GridEntity getByName "quad-zmax-1"]  ]
$bc apply [list [pw::GridEntity getByName "blk-4"]        \
                [pw::GridEntity getByName "quad-zmax-2"]  ]

set bc [pw::BoundaryCondition getByName "inlet"]
$bc apply [list [pw::GridEntity getByName "blk-1"]        \
                [pw::GridEntity getByName "quad-xmin-1"]  ]
$bc apply [list [pw::GridEntity getByName "blk-3"]        \
                [pw::GridEntity getByName "quad-xmin-2"]  ]

set bc [pw::BoundaryCondition getByName "outlet"]
$bc apply [list [pw::GridEntity getByName "blk-2"]        \
                [pw::GridEntity getByName "quad-xmax-1"]  ]
$bc apply [list [pw::GridEntity getByName "blk-4"]        \
                [pw::GridEntity getByName "quad-xmax-2"]  ]

set bc [pw::BoundaryCondition getByName "period"]
$bc apply [list [pw::GridEntity getByName "blk-1"]        \
                [pw::GridEntity getByName "quad-ymin-1"]  ]
$bc apply [list [pw::GridEntity getByName "blk-1"]        \
                [pw::GridEntity getByName "quad-ymax-1"]  ]
$bc apply [list [pw::GridEntity getByName "blk-2"]        \
                [pw::GridEntity getByName "quad-ymin-2"]  ]
$bc apply [list [pw::GridEntity getByName "blk-2"]        \
                [pw::GridEntity getByName "quad-ymax-2"]  ]
$bc apply [list [pw::GridEntity getByName "blk-3"]        \
                [pw::GridEntity getByName "quad-ymin-3"]  ]
$bc apply [list [pw::GridEntity getByName "blk-3"]        \
                [pw::GridEntity getByName "quad-ymax-3"]  ]
$bc apply [list [pw::GridEntity getByName "blk-4"]        \
                [pw::GridEntity getByName "quad-ymin-4"]  ]
$bc apply [list [pw::GridEntity getByName "blk-4"]        \
                [pw::GridEntity getByName "quad-ymax-4"]  ]

set bc [pw::BoundaryCondition getByName "plate"]
$bc apply [list [pw::GridEntity getByName "blk-3"]        \
                [pw::GridEntity getByName "quad-xmed-2"]  ]
$bc apply [list [pw::GridEntity getByName "blk-4"]        \
                [pw::GridEntity getByName "quad-xmed-2"]  ]

puts "Specified boundary conditions"

#------------------------
# Save data for analysis
#------------------------

# Select all the blocks ...
set blocks_only [Delnov_Get_Entities_By_Name_Pattern "blk"]

# ... and export them
set export [pw::Application begin CaeExport [pw::Entity sort $blocks_only]]
  $export initialize -type CAE {with_plate.cgns}
  $export setAttribute FilePrecision Double
  $export setAttribute GridStructuredAsUnstructured true
  $export setAttribute ExportParentElements true
  if {![$export verify]} {
    error "Data verification failed."
  }
  $export write
$export end
unset export

puts "Saved data for analysis"

