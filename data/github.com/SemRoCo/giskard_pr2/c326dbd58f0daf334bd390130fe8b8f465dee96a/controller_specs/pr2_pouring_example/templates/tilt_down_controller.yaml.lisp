#
# Copyright (C) 2015-2016 Georg Bartels <georg.bartels@cs.uni-bremen.de>
#
# This file is part of giskard.
#
# giskard is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#

scope:
  # definition of some nice short-cuts
  - unit_x: {vector3: [1, 0, 0]}
  - unit_y: {vector3: [0, 1, 0]}
  - unit_z: {vector3: [0, 0, 1]}


  # definition of joint input variables
  - torso_lift_joint: {input-var: 0}
  - l_shoulder_pan_joint: {input-var: 1}
  - l_shoulder_lift_joint: {input-var: 2}
  - l_upper_arm_roll_joint: {input-var: 3}
  - l_elbow_flex_joint: {input-var: 4}
  - l_forearm_roll_joint: {input-var: 5}
  - l_wrist_flex_joint: {input-var: 6}
  - l_wrist_roll_joint: {input-var: 7}
  - r_shoulder_pan_joint: {input-var: 8}
  - r_shoulder_lift_joint: {input-var: 9}
  - r_upper_arm_roll_joint: {input-var: 10}
  - r_elbow_flex_joint: {input-var: 11}
  - r_forearm_roll_joint: {input-var: 12}
  - r_wrist_flex_joint: {input-var: 13}
  - r_wrist_roll_joint: {input-var: 14}

  # definition of constraint ranges and centers
  - bottle_top_to_cup_top_goal_lower: {vector3: [~a, ~a, ~a]}
  - bottle_top_to_cup_top_goal_upper: {vector3: [~a, ~a, ~a]}
  - bottle_top_to_cup_top_goal_center: 
      vector-add: 
        - bottle_top_to_cup_top_goal_lower
        - {scale-vector: [0.5, {vector-sub: [bottle_top_to_cup_top_goal_upper, bottle_top_to_cup_top_goal_lower]}]}
  - bottle_to_itself_goal_lower: {vector3: [~a, ~a, ~a]}
  - bottle_to_itself_goal_upper: {vector3: [~a, ~a, ~a]}
  - bottle_to_itself_goal_center: 
      vector-add: 
        - bottle_to_itself_goal_lower
        - {scale-vector: [0.5, {vector-sub: [bottle_to_itself_goal_upper, bottle_to_itself_goal_lower]}]}
  - cup_top_to_base_goal_lower: {vector3: [0.4, 0.0, 1.05]}
  - cup_top_to_base_goal_upper: {vector3: [0.4, 0.0, 1.05]}
  - cup_top_to_base_goal_center: 
      vector-add: 
        - cup_top_to_base_goal_lower
        - {scale-vector: [0.5, {vector-sub: [cup_top_to_base_goal_upper, cup_top_to_base_goal_lower]}]}
  - cup_to_itself_goal_lower: {vector3: [-0.005, -0.005, 0.08]}
  - cup_to_itself_goal_upper: {vector3: [0.005, 0.005, 0.09]}
  - cup_to_itself_goal_center: 
      vector-add: 
        - cup_to_itself_goal_lower
        - {scale-vector: [0.5, {vector-sub: [cup_to_itself_goal_upper, cup_to_itself_goal_lower]}]}

  # definition of joint transforms
  - torso_lift:
      frame: [{axis-angle: [unit_x, 0]}, {vector3: [-0.05, 0, {double-add: [0.739675, torso_lift_joint]}]}]
  - l_shoulder_pan:
      frame: [{axis-angle: [unit_z, l_shoulder_pan_joint]}, {vector3: [0.0, 0.188, 0.0]}]
  - l_shoulder_lift:
      frame: [{axis-angle: [unit_y, l_shoulder_lift_joint]}, {vector3: [0.1, 0, 0]}]
  - l_upper_arm_roll:
      frame: [{axis-angle: [unit_x, l_upper_arm_roll_joint]}, {vector3: [0, 0, 0]}]
  - l_elbow_flex:
      frame: [{axis-angle: [unit_y, l_elbow_flex_joint]}, {vector3: [0.4, 0, 0]}]
  - l_forearm_roll:
      frame: [{axis-angle: [unit_x, l_forearm_roll_joint]}, {vector3: [0, 0, 0]}]
  - l_wrist_flex:
      frame: [{axis-angle: [unit_y, l_wrist_flex_joint]}, {vector3: [0.321, 0, 0]}]
  - l_wrist_roll:
      frame: [{axis-angle: [unit_x, l_wrist_roll_joint]}, {vector3: [0, 0, 0]}]
  - l_gripper_offset:
      frame: [{axis-angle: [unit_x, 0]}, {vector3: [0.2156, 0, 0]}]
  - r_shoulder_pan:
      frame: [{axis-angle: [unit_z, r_shoulder_pan_joint]}, {vector3: [0, -0.188, 0]}]
  - r_shoulder_lift:
      frame: [{axis-angle: [unit_y, r_shoulder_lift_joint]}, {vector3: [0.1, 0, 0]}]
  - r_upper_arm_roll: 
      frame: [{axis-angle: [unit_x, r_upper_arm_roll_joint]}, {vector3: [0, 0, 0]}]
  - r_elbow_flex:
      frame: [{axis-angle: [unit_y, r_elbow_flex_joint]}, {vector3: [0.4, 0, 0]}]
  - r_forearm_roll:
      frame: [{axis-angle: [unit_x, r_forearm_roll_joint]}, {vector3: [0, 0, 0]}]
  - r_wrist_flex:
      frame: [{axis-angle: [unit_y, r_wrist_flex_joint]}, {vector3: [0.321, 0, 0]}]
  - r_wrist_roll:
      frame: [{axis-angle: [unit_x, r_wrist_roll_joint]}, {vector3: [0, 0, 0]}]
  - r_gripper_offset:
      frame: [{axis-angle: [unit_x, 0]}, {vector3: [0.18, 0, 0]}]


  # definition of elbow FK
  - left_elbow:
      frame-mul:
      - torso_lift
      - l_shoulder_pan
      - l_shoulder_lift
      - l_upper_arm_roll
      - l_elbow_flex
  - right_elbow:
      frame-mul:
      - torso_lift
      - r_shoulder_pan
      - r_shoulder_lift
      - r_upper_arm_roll
      - r_elbow_flex

  # defintion of EE FK
  - left_ee:
      frame-mul:
      - left_elbow
      - l_forearm_roll
      - l_wrist_flex
      - l_wrist_roll
      - l_gripper_offset
  - right_ee:
      frame-mul:
      - right_elbow
      - r_forearm_roll
      - r_wrist_flex
      - r_wrist_roll
      - r_gripper_offset

  # definition of bottle and cup frame
  - cup_frame: 
      frame-mul:
        - left_ee
        - frame: [{quaternion: [0, 0, 0, 1]}, {vector3: [-0.01, 0, -0.07]}]
  - bottle_frame:
      frame-mul:
        - right_ee
        - frame: [{quaternion: [0, 0, 0, 1]}, {vector3: [-0.01, 0.0, -0.095]}]

  # definition of points on bottle
  - bottle_bottom: {origin-of: bottle_frame}
  - bottle_top: {transform-vector: [bottle_frame, {vector3: [0, 0, 0.185]}]}

  # definition of points on cup
  - cup_bottom: {origin-of: cup_frame}
  - cup_top: {transform-vector: [cup_frame, {vector3: [0, 0, 0.085]}]}

  # definition of features
  - bottle_top_to_cup_top:
      cached-vector: {vector-sub: [bottle_top, cup_top]}
  - bottle_to_itself:
      cached-vector: {vector-sub: [bottle_top, bottle_bottom]}
  - cup_to_itself:
      cached-vector: {vector-sub: [cup_top, cup_bottom]}

  # definition of error vectors
  - bottle_top_to_cup_top_lower_error_vector: {vector-sub: [bottle_top_to_cup_top_goal_lower, bottle_top_to_cup_top]}
  - bottle_top_to_cup_top_upper_error_vector: {vector-sub: [bottle_top_to_cup_top_goal_upper, bottle_top_to_cup_top]}
  - bottle_top_to_cup_top_center_error_vector: {vector-sub: [bottle_top_to_cup_top_goal_center, bottle_top_to_cup_top]}
  - bottle_to_itself_lower_error_vector: {vector-sub: [bottle_to_itself_goal_lower, bottle_to_itself]}
  - bottle_to_itself_upper_error_vector: {vector-sub: [bottle_to_itself_goal_upper, bottle_to_itself]}
  - bottle_to_itself_center_error_vector: {vector-sub: [bottle_to_itself_goal_center, bottle_to_itself]}
  - cup_top_to_base_lower_error_vector: {vector-sub: [cup_top_to_base_goal_lower, cup_top]}
  - cup_top_to_base_upper_error_vector: {vector-sub: [cup_top_to_base_goal_upper, cup_top]}
  - cup_top_to_base_center_error_vector: {vector-sub: [cup_top_to_base_goal_center, cup_top]}
  - cup_to_itself_lower_error_vector: {vector-sub: [cup_to_itself_goal_lower, cup_to_itself]}
  - cup_to_itself_upper_error_vector: {vector-sub: [cup_to_itself_goal_upper, cup_to_itself]}
  - cup_to_itself_center_error_vector: {vector-sub: [cup_to_itself_goal_center, cup_to_itself]}
  # definition of double errors
  - bottle_top_behind_cup_top_error: {abs: {x-coord: bottle_top_to_cup_top_center_error_vector}}
  - bottle_top_left_cup_top_error: {abs: {y-coord: bottle_top_to_cup_top_center_error_vector}} 
  - bottle_top_above_cup_top_error: {abs: {z-coord: bottle_top_to_cup_top_center_error_vector}}  
  - bottle_behind_itself_error: {abs: {x-coord: bottle_to_itself_center_error_vector}} 
  - bottle_left_itself_error: {abs: {y-coord: bottle_to_itself_center_error_vector}}  
  - bottle_above_itself_error: {abs: {z-coord: bottle_to_itself_center_error_vector}}  
  - cup_top_behind_base_error: {abs: {x-coord: cup_top_to_base_center_error_vector}}  
  - cup_top_left_base_error: {abs: {y-coord: cup_top_to_base_center_error_vector}}  
  - cup_top_above_base_error: {abs: {z-coord: cup_top_to_base_center_error_vector}}  
  - cup_behind_itself_error: {abs: {x-coord: cup_to_itself_center_error_vector}}  
  - cup_left_itself_error: {abs: {y-coord: cup_to_itself_center_error_vector}}  
  - cup_above_left_error: {abs: {z-coord: cup_to_itself_center_error_vector}}  

  # some constants
  - weight_arm_joints: 0.00001
  - weight_torso_joint: 0.001
  - weight_pos_control: 1.0
  - weight_range_control: 1.0
  - neg_vel_limit_left_arm_joints: -0.6
  - pos_vel_limit_left_arm_joints: 0.6
  - neg_vel_limit_right_arm_joints: -0.6
  - pos_vel_limit_right_arm_joints: 0.6
  - range_gain: 4
  - center_gain: 1
  - pos_gain: 1
  - rot_gain: 1
  - pos_thresh: 0.03
  - rot_thresh: 0.05

  # definition of control laws
  # bottle relative to cup
  - bottle_top_to_cup_top_lower_error_x_abs:
      abs: {x-coord: bottle_top_to_cup_top_lower_error_vector}
  - bottle_top_to_cup_top_lower_x_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_top_to_cup_top_lower_error_x_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_top_to_cup_top_lower_error_x_abs]}
  - bottle_top_to_cup_top_lower_x_control:
      double-mul: 
        - bottle_top_to_cup_top_lower_x_scaling
        - range_gain
        - pos_gain
        - {x-coord: bottle_top_to_cup_top_lower_error_vector}
  - bottle_top_to_cup_top_upper_error_x_abs:
      abs: {x-coord: bottle_top_to_cup_top_upper_error_vector}
  - bottle_top_to_cup_top_upper_x_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_top_to_cup_top_upper_error_x_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_top_to_cup_top_upper_error_x_abs]}
  - bottle_top_to_cup_top_upper_x_control:
      double-mul: 
        - bottle_top_to_cup_top_upper_x_scaling
        - range_gain
        - pos_gain
        - {x-coord: bottle_top_to_cup_top_upper_error_vector}
  - bottle_top_to_cup_top_lower_error_y_abs:
      abs: {y-coord: bottle_top_to_cup_top_lower_error_vector}
  - bottle_top_to_cup_top_lower_y_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_top_to_cup_top_lower_error_y_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_top_to_cup_top_lower_error_y_abs]}
  - bottle_top_to_cup_top_lower_y_control:
      double-mul: 
        - bottle_top_to_cup_top_lower_y_scaling
        - range_gain
        - pos_gain
        - {y-coord: bottle_top_to_cup_top_lower_error_vector}
  - bottle_top_to_cup_top_upper_error_y_abs:
      abs: {y-coord: bottle_top_to_cup_top_upper_error_vector}
  - bottle_top_to_cup_top_upper_y_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_top_to_cup_top_upper_error_y_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_top_to_cup_top_upper_error_y_abs]}
  - bottle_top_to_cup_top_upper_y_control:
      double-mul: 
        - bottle_top_to_cup_top_upper_y_scaling
        - range_gain
        - pos_gain
        - {y-coord: bottle_top_to_cup_top_upper_error_vector}
  - bottle_top_to_cup_top_lower_error_z_abs:
      abs: {z-coord: bottle_top_to_cup_top_lower_error_vector}
  - bottle_top_to_cup_top_lower_z_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_top_to_cup_top_lower_error_z_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_top_to_cup_top_lower_error_z_abs]}
  - bottle_top_to_cup_top_lower_z_control:
      double-mul: 
        - bottle_top_to_cup_top_lower_z_scaling
        - range_gain
        - pos_gain
        - {z-coord: bottle_top_to_cup_top_lower_error_vector}
  - bottle_top_to_cup_top_upper_error_z_abs:
      abs: {z-coord: bottle_top_to_cup_top_upper_error_vector}
  - bottle_top_to_cup_top_upper_z_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_top_to_cup_top_upper_error_z_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_top_to_cup_top_upper_error_z_abs]}
  - bottle_top_to_cup_top_upper_z_control:
      double-mul: 
        - bottle_top_to_cup_top_upper_z_scaling
        - range_gain
        - pos_gain
        - {z-coord: bottle_top_to_cup_top_upper_error_vector}

  # bottle relative to itself
  - bottle_to_itself_lower_error_x_abs:
      abs: {x-coord: bottle_to_itself_lower_error_vector}
  - bottle_to_itself_lower_x_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_to_itself_lower_error_x_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_to_itself_lower_error_x_abs]}
  - bottle_to_itself_lower_x_control:
      double-mul: 
        - bottle_to_itself_lower_x_scaling
        - range_gain
        - pos_gain
        - {x-coord: bottle_to_itself_lower_error_vector}
  - bottle_to_itself_upper_error_x_abs:
      abs: {x-coord: bottle_to_itself_upper_error_vector}
  - bottle_to_itself_upper_x_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_to_itself_upper_error_x_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_to_itself_upper_error_x_abs]}
  - bottle_to_itself_upper_x_control:
      double-mul: 
        - bottle_to_itself_upper_x_scaling
        - range_gain
        - pos_gain
        - {x-coord: bottle_to_itself_upper_error_vector}
  - bottle_to_itself_lower_error_y_abs:
      abs: {y-coord: bottle_to_itself_lower_error_vector}
  - bottle_to_itself_lower_y_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_to_itself_lower_error_y_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_to_itself_lower_error_y_abs]}
  - bottle_to_itself_lower_y_control:
      double-mul: 
        - bottle_to_itself_lower_y_scaling
        - range_gain
        - pos_gain
        - {y-coord: bottle_to_itself_lower_error_vector}
  - bottle_to_itself_upper_error_y_abs:
      abs: {y-coord: bottle_to_itself_upper_error_vector}
  - bottle_to_itself_upper_y_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_to_itself_upper_error_y_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_to_itself_upper_error_y_abs]}
  - bottle_to_itself_upper_y_control:
      double-mul: 
        - bottle_to_itself_upper_y_scaling
        - range_gain
        - pos_gain
        - {y-coord: bottle_to_itself_upper_error_vector}
  - bottle_to_itself_lower_error_z_abs:
      abs: {z-coord: bottle_to_itself_lower_error_vector}
  - bottle_to_itself_lower_z_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_to_itself_lower_error_z_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_to_itself_lower_error_z_abs]}
  - bottle_to_itself_lower_z_control:
      double-mul: 
        - bottle_to_itself_lower_z_scaling
        - range_gain
        - pos_gain
        - {z-coord: bottle_to_itself_lower_error_vector}
  - bottle_to_itself_upper_error_z_abs:
      abs: {z-coord: bottle_to_itself_upper_error_vector}
  - bottle_to_itself_upper_z_scaling:
      double-if:
        - {double-sub: [pos_thresh, bottle_to_itself_upper_error_z_abs]}
        - 1
        - {double-div: [pos_thresh, bottle_to_itself_upper_error_z_abs]}
  - bottle_to_itself_upper_z_control:
      double-mul: 
        - bottle_to_itself_upper_z_scaling
        - range_gain
        - pos_gain
        - {z-coord: bottle_to_itself_upper_error_vector}
  # cup relative to base
  - cup_top_to_base_lower_error_x_abs:
      abs: {x-coord: cup_top_to_base_lower_error_vector}
  - cup_top_to_base_lower_x_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_top_to_base_lower_error_x_abs]}
        - 1
        - {double-div: [pos_thresh, cup_top_to_base_lower_error_x_abs]}
  - cup_top_to_base_lower_x_control:
      double-mul: 
        - cup_top_to_base_lower_x_scaling
        - range_gain
        - pos_gain
        - {x-coord: cup_top_to_base_lower_error_vector}
  - cup_top_to_base_upper_error_x_abs:
      abs: {x-coord: cup_top_to_base_upper_error_vector}
  - cup_top_to_base_upper_x_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_top_to_base_upper_error_x_abs]}
        - 1
        - {double-div: [pos_thresh, cup_top_to_base_upper_error_x_abs]}
  - cup_top_to_base_upper_x_control:
      double-mul: 
        - cup_top_to_base_upper_x_scaling
        - range_gain
        - pos_gain
        - {x-coord: cup_top_to_base_upper_error_vector}
  - cup_top_to_base_lower_error_y_abs:
      abs: {y-coord: cup_top_to_base_lower_error_vector}
  - cup_top_to_base_lower_y_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_top_to_base_lower_error_y_abs]}
        - 1
        - {double-div: [pos_thresh, cup_top_to_base_lower_error_y_abs]}
  - cup_top_to_base_lower_y_control:
      double-mul: 
        - cup_top_to_base_lower_y_scaling
        - range_gain
        - pos_gain
        - {y-coord: cup_top_to_base_lower_error_vector}
  - cup_top_to_base_upper_error_y_abs:
      abs: {y-coord: cup_top_to_base_upper_error_vector}
  - cup_top_to_base_upper_y_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_top_to_base_upper_error_y_abs]}
        - 1
        - {double-div: [pos_thresh, cup_top_to_base_upper_error_y_abs]}
  - cup_top_to_base_upper_y_control:
      double-mul: 
        - cup_top_to_base_upper_y_scaling
        - range_gain
        - pos_gain
        - {y-coord: cup_top_to_base_upper_error_vector}
  - cup_top_to_base_lower_error_z_abs:
      abs: {z-coord: cup_top_to_base_lower_error_vector}
  - cup_top_to_base_lower_z_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_top_to_base_lower_error_z_abs]}
        - 1
        - {double-div: [pos_thresh, cup_top_to_base_lower_error_z_abs]}
  - cup_top_to_base_lower_z_control:
      double-mul: 
        - cup_top_to_base_lower_z_scaling
        - range_gain
        - pos_gain
        - {z-coord: cup_top_to_base_lower_error_vector}
  - cup_top_to_base_upper_error_z_abs:
      abs: {z-coord: cup_top_to_base_upper_error_vector}
  - cup_top_to_base_upper_z_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_top_to_base_upper_error_z_abs]}
        - 1
        - {double-div: [pos_thresh, cup_top_to_base_upper_error_z_abs]}
  - cup_top_to_base_upper_z_control:
      double-mul: 
        - cup_top_to_base_upper_z_scaling
        - range_gain
        - pos_gain
        - {z-coord: cup_top_to_base_upper_error_vector}

  # cup relative to itself
  - cup_to_itself_lower_error_x_abs:
      abs: {x-coord: cup_to_itself_lower_error_vector}
  - cup_to_itself_lower_x_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_to_itself_lower_error_x_abs]}
        - 1
        - {double-div: [pos_thresh, cup_to_itself_lower_error_x_abs]}
  - cup_to_itself_lower_x_control:
      double-mul: 
        - cup_to_itself_lower_x_scaling
        - range_gain
        - pos_gain
        - {x-coord: cup_to_itself_lower_error_vector}
  - cup_to_itself_upper_error_x_abs:
      abs: {x-coord: cup_to_itself_upper_error_vector}
  - cup_to_itself_upper_x_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_to_itself_upper_error_x_abs]}
        - 1
        - {double-div: [pos_thresh, cup_to_itself_upper_error_x_abs]}
  - cup_to_itself_upper_x_control:
      double-mul: 
        - cup_to_itself_upper_x_scaling
        - range_gain
        - pos_gain
        - {x-coord: cup_to_itself_upper_error_vector}
  - cup_to_itself_lower_error_y_abs:
      abs: {y-coord: cup_to_itself_lower_error_vector}
  - cup_to_itself_lower_y_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_to_itself_lower_error_y_abs]}
        - 1
        - {double-div: [pos_thresh, cup_to_itself_lower_error_y_abs]}
  - cup_to_itself_lower_y_control:
      double-mul: 
        - cup_to_itself_lower_y_scaling
        - range_gain
        - pos_gain
        - {y-coord: cup_to_itself_lower_error_vector}
  - cup_to_itself_upper_error_y_abs:
      abs: {y-coord: cup_to_itself_upper_error_vector}
  - cup_to_itself_upper_y_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_to_itself_upper_error_y_abs]}
        - 1
        - {double-div: [pos_thresh, cup_to_itself_upper_error_y_abs]}
  - cup_to_itself_upper_y_control:
      double-mul: 
        - cup_to_itself_upper_y_scaling
        - range_gain
        - pos_gain
        - {y-coord: cup_to_itself_upper_error_vector}
  - cup_to_itself_lower_error_z_abs:
      abs: {z-coord: cup_to_itself_lower_error_vector}
  - cup_to_itself_lower_z_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_to_itself_lower_error_z_abs]}
        - 1
        - {double-div: [pos_thresh, cup_to_itself_lower_error_z_abs]}
  - cup_to_itself_lower_z_control:
      double-mul: 
        - cup_to_itself_lower_z_scaling
        - range_gain
        - pos_gain
        - {z-coord: cup_to_itself_lower_error_vector}
  - cup_to_itself_upper_error_z_abs:
      abs: {z-coord: cup_to_itself_upper_error_vector}
  - cup_to_itself_upper_z_scaling:
      double-if:
        - {double-sub: [pos_thresh, cup_to_itself_upper_error_z_abs]}
        - 1
        - {double-div: [pos_thresh, cup_to_itself_upper_error_z_abs]}
  - cup_to_itself_upper_z_control:
      double-mul: 
        - cup_to_itself_upper_z_scaling
        - range_gain
        - pos_gain
        - {z-coord: cup_to_itself_upper_error_vector}

controllable-constraints:
  # torso joint
  - controllable-constraint: [-0.02, 0.02, weight_torso_joint, 0, torso_lift_joint]
  # left arm joints
  - controllable-constraint: [neg_vel_limit_left_arm_joints, pos_vel_limit_left_arm_joints, weight_arm_joints, 1, l_shoulder_pan_joint]
  - controllable-constraint: [neg_vel_limit_left_arm_joints, pos_vel_limit_left_arm_joints, weight_arm_joints, 2, l_shoulder_lift_joint]
  - controllable-constraint: [neg_vel_limit_left_arm_joints, pos_vel_limit_left_arm_joints, weight_arm_joints, 3, l_upper_arm_roll_joint]
  - controllable-constraint: [neg_vel_limit_left_arm_joints, pos_vel_limit_left_arm_joints, weight_arm_joints, 4, l_elbow_flex_joint]
  - controllable-constraint: [neg_vel_limit_left_arm_joints, pos_vel_limit_left_arm_joints, weight_arm_joints, 5, l_forearm_roll_joint]
  - controllable-constraint: [neg_vel_limit_left_arm_joints, pos_vel_limit_left_arm_joints, weight_arm_joints, 6, l_wrist_flex_joint]
  - controllable-constraint: [neg_vel_limit_left_arm_joints, pos_vel_limit_left_arm_joints, weight_arm_joints, 7, l_wrist_roll_joint]
  # right arm joints
  - controllable-constraint: [neg_vel_limit_right_arm_joints, pos_vel_limit_right_arm_joints, weight_arm_joints, 8, r_shoulder_pan_joint]
  - controllable-constraint: [neg_vel_limit_right_arm_joints, pos_vel_limit_right_arm_joints, weight_arm_joints, 9, r_shoulder_lift_joint]
  - controllable-constraint: [neg_vel_limit_right_arm_joints, pos_vel_limit_right_arm_joints, weight_arm_joints, 10, r_upper_arm_roll_joint]
  - controllable-constraint: [neg_vel_limit_right_arm_joints, pos_vel_limit_right_arm_joints, weight_arm_joints, 11, r_elbow_flex_joint]
  - controllable-constraint: [neg_vel_limit_right_arm_joints, pos_vel_limit_right_arm_joints, weight_arm_joints, 12, r_forearm_roll_joint]
  - controllable-constraint: [neg_vel_limit_right_arm_joints, pos_vel_limit_right_arm_joints, weight_arm_joints, 13, r_wrist_flex_joint]
  - controllable-constraint: [neg_vel_limit_right_arm_joints, pos_vel_limit_right_arm_joints, weight_arm_joints, 14, r_wrist_roll_joint]

soft-constraints:
  # PR2-specific constraints
  - soft-constraint: [{double-sub: [0.2, torso_lift_joint]}, {double-sub: [0.3, torso_lift_joint]} , weight_pos_control, torso_lift_joint, torso_lift_joint control slack]
  # task-specific constraints
  # bottle relative to cup
  - soft-constraint:
    - bottle_top_to_cup_top_lower_x_control
    - bottle_top_to_cup_top_upper_x_control
    - weight_range_control
    - {x-coord: bottle_top_to_cup_top}
    - bottle_behind_cup_range control slack
  - soft-constraint:
    - bottle_top_to_cup_top_lower_y_control
    - bottle_top_to_cup_top_upper_y_control
    - weight_range_control
    - {y-coord: bottle_top_to_cup_top}
    - bottle_left_cup_range control slack
  - soft-constraint:
    - bottle_top_to_cup_top_lower_z_control
    - bottle_top_to_cup_top_upper_z_control
    - weight_range_control
    - {z-coord: bottle_top_to_cup_top}
    - bottle_above_cup_range control slack
  # bottle relative to itself
  - soft-constraint:
    - bottle_to_itself_lower_x_control
    - bottle_to_itself_upper_x_control
    - weight_range_control
    - {x-coord: bottle_to_itself}
    - bottle_behind_itself_range control slack
  - soft-constraint:
    - bottle_to_itself_lower_y_control
    - bottle_to_itself_upper_y_control
    - weight_range_control
    - {y-coord: bottle_to_itself}
    - bottle_left_itself_range control slack
  - soft-constraint:
    - bottle_to_itself_lower_z_control
    - bottle_to_itself_upper_z_control
    - weight_range_control
    - {z-coord: bottle_to_itself}
    - bottle_above_itself_range control slack
  # cup relative to itself
  - soft-constraint:
    - cup_to_itself_lower_x_control
    - cup_to_itself_upper_x_control
    - weight_range_control
    - {x-coord: cup_to_itself}
    - cup_behind_itself_range control slack
  - soft-constraint:
    - cup_to_itself_lower_y_control
    - cup_to_itself_upper_y_control
    - weight_range_control
    - {y-coord: cup_to_itself}
    - cup_left_itself_range control slack
  - soft-constraint:
    - cup_to_itself_lower_z_control
    - cup_to_itself_upper_z_control
    - weight_range_control
    - {z-coord: cup_to_itself}
    - cup_above_itself_range control slack
  # cup relative to world
  - soft-constraint:
    - cup_top_to_base_lower_x_control
    - cup_top_to_base_upper_x_control
    - weight_range_control
    - {x-coord: cup_top}
    - cup_behind_base_range control slack
  - soft-constraint:
    - cup_top_to_base_lower_y_control
    - cup_top_to_base_upper_y_control
    - weight_range_control
    - {y-coord: cup_top}
    - cup_left_base_range control slack
  - soft-constraint:
    - cup_top_to_base_lower_z_control
    - cup_top_to_base_upper_z_control
    - weight_range_control
    - {z-coord: cup_top}
    - cup_above_base_range control slack

hard-constraints:
  - hard-constraint:
      - {double-sub: [0.0115, torso_lift_joint]}
      - {double-sub: [0.325, torso_lift_joint]}
      - torso_lift_joint
  - hard-constraint:
      - {double-sub: [-0.5646, l_shoulder_pan_joint]}
      - {double-sub: [2.1353, l_shoulder_pan_joint]}
      - l_shoulder_pan_joint
  - hard-constraint:
      - {double-sub: [-0.3536, l_shoulder_lift_joint]}
      - {double-sub: [1.2963, l_shoulder_lift_joint]}
      -  l_shoulder_lift_joint
  - hard-constraint:
      - {double-sub: [-0.65, l_upper_arm_roll_joint]}
      - {double-sub: [3.75, l_upper_arm_roll_joint]}
      - l_upper_arm_roll_joint
  - hard-constraint:
      - {double-sub: [-2.1213, l_elbow_flex_joint]}
      - {double-sub: [-0.15, l_elbow_flex_joint]}
      - l_elbow_flex_joint
  - hard-constraint:
      - {double-sub: [-2.0, l_wrist_flex_joint]}
      - {double-sub: [-0.1, l_wrist_flex_joint]}
      - l_wrist_flex_joint
  - hard-constraint:
      - {double-sub: [-2.1353, r_shoulder_pan_joint]}
      - {double-sub: [0.5646, r_shoulder_pan_joint]}
      - r_shoulder_pan_joint
  - hard-constraint:
      - {double-sub: [-0.3536, r_shoulder_lift_joint]}
      - {double-sub: [1.2963, r_shoulder_lift_joint]}
      -  r_shoulder_lift_joint
  - hard-constraint:
      - {double-sub: [-3.75, r_upper_arm_roll_joint]}
      - {double-sub: [0.65, r_upper_arm_roll_joint]}
      - r_upper_arm_roll_joint
  - hard-constraint:
      - {double-sub: [-2.1213, r_elbow_flex_joint]}
      - {double-sub: [-0.15, r_elbow_flex_joint]}
      - r_elbow_flex_joint
  - hard-constraint:
      - {double-sub: [-2.0, r_wrist_flex_joint]}
      - {double-sub: [-0.1, r_wrist_flex_joint]}
      - r_wrist_flex_joint
