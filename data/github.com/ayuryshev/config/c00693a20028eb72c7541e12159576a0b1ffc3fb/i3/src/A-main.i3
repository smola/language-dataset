#
# Main
#

set $mod Mod4

default_border pixel 2
font pango:DejaVu Sans Mono 16
floating_modifier $mod

exec --no-startup-id redshift-gtk
workspace_auto_back_and_forth yes

# start a terminal
#bindsym $mod+Return exec i3-sensible-terminal
set $TERMINAL terminal
bindsym $mod+Return exec $TERMINAL
bindsym $mod+Shift+Return exec alacritty
bindsym $mod+Shift+h exec chrome

# kill focused window
bindsym $mod+Shift+q kill

# alternatively, you can use the cursor keys:
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

# keyboard
exec --no-startup-id setxkbmap -layout us,ru -option "grp:rctrl_toggle" -option caps:super  -option compose:ralt 

# rofi
bindsym $mod+d exec rofi -show drun -location 0  -width 1800  -columns 5 
bindsym $mod+Tab exec rofi -show window -location 0  -width 1800  -columns 5 


bindsym $mod+b border toggle
bindsym $mod+g exec "i3-msg gaps inner set 150"
bindsym $mod+Shift+g exec "i3-msg gaps all  inner set 6"

# move focused window
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right

# split in vertical orientation
bindsym $mod+v split v

# enter fullscreen mode for the focused container
bindsym $mod+Shift+f fullscreen toggle

# change container layout (stacked, tabbed, toggle split)
bindsym $mod+s sticky toggle
bindsym $mod+w layout tabbed
bindsym $mod+e layout toggle split

# toggle tiling / floating
bindsym $mod+f floating toggle
# change focus between tiling / floating windows
bindsym $mod+space focus mode_toggle
# focus the parent container
bindsym $mod+a focus parent
# focus the child container
# bindsym $mod+c focus child
bindsym $mod+u [urgent=latest] focus


bindsym $mod+i			move to absolute position 0 0;		resize set 960 540
bindsym $mod+o			move to absolute position 480 0 ;	resize set 960 540
bindsym $mod+p			move to absolute position 960 0 ;	resize set 960 540
bindsym $mod+k			move to absolute position 0 0;		resize set 960 1080; mode superfloating
bindsym $mod+l			move to absolute position 480 0 ;	resize set 960 1080; mode superfloating
bindsym $mod+semicolon	move to absolute position 960 0 ;	resize set 960 1080; mode superfloating
bindsym $mod+comma		move to absolute position 0 540;	resize set 960 540
bindsym $mod+period		move to absolute position 480 540 ; resize set 960 540
bindsym $mod+slash		move to absolute position 960 540 ; resize set 960 540

mode "superfloating"{
	bindsym $mod+k move to absolute position 0 0;			resize set 1460 1080; mode "default"
	bindsym $mod+l move to absolute position 200 0 ;		resize set 1460 1080; mode "default"
	bindsym $mod+semicolon move to absolute position 960 0; resize set 1460 1080; mode "default" 
	bindsym space mode "default"
	bindsym Enter mode "default"
}



bindsym $mod+Shift+1 move container to workspace number 1
bindsym $mod+Shift+2 move container to workspace number 2
bindsym $mod+Shift+3 move container to workspace number 3
bindsym $mod+Shift+4 move container to workspace number 4
bindsym $mod+Shift+5 move container to workspace number 5
bindsym $mod+Shift+6 move container to workspace number 6
bindsym $mod+Shift+7 move container to workspace number 7
bindsym $mod+Shift+8 move container to workspace number 8
bindsym $mod+Shift+9 move container to workspace number 9
bindsym $mod+Shift+0 move container to workspace number 10
                                                 
bindsym $mod+z workspace next                
bindsym $mod+Shift+z workspace prev

bindsym $mod+x scratchpad show
bindsym $mod+Shift+x move container to scratchpad

# reload the configuration file
# bindsym $mod+Shift+c reload
# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $mod+Shift+Pause restart

# TODO: Divide A-main

# resize window (you can also use the mouse for that)
mode "resize" {
        bindsym j resize shrink width 10 px or 10 ppt
		bindsym semicolon resize grow width 10 px or 10 ppt
        # same bindings, but for the arrow keys
        bindsym Left resize shrink width 30 px or 30 ppt
        bindsym Down resize grow height 30 px or 30 ppt
        bindsym Up resize shrink height 30 px or 30 ppt
        bindsym Right resize grow width 30 px or 30 ppt

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
        bindsym space mode "default"
		bindsym $mod+Shift+r mode "default"
}
bindsym $mod+Shift+r mode "resize"

for_window [class="Termite"] border pixel 4
for_window [class="Deepin-terminal"] border pixel 4
