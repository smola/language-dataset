# i3 config file (v4)
#
# Please see https://i3wm.org/docs/userguide.html for a complete reference!
#
# This config file uses keycodes (bindsym) and was written for the QWERTY
# layout.
#
# To get a config file with the same key positions, but for your current
# layout, use the i3-config-wizard
#

# Font for window titles. Will also be used by the bar unless a different font
# is used in the bar {} block below.
font pango:monospace 12

# This font is widely installed, provides lots of unicode glyphs, right-to-left
# text rendering and scalability on retina/hidpi displays (thanks to pango).
#font pango:DejaVu Sans Mono 8

# Before i3 v4.8, we used to recommend this one as the default:
# font -misc-fixed-medium-r-normal--13-120-75-75-C-70-iso10646-1
# The font above is very space-efficient, that is, it looks good, sharp and
# clear in small sizes. However, its unicode glyph coverage is limited, the old
# X core fonts rendering does not support right-to-left and this being a bitmap
# font, it doesn't scale on retina/hidpi displays.

# use these keys for focus, movement, and resize directions when reaching for
# the arrows is not convenient
set $up l
set $down k
set $left j
set $right semicolon

# use Mouse+Mod4 to drag floating windows to their wanted position
floating_modifier Mod4

# start a terminal
bindsym Mod4+Return exec i3-sensible-terminal

# kill focused window
bindsym Mod4+Shift+q kill

# start dmenu (a program launcher)
bindsym Mod4+d exec dmenu_run
# There also is the (new) i3-dmenu-desktop which only displays applications
# shipping a .desktop file. It is a wrapper around dmenu, so you need that
# installed.
# bindsym Mod4+d exec --no-startup-id i3-dmenu-desktop

# change focus
bindsym Mod4+$left focus left
bindsym Mod4+$down focus down
bindsym Mod4+$up focus up
bindsym Mod4+$right focus right

# move focused window
bindsym Mod4+Shift+$left move left
bindsym Mod4+Shift+$down move down
bindsym Mod4+Shift+$up move up
bindsym Mod4+Shift+$right move right

# alternatively, you can use the cursor keys:
bindsym Mod4+Shift+Left move left
bindsym Mod4+Shift+Down move down
bindsym Mod4+Shift+Up move up
bindsym Mod4+Shift+Right move right

# split in horizontal orientation
bindsym Mod4+h split h

# split in vertical orientation
bindsym Mod4+v split v

# enter fullscreen mode for the focused container
bindsym Mod4+f fullscreen toggle

# change container layout (stacked, tabbed, toggle split)
bindsym Mod4+s layout stacking
bindsym Mod4+w layout tabbed
bindsym Mod4+e layout toggle split

# toggle tiling / floating
bindsym Mod4+Shift+space floating toggle

# change focus between tiling / floating windows
bindsym Mod4+space focus mode_toggle

# focus the parent container
bindsym Mod4+a focus parent

# focus the child container
#bindsym Mod4+d focus child

# move the currently focused window to the scratchpad
bindsym Mod4+Shift+minus move scratchpad

# Show the next scratchpad window or hide the focused scratchpad window.
# If there are multiple scratchpad windows, this command cycles through them.
bindsym Mod4+minus scratchpad show

# Define names for default workspaces for which we configure key bindings later on.
# We use variables to avoid repeating the names in multiple places.
set $ws1 "Human"
set $ws2 "Com"
set $ws3 "Read"
set $ws4 "Work4"
set $ws5 "Work5"
set $ws6 "Work6"
set $ws7 "Tall7"
set $ws8 "Tall8"
set $ws9 "Tall9"
set $ws10 "Server"

# Set default workspaces
workspace $ws1 output eDP-1
workspace $ws2 output eDP-1
workspace $ws3 output eDP-1
workspace $ws4 output HDMI-1
workspace $ws5 output HDMI-1
workspace $ws6 output HDMI-1
workspace $ws7 output DP-2
workspace $ws8 output DP-2
workspace $ws9 output DP-2
workspace $ws0 output HDMI-1

# Set workspace wallpaper
exec --no-startup-id feh --bg-scale ~/Pictures/wallpaper/work.jpg

# switch to workspace
bindsym Mod4+1 workspace $ws1
bindsym Mod4+2 workspace $ws2
bindsym Mod4+3 workspace $ws3
bindsym Mod4+4 workspace $ws4
bindsym Mod4+5 workspace $ws5
bindsym Mod4+6 workspace $ws6
bindsym Mod4+7 workspace $ws7
bindsym Mod4+8 workspace $ws8
bindsym Mod4+9 workspace $ws9
bindsym Mod4+0 workspace $ws10

# move focused container to workspace
bindsym Mod4+Shift+1 move container to workspace $ws1
bindsym Mod4+Shift+2 move container to workspace $ws2
bindsym Mod4+Shift+3 move container to workspace $ws3
bindsym Mod4+Shift+4 move container to workspace $ws4
bindsym Mod4+Shift+5 move container to workspace $ws5
bindsym Mod4+Shift+6 move container to workspace $ws6
bindsym Mod4+Shift+7 move container to workspace $ws7
bindsym Mod4+Shift+8 move container to workspace $ws8
bindsym Mod4+Shift+9 move container to workspace $ws9
bindsym Mod4+Shift+0 move container to workspace $ws10

# move focuse to monitor
bindsym Mod4+F1 focus output eDP-1
bindsym Mod4+F2 focus output HDMI-1
bindsym Mod4+F3 focus output DP-2

# reload the configuration file
bindsym Mod4+Shift+c reload
# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym Mod4+Shift+r restart
# exit i3 (logs you out of your X session)
bindsym Mod4+Shift+Escape exec i3-msg exit

# resize window (you can also use the mouse for that)
mode "resize" {
        # These bindings trigger as soon as you enter the resize mode

        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym $left       resize shrink width 10 px or 10 ppt
        bindsym $down       resize grow height 10 px or 10 ppt
        bindsym $up         resize shrink height 10 px or 10 ppt
        bindsym $right      resize grow width 10 px or 10 ppt

        # same bindings, but for the arrow keys
        bindsym Left        resize shrink width 10 px or 10 ppt
        bindsym Down        resize grow height 10 px or 10 ppt
        bindsym Up          resize shrink height 10 px or 10 ppt
        bindsym Right       resize grow width 10 px or 10 ppt

        # back to normal: Enter or Escape or Mod4+r
        bindsym Return mode "default"
        bindsym Escape mode "default"
        bindsym Mod4+r mode "default"
}

bindsym Mod4+r mode "resize"

# Start i3bar to display a workspace bar (plus the system information i3status
# finds out, if available)
bar {
  status_command i3status
	position top
}

#######################################################################
# automatically start i3-config-wizard to offer the user to create a
# keysym-based config which used their favorite modifier (alt or windows)
#
# i3-config-wizard will not launch if there already is a config file
# in ~/.i3/config.
#
# Please remove the following exec line:
#######################################################################
exec i3-config-wizard

hide_edge_borders smart
default_border pixel
force_display_urgency_hint 500 ms

bindsym Mod4+x exec emacsclient -c
bindsym Mod4+c exec google-chrome

# Lock screen
bindsym Mod4+p exec i3lock -i /home/steep/Pictures/wallpaper/work.png -t

bindsym Mod4+Left move workspace to output left
bindsym Mod4+Right move workspace to output right
bindsym Mod4+Up move workspace to output up
bindsym Mod4+Down move workspace to output down

# Pulse Audio controls
bindsym XF86AudioRaiseVolume exec --no-startup-id pactl -- set-sink-volume 0 +5% #increase sound volume
bindsym XF86AudioLowerVolume exec --no-startup-id pactl -- set-sink-volume 0 -5% #decrease sound volume
bindsym XF86AudioMute exec --no-startup-id pactl set-sink-mute 0 toggle # mute sound

# Sreen brightness controls
bindsym XF86MonBrightnessDown exec xbacklight -inc 20 # increase screen brightness
bindsym XF86MonBrightnessUp exec xbacklight -dec 20 # decrease screen brightness