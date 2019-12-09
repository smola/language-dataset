# i3 config file (v4) for Regolith Desktop Environment #
# Please see http://i3wm.org/docs/userguide.html for a complete reference!

# This defines which key super maps to on your keyboard.
# Alt key is Mod1, and Windows key is Mod4
set $mod Mod4

exec "wal -R"

# Kanji Numbers
set $ws1 "1:一"
set $ws2 "2:二"
set $ws3 "3:三"
set $ws4 "4:四"
set $ws5 "5:五"
set $ws6 "6:六"
set $ws7 "7:七"
set $ws8 "8:八"
set $ws9 "9:九"
set $ws10 "10:十"
set $ws11 "11:XI"
set $ws12 "12:XII"
set $ws13 "13:XIII"
set $ws14 "14:XIV"
set $ws15 "15:XV"
set $ws16 "16:XVI"
set $ws17 "17:XVII"
set $ws18 "18:XIIX"
set $ws19 "19:XIX"

# Use Mouse+$mod to drag floating windows to their wanted position
floating_modifier $mod

# mod + mouse middle click to close window
bindsym --release --whole-window $mod+button2 kill

# mod + shift + left click to float window
bindsym --whole-window $mod+Shift+button1 floating toggle

hide_edge_borders smart

# start a terminal
bindsym $mod+Return exec rxvt

# start a web browser
bindsym $mod+Shift+Return exec /usr/bin/sensible-browser

# kill focused window
bindsym $mod+q kill

set_from_resource $rofiTheme rofi.theme "regolith-theme"

# rofi app and window launchers
bindsym $mod+space exec rofi -show drun -theme $rofiTheme
bindsym $mod+Shift+space exec rofi -show run -theme $rofiTheme
bindsym $mod+Ctrl+space exec rofi -show window -theme $rofiTheme

# change focus
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

bindsym $mod+h focus left
bindsym $mod+j focus down
bindsym $mod+k focus up
bindsym $mod+l focus right

# move windows in workspaces
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right

bindsym $mod+Shift+h move left
bindsym $mod+Shift+j move down
bindsym $mod+Shift+k move up
bindsym $mod+Shift+l move right

# toggle split orientation
bindsym $mod+BackSpace split toggle

# enter fullscreen mode for the focused container
bindsym $mod+f fullscreen toggle

# change gaps interactively
bindsym $mod+minus gaps inner current minus 6
bindsym $mod+plus gaps inner current plus 6

bindsym $mod+Shift+plus gaps outer current plus 6
bindsym $mod+Shift+minus gaps outer current minus 6

# toggle tiling / floating
bindsym $mod+Shift+f floating toggle

# change focus between tiling / floating windows
bindsym $mod+Shift+t focus mode_toggle

# toggle tabbed mode
bindsym $mod+t layout toggle tabbed 
bindsym $mod+v layout toggle splith splitv

# switch to workspace
bindsym $mod+1 workspace $ws1
bindsym $mod+2 workspace $ws2
bindsym $mod+3 workspace $ws3
bindsym $mod+4 workspace $ws4
bindsym $mod+5 workspace $ws5
bindsym $mod+6 workspace $ws6
bindsym $mod+7 workspace $ws7
bindsym $mod+8 workspace $ws8
bindsym $mod+9 workspace $ws9
bindsym $mod+0 workspace $ws10
bindsym $mod+Ctrl+1 workspace $ws11
bindsym $mod+Ctrl+2 workspace $ws12
bindsym $mod+Ctrl+3 workspace $ws13
bindsym $mod+Ctrl+4 workspace $ws14
bindsym $mod+Ctrl+5 workspace $ws15
bindsym $mod+Ctrl+6 workspace $ws16
bindsym $mod+Ctrl+7 workspace $ws17
bindsym $mod+Ctrl+8 workspace $ws18
bindsym $mod+Ctrl+9 workspace $ws19

# cycle between workspace
bindsym $mod+Tab workspace next
bindsym $mod+Shift+Tab workspace prev

# move focused container to workspace
bindsym $mod+Shift+1 move container to workspace $ws1
bindsym $mod+Shift+2 move container to workspace $ws2
bindsym $mod+Shift+3 move container to workspace $ws3
bindsym $mod+Shift+4 move container to workspace $ws4
bindsym $mod+Shift+5 move container to workspace $ws5
bindsym $mod+Shift+6 move container to workspace $ws6
bindsym $mod+Shift+7 move container to workspace $ws7
bindsym $mod+Shift+8 move container to workspace $ws8
bindsym $mod+Shift+9 move container to workspace $ws9
bindsym $mod+Shift+0 move container to workspace $ws10
bindsym $mod+Shift+Ctrl+1 move container to workspace $ws11
bindsym $mod+Shift+Ctrl+2 move container to workspace $ws12
bindsym $mod+Shift+Ctrl+3 move container to workspace $ws13
bindsym $mod+Shift+Ctrl+4 move container to workspace $ws14
bindsym $mod+Shift+Ctrl+5 move container to workspace $ws15
bindsym $mod+Shift+Ctrl+6 move container to workspace $ws16
bindsym $mod+Shift+Ctrl+7 move container to workspace $ws17
bindsym $mod+Shift+Ctrl+8 move container to workspace $ws18
bindsym $mod+Shift+Ctrl+9 move container to workspace $ws19

bindsym $mod+Shift+grave move container to workspace $ws20

# custom binding to workspaces
for_window [class="Spotify"] move to workspace $ws10
for_window [class="Slack"] move to workspace $ws9
for_window [class="discord"] move to workspace $ws9
for_window [class="Eog"] floating enable
for_window [class="Places"] floating enable

# reload the configuration file
bindsym $mod+Shift+c reload

# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $mod+Shift+r restart

# exit i3
bindsym $mod+Shift+e exit

# lock the screen
# bindsym $mod+Escape exec gnome-screensaver-command --lock

# put the system to sleep
bindsym $mod+Shift+s exec systemctl suspend

# shortcuts for common system configuration tasks
bindsym $mod+w exec gnome-control-center wifi
bindsym $mod+b exec gnome-control-center bluetooth
bindsym $mod+d exec gnome-control-center display
bindsym $mod+p exec gnome-control-center power
bindsym $mod+s exec gnome-control-center sound

# resize window (you can also use the mouse for that)
mode "Resize Mode" {
        # These bindings trigger as soon as you enter the resize mode
        bindsym Left resize shrink width 6 px or 6 ppt
        bindsym Down resize grow height 6 px or 6 ppt
        bindsym Up resize shrink height 6 px or 6 ppt
        bindsym Right resize grow width 6 px or 6 ppt

        bindsym Shift+Left resize shrink width 12 px or 12 ppt
        bindsym Shift+Down resize grow height 12 px or 12 ppt
        bindsym Shift+Up resize shrink height 12 px or 12 ppt
        bindsym Shift+Right resize grow width 12 px or 12 ppt

        bindsym h resize shrink width 6 px or 6 ppt
        bindsym k resize grow height 6 px or 6 ppt
        bindsym j resize shrink height 6 px or 6 ppt
        bindsym l resize grow width 6 px or 6 ppt

        bindsym Shift+h resize shrink width 12 px or 12 ppt
        bindsym Shift+k resize grow height 12 px or 12 ppt
        bindsym Shift+j resize shrink height 12 px or 12 ppt
        bindsym Shift+l resize grow width 12 px or 12 ppt

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
        bindsym $mod+r mode "default"
}
bindsym $mod+r mode "Resize Mode"

# Disable titlebar
new_window pixel 1
new_float pixel 1

# Gaps (i3-gaps)
gaps inner 12
gaps outer 1

# Only enable gaps on a workspace when there is at least one container
smart_gaps on

set_from_resource $focused.color.border i3-wm.client.focused.color.border "#002b36"
set_from_resource $focused.color.background i3-wm.client.focused.color.background "#586e75"
set_from_resource $focused.color.text i3-wm.client.focused.color.text "#fdf6e3"
set_from_resource $focused.color.indicator i3-wm.client.focused.color.indicator "#268bd2"
set_from_resource $focused.color.child_border i3-wm.client.focused.color.child_border

set_from_resource $focused_inactive.color.border i3-wm.client.focused_inactive.color.border "#002b36"
set_from_resource $focused_inactive.color.background i3-wm.client.focused_inactive.color.background "#073642"
set_from_resource $focused_inactive.color.text i3-wm.client.focused_inactive.color.text "#839496"
set_from_resource $focused_inactive.color.indicator i3-wm.client.focused_inactive.color.indicator "#073642"
set_from_resource $focused_inactive.color.child_border i3-wm.client.focused_inactive.color.child_border 

set_from_resource $unfocused.color.border i3-wm.client.unfocused.color.border "#002b36"
set_from_resource $unfocused.color.background i3-wm.client.unfocused.color.background "#073642"
set_from_resource $unfocused.color.text i3-wm.client.unfocused.color.text "#839496"
set_from_resource $unfocused.color.indicator i3-wm.client.unfocused.color.indicator "#073642"
set_from_resource $unfocused.color.child_border i3-wm.client.unfocused.color.child_border

set_from_resource $urgent.color.border i3-wm.client.urgent.color.border "#002b36"
set_from_resource $urgent.color.background i3-wm.client.urgent.color.background "#dc322f"
set_from_resource $urgent.color.text i3-wm.client.urgent.color.text "#fdf6e3"
set_from_resource $urgent.color.indicator i3-wm.client.urgent.color.indicator "#002b36"
set_from_resource $urgent.color.child_border i3-wm.client.urgent.color.child_border

#set_from_resource $pycolor1 i3wm.color1 #f0f0f0
#set_from_resource $pycolor2 i3wm.color2 #f0f0f0
#
#
## Window Border color
## class                 border                             background                         text                               indicator                          child_border
#client.focused          $pycolor1                          $focused.color.background          $focused.color.text                $focused.color.indicator           $focused.color.child_border
#client.focused_inactive $pycolor2                          $focused_inactive.color.background $focused_inactive.color.text       $focused_inactive.color.indicator  $focused_inactive.color.child_border
#client.unfocused        $pycolor2                          $unfocused.color.background        $unfocused.color.text              $unfocused.color.indicator         $unfocused.color.child_border
#client.urgent           $pycolor2                          $urgent.color.background           $urgent.color.text                 $urgent.color.indicator            $urgent.color.child_border

set_from_resource $c0 i3wm.color0 #f0f0f0
set_from_resource $c1 i3wm.color1 #f0f0f0
set_from_resource $c2 i3wm.color2 #f0f0f0
set_from_resource $c3 i3wm.color3 #f0f0f0
set_from_resource $c4 i3wm.color4 #f0f0f0
set_from_resource $c5 i3wm.color5 #f0f0f0
set_from_resource $c6 i3wm.color6 #f0f0f0
set_from_resource $c7 i3wm.color7 #f0f0f0
set_from_resource $c8 i3wm.color8 #f0f0f0
set_from_resource $c9 i3wm.color9 #f0f0f0
set_from_resource $c10 i3wm.color10 #f0f0f0
set_from_resource $c11 i3wm.color11 #f0f0f0
set_from_resource $c12 i3wm.color12 #f0f0f0
set_from_resource $c13 i3wm.color13 #f0f0f0
set_from_resource $c14 i3wm.color14 #f0f0f0
set_from_resource $c15 i3wm.color15 #f0f0f0

# class                 border  backgr. text indicator child_border
client.focused          $c7     $c7     $c0  $c7       $c7
client.focused_inactive $c2     $c2     $c7  $c2       $c2
client.unfocused        $c2     $c2     $c7  $c2       $c2
client.urgent           $c4     $c4     $c0  $c2       $c2
client.placeholder      $c2     $c2     $c7  $c2       $c2

client.background       $c0

# Enable popup during fullscreen
popup_during_fullscreen smart

exec_always --no-startup-id /home/silver/.config/polybar/launch.sh

# Configure the bar
#bar {
#  font $i3-wm.bar.font
#  separator_symbol "|"
#  status_command i3xrocks -c ~/.config/regolith/i3xrocks/config
#  tray_output none
#  strip_workspace_numbers yes
#
#  colors {
#      background $i3-wm.bar.background.color
#      statusline $i3-wm.bar.statusline.color
#      separator  $i3-wm.bar.separator.color
#
##                        BORDER  BACKGROUND TEXT
#      focused_workspace  $i3-wm.bar.workspace.focused.border.color      $i3-wm.bar.workspace.focused.background.color   $i3-wm.bar.workspace.focused.text.color
#      active_workspace   $i3-wm.bar.workspace.active.border.color       $i3-wm.bar.workspace.active.background.color    $i3-wm.bar.workspace.active.text.color
#      inactive_workspace $i3-wm.bar.workspace.inactive.border.color     $i3-wm.bar.workspace.inactive.background.color  $i3-wm.bar.workspace.inactive.text.color
#      urgent_workspace   $i3-wm.bar.workspace.urgent.border.color       $i3-wm.bar.workspace.urgent.background.color    $i3-wm.bar.workspace.urgent.text.color
#  }
#}

# Run programs when i3 starts

for_window [instance="floatterm"] floating enable
for_window [instance="floatterm"] move scratchpad; scratchpad show
for_window [instance="floatterm"] resize set 1200 800
for_window [instance="floatterm"] move position 50 50

for_window [class="mpv"] floating enable, resize set 1170 660, move position center
for_window [class="evolution-alarm-notify"] floating enable, resize set 1170 660, move position center

# custom keybinds
bindsym $mod+Shift+x exec shutdown.sh
bindsym $mod+e exec rxvt -e ranger
bindsym $mod+n exec rxvt -e newsboat
bindsym $mod+c exec rxvt -e cmus
bindsym $mod+u [instance="floatterm"] scratchpad show
bindsym $mod+Mod1+Return exec rxvt -name floatterm
bindsym $mod+m exec cmus-remote -u

# Composite manager
exec --no-startup-id compton -f --config /etc/xdg/compton.conf

# Hide the mouse pointer if unused for a duration
exec --no-startup-id /usr/bin/unclutter -b
