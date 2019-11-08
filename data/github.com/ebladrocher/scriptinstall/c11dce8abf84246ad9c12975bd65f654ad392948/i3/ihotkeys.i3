

########################################################################
########################################################################
####
####                   HOTKEYS 
####
########################################################################
########################################################################

# volume
bindsym XF86AudioRaiseVolume exec pactl set-sink-volume 0 +5%
bindsym XF86AudioLowerVolume exec pactl set-sink-volume 0 -5%
bindsym XF86AudioMute exec pactl set-sink-mute 0 toggle
bindsym XF86AudioMicMute exec pactl set-source-mute alsa_input.pci-0000_00_1b.0.analog-stereo toggle
#bindsym XF86AudioMicMute exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle && $refresh_i3status

# light
bindsym XF86MonBrightnessUp exec xbacklight -inc 5
bindsym XF86MonBrightnessDown exec xbacklight -dec 5


# lock screen 
bindsym $mod+Escape exec /usr/bin/i3lockcustom.sh

# Screenshot
bindsym --release Ctrl+Print exec maim -u ~/Pictures/Screenshots/screenshot-$(date +%F_%T).png && notify-send "Screenshot captured"

bindsym --release $mod+Shift+s exec maim | feh - -x & maim -s --format png /dev/stdout | xclip -selection clipboard -t image/png -i && notify-send "Screenshot captured"
for_window [class="feh"] fullscreen enable

#bindsym --release $mod+Shift+s exec maim -s -u --format png /dev/stdout | xclip -selection clipboard -t image/png -i && notify-send "Screenshot captured"

# Console app
bindsym $mod+Shift+w exec $term -e $file &
bindsym ctrl+Shift+Escape exec $term -e $cpu &

#bindsym $mod+Shift+p exec gnome-calculator

#bindcode $mod+110 exec gksudo /home/dm/soft/vpn1.sh #win+PgUp
#bindcode $mod+115 exec gksudo /home/dm/soft/vpn2.sh #win+PgDown
#bindcode $mod+118 exec gksudo /home/dm/soft/vpn_off.sh #win+ins

#bindsym ctrl+backslash exec /home/dm/soft/passmenu
#bindsym $mod+shift+y exec /home/dm/soft/youtube_vlc.sh

#bindcode $mod+34 exec /home/dm/soft/encrypt_personal_docs.sh #mod+[
#bindcode $mod+35 exec /home/dm/soft/decrypt_personal_docs.sh #mod+]

#bindsym --release ctrl+shift+4 exec "/home/dm/soft/screenshot.sh"
#bindsym --release ctrl+shift+5 exec "/home/dm/soft/screenshot_local.sh"

#bindcode $mod+shift+118 exec "caffeine -a"
#bindcode $mod+shift+119 exec "caffeine kill"

#bindsym --release ctrl+shift+6 exec "/home/dm/soft/screencast3.sh"

#bindsym ctrl+shift+7 exec "/home/dm/soft/youtube_one_click.sh"

#bindsym $mod+i exec slock
#bindsym $mod+l exec "setxkbmap -layout us && sleep 0.1 && setxkbmap -option grp:caps_toggle 'us,ru' -option grp_led:caps && sleep 0.1 && i3lock -c 111111"

#bindsym $mod+l exec "/home/dm/fuzzy_lock.sh"
#bindsym $mod+k exec i3-msg exec 'i3lock -c 000000' && systemctl suspend




