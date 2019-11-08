# For YI firmware 1.2.13, MODEL Z23L 
# 1296p30 video mode, ABR, 35Mbps, bitrate range: .75 - 1.25, 4GB file

# Set 1296p30 mode
writeb 0xC06CC426 0x02
# Set CBR2 bitrate mode - ABR like
writeb 0xC05C1EAC 0x05
# Set bitrate to 35 Mbps
writew 0xC05C1EB2 0x420C
# Set low bitrate limit for ABR
writel 0xC05C1EB4 0x3f400000
# Set high bitrate limit for ABR
writel 0xC05C1EB8 0x3fa00000

# Set file size limit to 4GB:
writew 0xC03A8520 0x2004

# Sharpness Adjustments
# Coring
t is2 -shp mode 0
t is2 -shp max_change 5 5
t is2 -shp cor d:\coring.txt
sleep 1
# Noise adjust 
t ia2 -adj tidx -1 1024 -1

# Beep when script is loaded
t pwm 1 set_level 10
t pwm 1 enable
sleep 1
t pwm 1 disable
