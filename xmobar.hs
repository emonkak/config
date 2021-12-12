import Xmobar

import Monitors.CatNum (CatNum(..))
import Monitors.PulseAudio (PulseAudio(..))

config :: Config
config = defaultConfig
  { font = "xft:Monospace:pixelsize=11,Tazugane Gothic StdN:pixelsize=11,Noto Emoji:pixelsize=11"
  , additionalFonts = [ "xft:Material Icons:pixelsize=16" ]
  , bgColor = "#21272b"
  , fgColor = "#f5f6f7"
  , position = Top
  , overrideRedirect = False
  , commands = [ Run StdinReader
               , Run $ MPD
                 [ "-t", "<statei> <title><fc=#869096>/</fc><artist> <fc=#869096>(<volume>%)</fc>"
                 , "-M", "20"
                 , "-e", "..."
                 , "--"
                 , "-P", "<fc=#5ebaf7><fn=1>\xe405</fn></fc>"
                 , "-S", "<fc=#869096><fn=1>\xe047</fn></fc>"
                 , "-Z", "<fc=#869096><fn=1>\xe034</fn></fc>"
                 , "-h", "127.0.0.1"
                 , "-p", "6600"
                 ] 10
               , Run $ Cpu
                 [ "-t", "<fc=#5ebaf7><fn=1>\xe1af</fn></fc> <total>"
                 , "-L", "50"
                 , "-H", "75"
                 , "-n", "#5ebaf7"
                 , "-h", "#fc8e88"
                 , "-S", "True"
                 , "-p", "3"
                 ] 10
               , Run $ CatNum "cputemp"
                 [ "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon2/temp1_input"
                 ]
                 [ "-t", "<fc=#869096>(</fc><n0><fc=#869096>)</fc>"
                 , "-L", "50000"
                 , "-H", "70000"
                 , "-l", "#869096"
                 , "-n", "#5ebaf7"
                 , "-h", "#fc8e88"
                 , "-m", "4"
                 , "--"
                 , "--divier", "1000"
                 , "--suffix", "°C"
                 ] 10
               , Run $ CatNum "gpu"
                 [ "/sys/class/drm/card0/device/gpu_busy_percent"
                 ]
                 [ "-t", "<fc=#5ebaf7><fn=1>\xe30c</fn></fc> <n0>"
                 , "-L", "50"
                 , "-H", "75"
                 , "-n", "#5ebaf7"
                 , "-h", "#fc8e88"
                 , "-m", "4"
                 , "--"
                 , "--suffix", "%"
                 ] 10
               , Run $ CatNum "gputemp"
                 [ "/sys/class/drm/card0/device/hwmon/hwmon1/temp2_input"
                 ]
                 [ "-t", "<fc=#869096>(</fc><n0><fc=#869096>)</fc>"
                 , "-L", "50000"
                 , "-H", "70000"
                 , "-l", "#869096"
                 , "-n", "#5ebaf7"
                 , "-h", "#fc8e88"
                 , "-m", "4"
                 , "--"
                 , "--divier", "1000"
                 , "--suffix", "°C"
                 ] 10
               , Run $ Memory
                 [ "-t", "<fc=#5ebaf7><fn=1>\xe322</fn></fc> <usedratio>"
                 , "-L", "50"
                 , "-H", "75"
                 , "-n", "#5ebaf7"
                 , "-h", "#fc8e88"
                 , "-S", "True"
                 , "-p", "3"
                 ] 10
               , Run $ DiskU
                 [ ("/", "<fc=#5ebaf7><fn=1>\xe1db</fn></fc> <usedp>")
                 ]
                 [ "-L", "50"
                 , "-H", "75"
                 , "-S", "True"
                 , "-p", "3"
                 , "-m", "1"
                 ] 100
               , Run $ Network "eth0"
                 [ "-t", "<fc=#5ebaf7><fn=1>\xe5d8</fn></fc> <tx> <fc=#5ebaf7><fn=1>\xe5db</fn></fc> <rx>"
                 , "-L", "1000"
                 , "-H", "100000"
                 , "-l", "#869096"
                 , "-n", "#5ebaf7"
                 , "-h", "#fc8e88"
                 , "-S", "True"
                 , "-m", "8"
                 ] 10
               , Run $ PulseAudio
                 [ "-t", "<status> <volume>"
                 , "-S", "True"
                 , "-p", "3"
                 , "--"
                 , "--on", "<fc=#5ebaf7><fn=1>\xe050</fn></fc>"
                 , "--off", "<fc=#869096><fn=1>\xe04f</fn></fc>"
                 ]
               , Run $ Date "%Y/%m/%d %a %H:%M:%S" "date" 10
               ]
  , template = " %StdinReader% }{ %mpd%  %cpu% %cputemp%  %gpu% %gputemp%  %memory%  %disku%  %eth0%  %pulseaudio%  %date% "
  }

main :: IO ()
main = xmobar config
