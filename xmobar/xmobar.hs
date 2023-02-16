import Xmobar

import Monitors.CatNum (CatNum(..))
import Monitors.PulseAudio (PulseAudio(..))
import System.Environment (getExecutablePath)
import System.FilePath.Posix (takeDirectory)

myPrimaryColor = "#5686d7"
mySecondaryColor = "#698aa8"
myThirdColor = "#cf6950"
myMutedColor = "#3f576e"
myForegroundColor = "#d1dbe7"
myBackgroundColor = "#22262b"

makeConfig :: FilePath -> Config
makeConfig configDirectory = defaultConfig
  { font = "xft:M PLUS Code Latin 60:pixelsize=11,Noto Sans CJK JP:pixelsize=11,Noto Emoji:pixelsize=11"
  , additionalFonts = [ "xft:Material Icons:pixelsize=16" ]
  , bgColor = myBackgroundColor
  , fgColor = myForegroundColor
  , position = Top
  , overrideRedirect = True
  , commands = [ Run StdinReader
               , Run $ MPD
                 [ "-t", "<statei> <title><fc=" ++ mySecondaryColor ++ ">/</fc><artist> <fc=" ++ mySecondaryColor ++ ">(<volume>%)</fc>"
                 , "-M", "20"
                 , "-e", "..."
                 , "--"
                 , "-P", "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe405</fn></fc>"
                 , "-S", "<fc=" ++ mySecondaryColor ++ "><fn=1>\xe047</fn></fc>"
                 , "-Z", "<fc=" ++ mySecondaryColor ++ "><fn=1>\xe034</fn></fc>"
                 , "-h", "127.0.0.1"
                 , "-p", "6600"
                 ] 10
               , Run $ Cpu
                 [ "-t", "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe1af</fn></fc> <total>"
                 , "-L", "50"
                 , "-H", "75"
                 , "-n", myPrimaryColor
                 , "-h", myThirdColor
                 , "-S", "True"
                 , "-p", "3"
                 ] 10
               , Run $ CatNum "cputemp"
                 [ "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon1/temp1_input"
                 ]
                 [ "-t", "<fc=" ++ mySecondaryColor ++ ">(</fc><n0><fc=" ++ mySecondaryColor ++ ">)</fc>"
                 , "-L", "50000"
                 , "-H", "70000"
                 , "-l", mySecondaryColor
                 , "-n", myPrimaryColor
                 , "-h", myThirdColor
                 , "-m", "4"
                 , "--"
                 , "--divier", "1000"
                 , "--suffix", "°C"
                 ] 10
               , Run $ Memory
                 [ "-t", "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe322</fn></fc> <usedratio>"
                 , "-L", "50"
                 , "-H", "75"
                 , "-n", myPrimaryColor
                 , "-h", myThirdColor
                 , "-S", "True"
                 , "-p", "3"
                 ] 10
               , Run $ DiskU
                 [ ("/", "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe1db</fn></fc> <usedp>")
                 ]
                 [ "-L", "50"
                 , "-H", "75"
                 , "-S", "True"
                 , "-p", "3"
                 , "-m", "1"
                 ] 100
               , Run $ Network "eth0"
                 [ "-t", "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe5d8</fn></fc> <tx> <fc=" ++ myPrimaryColor ++ "><fn=1>\xe5db</fn></fc> <rx>"
                 , "-L", "10000"
                 , "-H", "1000000"
                 , "-l", mySecondaryColor
                 , "-n", myPrimaryColor
                 , "-h", myThirdColor
                 , "-S", "True"
                 , "-m", "8"
                 ] 10
               , Run $ PulseAudio
                 [ "-t", "<status> <volume>"
                 , "-S", "True"
                 , "-p", "3"
                 , "--"
                 , "--on", "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe050</fn></fc>"
                 , "--off", "<fc=" ++ mySecondaryColor ++ "><fn=1>\xe04f</fn></fc>"
                 ]
               , Run $ Date "%Y-%m-%d %a %H:%M:%S" "date" 10
               ]
  , template = " %StdinReader% }{ %mpd%  %cpu% %cputemp%  %memory%  %disku%  %eth0%  %pulseaudio%  %date% "
  , iconRoot = configDirectory <> "/icons"
  }

main :: IO ()
main = do
  configDirectory <- takeDirectory <$> getExecutablePath
  let config = makeConfig configDirectory
  xmobar config
