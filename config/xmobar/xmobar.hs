import Monitors.CatNum (CatNum (..))
import Monitors.PulseAudio (PulseAudio (..))
import System.Environment (getExecutablePath)
import System.FilePath.Posix (takeDirectory)
import Xmobar

myPrimaryColor = "#5686d7"

mySecondaryColor = "#cf6950"

myThirdColor = "#698aa8"

myLightGrayColor = "#3f576e"

myDarkGrayColor = "#334454"

myForegroundColor = "#d1dbe7"

myBackgroundColor = "#22262b"

makeConfig :: FilePath -> Config
makeConfig configDirectory =
  defaultConfig
    { font = "Monospace 9",
      additionalFonts = ["Material Icons 12"],
      bgColor = myBackgroundColor,
      fgColor = myForegroundColor,
      position = TopH 20,
      border = NoBorder,
      overrideRedirect = True,
      commands =
        [ Run UnsafeStdinReader,
          Run $
            MPD
              [ "-t",
                "<statei> <title><fc=" ++ myThirdColor ++ ">/</fc><artist> <fc=" ++ myThirdColor ++ ">(<volume>%)</fc>",
                "-M",
                "20",
                "-e",
                "...",
                "--",
                "-P",
                "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe405</fn></fc>",
                "-S",
                "<fc=" ++ myThirdColor ++ "><fn=1>\xe047</fn></fc>",
                "-Z",
                "<fc=" ++ myThirdColor ++ "><fn=1>\xe034</fn></fc>",
                "-h",
                "127.0.0.1",
                "-p",
                "6600"
              ]
              10,
          Run $
            Cpu
              [ "-t",
                "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe1af</fn></fc> <total>",
                "-L",
                "50",
                "-H",
                "75",
                "-n",
                myPrimaryColor,
                "-h",
                mySecondaryColor,
                "-S",
                "True",
                "-p",
                "3"
              ]
              10,
          Run $
            CatNum
              "cputemp"
              [ "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon1/temp1_input"
              ]
              [ "-t",
                "<fc=" ++ myThirdColor ++ ">(</fc><n0><fc=" ++ myThirdColor ++ ">)</fc>",
                "-L",
                "50000",
                "-H",
                "70000",
                "-l",
                myThirdColor,
                "-n",
                myPrimaryColor,
                "-h",
                mySecondaryColor,
                "-m",
                "4",
                "--",
                "--divier",
                "1000",
                "--suffix",
                "°C"
              ]
              10,
          Run $
            Memory
              [ "-t",
                "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe322</fn></fc> <usedratio>",
                "-L",
                "50",
                "-H",
                "75",
                "-n",
                myPrimaryColor,
                "-h",
                mySecondaryColor,
                "-S",
                "True",
                "-p",
                "3"
              ]
              10,
          Run $
            DiskU
              [ ("/", "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe1db</fn></fc> <usedp>")
              ]
              [ "-L",
                "50",
                "-H",
                "75",
                "-S",
                "True",
                "-p",
                "3",
                "-m",
                "1"
              ]
              100,
          Run $
            Network
              "eth0"
              [ "-t",
                "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe5d8</fn></fc> <tx> <fc=" ++ myPrimaryColor ++ "><fn=1>\xe5db</fn></fc> <rx>",
                "-L",
                "10000",
                "-H",
                "1000000",
                "-l",
                myThirdColor,
                "-n",
                myPrimaryColor,
                "-h",
                mySecondaryColor,
                "-S",
                "True",
                "-m",
                "8"
              ]
              10,
          Run $
            PulseAudio
              [ "-t",
                "<status> <volume>",
                "-S",
                "True",
                "-p",
                "3",
                "--",
                "--on",
                "<fc=" ++ myPrimaryColor ++ "><fn=1>\xe050</fn></fc>",
                "--off",
                "<fc=" ++ myThirdColor ++ "><fn=1>\xe04f</fn></fc>"
              ],
          Run $ Date "%Y-%m-%d %a %H:%M:%S" "date" 10
        ],
      template = "} %UnsafeStdinReader% { %mpd%  %cpu% %cputemp%  %memory%  %disku%  %eth0%  %pulseaudio%  %date% ",
      iconRoot = configDirectory <> "/icons"
    }

main :: IO ()
main = do
  configDirectory <- takeDirectory <$> getExecutablePath
  let config = makeConfig configDirectory
  xmobar config
