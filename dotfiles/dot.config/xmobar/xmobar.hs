import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Xmobar
-- import Xmobar.Plugins.Monitors.Common.Run (runM)
-- import Xmobar.Plugins.Monitors.Common.Types (MConfig, Monitor, io, mkMConfig)
-- import Xmobar.Plugins.Monitors.Common.Parsers (parseTemplate)

-- data CatIntDiv = CatIntDiv String FilePath Int Args Rate
--   deriving (Read, Show)
--
-- catIntConfig :: IO MConfig
-- catIntConfig = mkMConfig "<v>" ["v"]
--
-- runCatIntDiv :: FilePath -> Int -> [String] -> Monitor String
-- runCatIntDiv path base _ = parseTemplate <$> return $ io $ format <$> readFile path
--   where
--     format = show . (`div` base) . fromMaybe (0 :: Int) . readMaybe
--
-- instance Exec CatIntDiv where
--   alias (CatIntDiv name _ _ _ _) = name
--   start (CatIntDiv _ path base args rate) = runM args catIntConfig (runCatIntDiv path base) rate

config :: Config
config = defaultConfig
  { font = "xft:Monospace:pixelsize=11,Noto Sans CJK JP:pixelsize=11"
  , additionalFonts = [ "xft:FontAwesome:pixelsize=12"
                      ]
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
                 , "-P", "<fc=#5ebaf7><fn=1>\xf001</fn></fc>"
                 , "-S", "<fc=#869096><fn=1>\xf04d</fn></fc>"
                 , "-Z", "<fc=#869096><fn=1>\xf04c</fn></fc>"
                 , "-h", "127.0.0.1"
                 , "-p", "6600"
                 ] 10
               , Run $ Cpu
                 [ "-t", "<fc=#5ebaf7><fn=1>\xf013</fn></fc> <total>"
                 , "-L", "50"
                 , "-H", "75"
                 , "-n", "#5ebaf7"
                 , "-h", "#fc8e88"
                 , "-S", "True"
                 , "-p", "3"
                 ] 10
               , Run $ CoreTemp
                 [ "-t", "<fc=#5ebaf7><fn=1>\xf2c7</fn></fc> <core0>°C"
                 , "-m", "3"
                 ] 10
               , Run $ Memory
                 [ "-t", "<fc=#5ebaf7><fn=1>\xf2db</fn></fc> <usedratio>"
                 , "-L", "50"
                 , "-H", "75"
                 , "-n", "#5ebaf7"
                 , "-h", "#fc8e88"
                 , "-S", "True"
                 , "-p", "3"
                 ] 10
               , Run $ DiskU
                 [ ("/", "<fc=#5ebaf7><fn=1>\xf1c0</fn></fc> <usedp>")
                 ]
                 [ "-L", "50"
                 , "-H", "75"
                 , "-S", "True"
                 , "-p", "3"
                 , "-m", "1"
                 ] 100
               , Run $ Network "eth0"
                 [ "-t", "<fc=#5ebaf7><fn=1>\xf062</fn></fc> <tx> <fc=#5ebaf7><fn=1>\xf063</fn></fc> <rx>"
                 , "-L", "1000"
                 , "-H", "100000"
                 , "-l", "#869096"
                 , "-n", "#5ebaf7"
                 , "-h", "#fc8e88"
                 , "-S", "True"
                 , "-d", "1"
                 , "-m", "9"
                 ] 10
               , Run $ Alsa "pulse" "Master"
                 [ "-t", "<status> <volume>"
                 , "-S", "True"
                 , "-p", "3"
                 , "--"
                 , "--on", "<fn=1>\xf028</fn>"
                 , "--onc", "#5ebaf7"
                 , "--off", "<fn=1>\xf026  </fn>"
                 , "--offc", "#869096"
                 ]
               , Run $ Date "%Y/%m/%d %a %H:%M:%S" "date" 10
               ]
  , template = " %StdinReader% }{ %mpd%  %cpu%  %coretemp%  %memory%  %disku%  %eth0%  %alsa:pulse:Master%  %date% "
  }

main :: IO ()
main = xmobar config
