-- My xmonad.hs
-- Import  --{{{1

import XMonad hiding (Tall)

import XMonad.Actions.DwmPromote
import XMonad.Actions.UpdatePointer

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.HintedTile
import XMonad.Layout.IM
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run

import XMonad.Prompt
import XMonad.Prompt.Shell

import Data.List
import System.Environment
import System.Exit
import System.IO.Unsafe

import qualified Data.Map as M
import qualified XMonad.StackSet as W




-- Lauout  --{{{1

myLayoutHook = avoidStruts $
  smartBorders $
  onWorkspace "5:gimp" gimpLayout $
  toggleLayouts Full (hintedTile Wide ||| hintedTile Tall)
  where
    hintedTile = HintedTile 1 (3/100) (3/5) TopLeft
    tabLayout  = tabbed shrinkText myTheme
    gimpLayout = named "Gimp" $
                 withIM (0.15) (Role "gimp-toolbox") $
                 reflectHoriz $ withIM (0.20) (Role "gimp-dock") $
                 reflectHoriz $ tabLayout




-- Management  --{{{1

myManageHook = composeAll
  [ className =? "stalonetray"   --> doIgnore
  , className =? "MPlayer"       --> doCenterFloat
  , className =? "Xmessage"      --> doCenterFloat
  , className =? "feh"           --> doCenterFloat
  , className =? "rdesktop"      --> doCenterFloat
  , className =? "XFontSel"      --> doFloat
  , className =? "Pidgin"        --> doFloat <+> doShiftAndGo "3:im"
  , className =? "Skype"         --> doFloat <+> doShiftAndGo "3:im"
  , className =? "XmBDFEdit"     --> doFloat <+> doShiftAndGo "4:gfx"
  , className =? "fontforge"     --> doFloat <+> doShiftAndGo "4:gfx"
  , className =? "Chrome"        --> doShiftAndGo "2:web"
  , className =? "Opera"         --> doShiftAndGo "2:web"
  , className =? "GQview"        --> doShiftAndGo "4:gfx"
  , className =? "Inkscape"      --> doShiftAndGo "4:gfx"
  , className =? "Gimp"          --> doShiftAndGo "5:gimp"
  , className =? "Gimp" <&&>
    role /=? "gimp-toolbox" <&&>
    role /=? "gimp-dock" <&&>
    role /=? "gimp-image-window" --> doCenterFloat
  ]
  where
    doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
    role = stringProperty "WM_WINDOW_ROLE"




-- Log  --{{{1

myLogHook h = do
  updatePointer (Relative 0.5 0.5)
  dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor "#ffffff" "#3366cc" . wrap (dzenIcon "square.xbm") " "
    , ppHidden          = dzenColor "#cccccc" ""        . wrap (dzenIcon "square2.xbm") " "
    , ppHiddenNoWindows = dzenColor "#666666" ""        . wrap "^p(8)" " "
    , ppSep             = dzenColor "#666666" "" " | "
    , ppWsSep           = ""
    , ppTitle           = dzenEscape
    , ppLayout          = \x -> case x of
                          "Wide" -> dzenIcon "layout-mirror-bottom.xbm"
                          "Tall" -> dzenIcon "layout-tall-right.xbm"
                          "Full" -> dzenIcon "layout-full.xbm"
                          "Gimp" -> dzenIcon "layout-im-tall.xbm"
                          _      -> x
    , ppOutput          = hPutStrLn h
    }
  where
    dzenIcon = wrap ("^i(" ++ (unsafePerformIO $ getEnv "HOME") ++ "/.dzen/") ")"




-- Theme  --{{{1

myFont = "-artwiz-snap-normal-r-normal--10-*-*-*-*-*-*-*, -mplus-gothic-medium-r-normal--12-*-*-*-*-*-*-*"

myTheme = defaultTheme
  { fontName            = myFont
  , activeColor         = "#003366"
  , activeBorderColor   = "#3366cc"
  , activeTextColor     = "#cccccc"
  , inactiveColor       = "#000000"
  , inactiveBorderColor = "#333333"
  , inactiveTextColor   = "#999999"
  , decoHeight          = 18
  }

myXPConfig = defaultXPConfig
  { font              = myFont
  , fgColor           = "#cccccc"
  , fgHLight          = "#cccc00"
  , bgColor           = "#000000"
  , bgHLight          = "#000000"
  , borderColor       = "#333333"
  , promptBorderWidth = 0
  , position          = Top
  , height            = 18
  , historyFilter     = deleteAllDuplicates
  }




-- Keys  --{{{1

myKeys conf = mkKeymap conf $
  [ ("M-<Return>",   dwmpromote)
  , ("M-<Space>",    sendMessage NextLayout)
  , ("M-S-<Return>", spawn $ XMonad.terminal conf)
  , ("M-S-<Space>",  setLayout $ XMonad.layoutHook conf)
  , ("M-f",          sendMessage ToggleLayout)

  , ("M-j",          windows W.focusDown)
  , ("M-k",          windows W.focusUp)
  , ("M-m",          windows W.focusMaster)
  , ("M-S-j",        windows W.swapDown)
  , ("M-S-k",        windows W.swapUp)
  , ("M-h",          sendMessage Shrink)
  , ("M-l",          sendMessage Expand)

  , ("M-t",          withFocused $ windows . W.sink)
  , ("M-,",          sendMessage $ IncMasterN 1)
  , ("M-.",          sendMessage $ IncMasterN (-1))

  , ("M-n",          refresh)
  , ("M-q",          spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-q",        io $ exitWith ExitSuccess)
  , ("M-S-c",        kill)

  , ("M-p",          shellPrompt myXPConfig)

  , ("M-=",          spawn "amixer -q set Master 5%+")
  , ("M--",          spawn "amixer -q set Master 5%-")
  , ("M-S-<Esc>",    spawn "sleep 1; xset dpms force off")

  , ("M-x c",        spawn "chromium-bin")
  , ("M-x e",        spawn "gvim")
  , ("M-x g",        spawn "gimp")
  , ("M-x o",        spawn "opera")
  , ("M-x v",        spawn "gqview")

  , ("M-\\",         spawn "ncmpcpp toggle")
  , ("M-[",          spawn "ncmpcpp prev")
  , ("M-]",          spawn "ncmpcpp next")

  , ("<Print>",      spawn "scrot -e 'mv $f ~/Desktop' '%y%m%d-%H%M%S.png'")
  ]
  ++

  [(m ++ k, windows $ f w)
    | (w, k) <- zip (XMonad.workspaces conf) (map show [1..9])
    , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]]




-- Main  --{{{1

main = do
  statusPipe <- spawnPipe myStatusbar
  xmonad $ defaultConfig
    { terminal           = "urxvt"
    , normalBorderColor  = "#333333"
    , focusedBorderColor = "#3366cc"
    , borderWidth        = 2

    , workspaces         = ["1:term", "2:web", "3:im", "4:gfx", "5:gimp"]
    , modMask            = mod4Mask
    , keys               = myKeys

    , layoutHook         = myLayoutHook
    , manageHook         = manageDocks <+> myManageHook
    , logHook            = myLogHook statusPipe
    , startupHook        = setWMName "LG3D"
    }
  where
    myStatusbar = "dzen2 -x 0 -y 0 -w 1280 -h 18 -ta l -fg '#cccccc' -bg '#000000' -fn '" ++ myFont ++
                  "' -e 'onstart=lower'"




-- __END__  --{{{1
-- vim: expandtab softtabstop=2 shiftwidth=2
-- vim: foldmethod=marker
