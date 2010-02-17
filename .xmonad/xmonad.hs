-- My xmonad.hs
-- Import  --{{{1

import XMonad hiding (Tall)

import XMonad.Actions.UpdatePointer

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Tabbed
import XMonad.Layout.HintedTile

import XMonad.Util.EZConfig
import XMonad.Util.Run

import XMonad.Prompt
import XMonad.Prompt.Shell

import System.Exit


import qualified Data.Map as M
import qualified XMonad.StackSet as W




-- Management  --{{{1

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myManageHook = composeAll
  [ className =? "MPlayer"        --> doCenterFloat
  , className =? "Xmessage"       --> doCenterFloat
  , className =? "feh"            --> doCenterFloat
  , className =? "fontforge"      --> doFloat
  , className =? "XmBDFEdit"      --> doFloat
  , className =? "Gimp"           --> doShiftAndGo "9"
  , resource  =? "desktop_window" --> doIgnore
  , resource  =? "kdesktop"       --> doIgnore
  ]

doShiftAndGo :: WorkspaceId -> ManageHook
doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws




-- Theme  --{{{1

myTheme = defaultTheme
  { fontName            = "-xos4-terminus-medium-r-normal--12-*-*-*-*-*-*-*, -mplus-gothic-medium-r-normal--12-*-*-*-*-*-*-*"
  , activeColor         = "#000000"
  , activeBorderColor   = "orange"
  , activeTextColor     = "#cccccc"
  , inactiveColor       = "#000000"
  , inactiveBorderColor = "#333333"
  , inactiveTextColor   = "#999999"
  , decoHeight          = 16
  }


myXPConfig = defaultXPConfig
  { font              = "-xos4-terminus-medium-r-normal--12-*-*-*-*-*-*-*"
  , fgColor           = "#cccccc"
  , fgHLight          = "#cccc00"
  , bgColor           = "#000000"
  , bgHLight          = "#000000"
  , borderColor       = "#333333"
  , promptBorderWidth = 0
  , position          = Top
  , height            = 16
  }




-- Lauout  --{{{1

myLayoutHook = avoidStruts $
  smartBorders $
  onWorkspace "9" gimpLayout $
  hintedTile Wide ||| hintedTile Tall ||| Full

  where
    hintedTile  = HintedTile 1 (3/100) (3/5) TopLeft
    tabLayout   = tabbed shrinkText myTheme
    gimpLayout  = withIM (0.15) (Role "gimp-toolbox") $
                  reflectHoriz $ withIM (0.20) (Role "gimp-dock") $ tabLayout




-- Log  --{{{1

myLogHook xmproc = dynamicLogWithPP $ xmobarPP
  { ppOutput  = hPutStrLn xmproc
  , ppCurrent = xmobarColor "#cccc00" "" . wrap "[" "]"
  , ppTitle   = xmobarColor "#00cc00" "" . shorten 80
  , ppLayout  = (\x -> case x of
    "IM ReflectX IM Tabbed Simplest" -> "GIMP"
    "Tabbed Simplest"                -> "Tab"
    _                                -> x
  )
  }




-- Keys  --{{{1

myKeys conf = mkKeymap conf $
  [ ("M-<Return>",   windows W.swapMaster)
  , ("M-<Space>",    sendMessage NextLayout)
  , ("M-S-<Return>", spawn $ XMonad.terminal conf)
  , ("M-S-<Space>",  setLayout $ XMonad.layoutHook conf)

  , ("M-<Tab>",      windows W.focusDown)
  , ("M-S-<Tab>",    windows W.focusUp)
  , ("M-j",          windows W.focusDown)
  , ("M-k",          windows W.focusUp)
  , ("M-m",          windows W.focusMaster)
  , ("M-S-j",        windows W.swapDown)
  , ("M-S-k",        windows W.swapUp)
  , ("M-h",          sendMessage Shrink)
  , ("M-l",          sendMessage Expand)

  , ("M-t",          withFocused $ windows . W.sink)
  , ("M-,",          sendMessage (IncMasterN 1))
  , ("M-.",          sendMessage (IncMasterN (-1)))

  , ("M-p",          shellPrompt myXPConfig)
  , ("M-n",          refresh)
  , ("M-q",          spawn "xmonad --recompile; xmonad --restart")
  , ("M-S-q",        io (exitWith ExitSuccess))
  , ("M-S-c",        kill)

  , ("M-=",          spawn "amixer -q set Master 2dB+")
  , ("M--",          spawn "amixer -q set Master 2dB-")
  , ("M-0",          spawn "amixer -q set Master toggle")
  , ("M-<Esc>",      spawn "sleep 1; xset dpms force off")

  , ("M-x o",        spawn "opera")
  , ("M-x v",        spawn "gqview")
  , ("M-x g",        spawn "gimp")
  , ("M-x f",        spawn "fontforge")
  , ("M-x i",        spawn "inkscape")

  , ("M-\\",         spawn "ncmpcpp toggle")
  , ("M-[",          spawn "ncmpcpp prev")
  , ("M-]",          spawn "ncmpcpp next")
  ]
  ++

  [(m ++ k, windows $ f w)
    | (w, k) <- zip (XMonad.workspaces conf) (map show [1..9])
    , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]




-- Main  --{{{1

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { terminal           = "urxvt"
    , modMask            = mod4Mask
    , borderWidth        = 1
    , normalBorderColor  = "#333333"
    , focusedBorderColor = "orange"

    , workspaces         = myWorkspaces
    , keys               = myKeys

    , startupHook        = setWMName "LG3D"
    , layoutHook         = myLayoutHook
    , manageHook         = myManageHook
    , logHook            = myLogHook xmproc
    }




-- __END__  --{{{1
-- vim: expandtab softtabstop=2 shiftwidth=2
-- vim: foldmethod=marker
