-- My xmonad.hs
-- Import  --{{{1

import XMonad hiding (Tall)

import XMonad.Actions.CycleWS
import XMonad.Actions.Promote
import XMonad.Actions.FloatSnap

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.IM
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating

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




-- Hook  --{{{1
-- Lauout  --{{{2

myLayoutHook = avoidStruts $
  onWorkspace "gimp" gimpLayout $
  toggleLayouts (noBorders Full) (smartBorders $ tallLayout ||| wideLayout)
  where
    resizeTall = ResizableTall 1 (3/100) (1/2) []
    tallLayout = named "Tall" $ resizeTall
    wideLayout = named "Wide" $ Mirror $ resizeTall
    tabbedLayout = named "Tabbed" $ tabbed shrinkText myTheme
    gimpLayout = named "Gimp" $ withIM (0.15) (Role "gimp-toolbox") $
                 reflectHoriz $ withIM (0.20) (Role "gimp-dock") $
                 reflectHoriz $ trackFloating $ tabbedLayout




-- Manage  --{{{2

myManageHook = composeOne
  [ isFullscreen                         -?> doFullFloat
  , isDialog                             -?> doCenterFloat
  , appName    =? "messageconsoledialog" -?> doCenterFloat
  , className  =? "Evince"               -?> doCenterFloat
  , className  =? "MPlayer"              -?> doCenterFloat
  , className  =? "Smplayer"             -?> doCenterFloat
  , className  =? "XDvi"                 -?> doCenterFloat
  , className  =? "XFontSel"             -?> doCenterFloat
  , className  =? "Xmessage"             -?> doCenterFloat
  , className  =? "feh"                  -?> doCenterFloat
  , className  =? "qemu-system-x86_64"   -?> doCenterFloat
  , className  =? "rdesktop"             -?> doCenterFloat
  , className  =? "Gimp"
    <&&> role /=? "gimp-toolbox"
    <&&> role /=? "gimp-dock"
    <&&> role /=? "gimp-image-window"    -?> doFloat
  , className  =? "Pidgin"               -?> doFloat <+> doShiftAndGo "web"
  , className  =? "Skype"                -?> doFloat <+> doShiftAndGo "web"
  , className  =? "XmBDFEdit"            -?> doFloat <+> doShiftAndGo "media"
  , className  =? "fontforge"            -?> doFloat <+> doShiftAndGo "media"
  , className  =? "Google-chrome"        -?> doShiftAndGo "web"
  , className  =? "Opera"                -?> doShiftAndGo "web"
  , className  =? "GQview"               -?> doShiftAndGo "media"
  , className  =? "Inkscape"             -?> doShiftAndGo "media"
  , className  =? "Gimp"                 -?> doShiftAndGo "gimp"
  ]
  where
    doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
    role = stringProperty "WM_WINDOW_ROLE"




-- Log  --{{{2

myLogHook h = do
  home <- io $ getEnv "HOME"
  floatp <- withWindowSet isFloat
  let dzenIcon = wrap ("^i(" ++ home ++ "/.dzen/") ")"
  dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor "#222222" "#63afaf" . wrap (dzenIcon "square.xbm") " "
    , ppHidden          = dzenColor "#dcdccc" "" . wrap (dzenIcon "square3.xbm") " "
    , ppHiddenNoWindows = wrap "^p(8)" " "
    , ppSep             = dzenColor "#666666" "" " | "
    , ppWsSep           = ""
    , ppTitle           = if floatp then (dzenIcon "square.xbm" ++) . dzenEscape else dzenEscape
    , ppLayout          = dzenColor "#63afaf" ""
    , ppOutput          = hPutStrLn h
    }
  where
    isFloat ws = case W.peek ws of
      Nothing -> return False
      Just w  -> return $ M.member w $ W.floating ws




-- Handle Event  --{{{2

-- Don't focus follows mouse when Cross layout.
-- myHandleEventHook = followOnlyIf $ fmap (\x -> case x of
--                       "Cross" -> False
--                       _       -> True
--                     ) currentLayout
--   where
--     currentLayout = withWindowSet $ return . description . W.layout . W.workspace . W.current




-- Theme  --{{{1

myFont = "-artwiz-gelly-medium-r-normal--10-*-*-*-*-*-*-*, -mplus-gothic-medium-r-normal--10-*-*-*-*-*-*-*"

myTheme = defaultTheme
  { fontName            = myFont
  , activeColor         = "#63afaf"
  , activeBorderColor   = "#63afaf"
  , activeTextColor     = "#222222"
  , inactiveColor       = "#666666"
  , inactiveBorderColor = "#666666"
  , inactiveTextColor   = "#dcdccc"
  , decoHeight          = 14
  }

myXPConfig = defaultXPConfig
  { font              = myFont
  , fgColor           = "#dcdccc"
  , bgColor           = "#222222"
  , fgHLight          = "#63afaf"
  , bgHLight          = "#222222"
  , borderColor       = "#000000"
  , promptBorderWidth = 0
  , position          = Top
  , height            = 14
  , historyFilter     = deleteAllDuplicates
  }




-- Keys  --{{{1

myKeys conf = mkKeymap conf $
  [ ("M-<Return>",   promote)
  , ("M-<Space>",    sendMessage NextLayout)
  , ("M-S-<Return>", spawn $ XMonad.terminal conf)
  , ("M-S-<Space>",  setLayout $ XMonad.layoutHook conf)

  , ("M-b",          sendMessage ToggleStruts)
  , ("M-f",          sendMessage ToggleLayout)

  , ("M-<Tab>",      moveTo Next NonEmptyWS)
  , ("M-S-<Tab>",    moveTo Prev NonEmptyWS)

  , ("M-j",          windows W.focusDown)
  , ("M-k",          windows W.focusUp)
  , ("M-m",          windows W.focusMaster)
  , ("M-S-j",        windows W.swapDown)
  , ("M-S-k",        windows W.swapUp)
  , ("M-h",          sendMessage Shrink)
  , ("M-l",          sendMessage Expand)

  , ("M-S-h",        sendMessage MirrorExpand)
  , ("M-S-l",        sendMessage MirrorShrink)

  , ("M-t",          withFocused $ windows . W.sink)
  , ("M-,",          sendMessage $ IncMasterN 1)
  , ("M-.",          sendMessage $ IncMasterN (-1))

  , ("M-n",          refresh)
  , ("M-q",          spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-q",        io $ exitWith ExitSuccess)
  , ("M-S-c",        kill)

  , ("M-p",          shellPrompt myXPConfig)

  , ("M-M1-j",       withFocused $ snapMove D Nothing)
  , ("M-M1-k",       withFocused $ snapMove U Nothing)
  , ("M-M1-h",       withFocused $ snapMove L Nothing)
  , ("M-M1-l",       withFocused $ snapMove R Nothing)

  , ("M-C-<Space>",  spawn "feh --bg-fill \"$(find ~/Pictures/wallpaper/anime -type f -name '*.jpg' -o -name '*.png' | shuf -n 1)\"")

  , ("M-=",          safeSpawn "amixer" ["-q", "set", "Master", "5%+"])
  , ("M--",          safeSpawn "amixer" ["-q", "set", "Master", "5%-"])

  , ("M-\\",         safeSpawn "ncmpcpp" ["toggle"])
  , ("M-[",          safeSpawn "ncmpcpp" ["prev"])
  , ("M-]",          safeSpawn "ncmpcpp" ["next"])

  , ("M-x c",        safeSpawnProg "google-chrome")
  , ("M-x e",        safeSpawnProg "gvim")
  , ("M-x g",        safeSpawnProg "gimp")
  , ("M-x j",        safeSpawnProg "jd")
  , ("M-x o",        safeSpawnProg "opera")
  , ("M-x v",        safeSpawnProg "gqview")

  , ("M-<Print>",    safeSpawn "scrot" ["-e", "mv $f ~/Desktop", "%y%m%d-%H%M%S.png"])
  ]
  ++
  [ (m ++ k, windows $ f w)
  | (w, k) <- zip (XMonad.workspaces conf) (map show [1..9])
  , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
  ]




-- Main  --{{{1

myStatusbar = "dzen2 -x 0 -y 0 -w 1920 -h 14 -ta l -fg '#dcdccc' -bg '#222222'"
              ++ " -fn '" ++ myFont ++ "' -e 'onstart=lower'"

main = do
  statusPipe <- spawnPipe myStatusbar
  xmonad $ defaultConfig
    { terminal           = "urxvt"
    , normalBorderColor  = "#666666"
    , focusedBorderColor = "#63afaf"
    , borderWidth        = 2

    , workspaces         = ["work", "web", "misc", "media", "gimp"]
    , modMask            = mod4Mask
    , keys               = myKeys

    , layoutHook         = myLayoutHook
    , manageHook         = manageDocks <+> myManageHook
    , logHook            = fadeInactiveLogHook 0xdddddddd <+> myLogHook statusPipe
    , startupHook        = setWMName "LG3D"

    , focusFollowsMouse  = True
    }




-- __END__  --{{{1
-- vim: expandtab softtabstop=2 shiftwidth=2
-- vim: foldmethod=marker
