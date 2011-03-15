-- My xmonad.hs
-- Import  --{{{1

import XMonad hiding (Tall)

import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.Promote

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating

import XMonad.Util.EZConfig
import XMonad.Util.Run

import XMonad.Prompt
import XMonad.Prompt.Shell

import Data.List
import System.Environment
import System.Exit

import Foreign.C.String (castCharToCChar)
import Graphics.X11.Xlib.Extras (changeProperty8, propModeReplace)

import qualified Data.Map as M
import qualified XMonad.StackSet as W




-- Hooks  --{{{1
-- Lauout  --{{{2

myLayoutHook = avoidStrutsOn [U,R,L] $ smartBorders $
  onWorkspace "web" (toggleLayouts Full (webIM $ tallLayout ||| wideLayout)) $
  onWorkspace "gimp" (toggleLayouts Full (gimpIM tabbedLayout)) $
  toggleLayouts Full (tallLayout ||| wideLayout)
  where
    basicLayout = layoutHintsToCenter $ ResizableTall 1 (2/100) (1/2) []
    tallLayout = named "Tall" $ basicLayout
    wideLayout = named "Wide" $ Mirror basicLayout
    tabbedLayout = named "Tabbed" $ tabbed shrinkText myTheme
    webIM = reflectHoriz . withIM (0.15) ((ClassName "Pidgin" `And` Role "buddy_list") `Or`
                                          (ClassName "Skype" `And` Role "MainWindow")) .
            reflectHoriz . trackFloating
    gimpIM = named "Gimp" . withIM (0.15) (Role "gimp-toolbox") .
             reflectHoriz . withIM (0.20) (Role "gimp-dock") .
             reflectHoriz . trackFloating




-- Manage  --{{{2

myManageHook = composeOne
  [ isDialog                                -?> doFloat
  , isFullscreen                            -?> doFullFloat
  , className  =? "MPlayer"                 -?> doCenterFloat
  , className  =? "Smplayer"                -?> doCenterFloat
  , className  =? "Vlc"                     -?> doCenterFloat
  , className  =? "XFontSel"                -?> doCenterFloat
  , className  =? "Xmessage"                -?> doCenterFloat
  , className  =? "feh"                     -?> doCenterFloat
  , className  =? "qemu-system-x86_64"      -?> doCenterFloat
  , className  =? "rdesktop"                -?> doCenterFloat
  , className  =? "Pidgin"                  -?> doShiftAndGo "web"
  , className  =? "Skype"
    <&&> fmap (isInfixOf "(Beta)") title    -?> doShiftAndGo "web" <+> addProperty "WM_WINDOW_ROLE" "MainWindow"
  , className  =? "Skype"                   -?> doShiftAndGo "web"
  , className  =? "libreoffice-startcenter" -?> doShiftAndGo "misc"
  , className  =? "GQview"                  -?> doShiftAndGo "media"
  , className  =? "Inkscape"                -?> doShiftAndGo "media"
  , className  =? "XmBDFEdit"               -?> doShiftAndGo "media" <+> doFloat
  , className  =? "fontforge"               -?> doShiftAndGo "media" <+> doFloat
  , className  =? "Gimp"
    <&&> role /=? "gimp-toolbox"
    <&&> role /=? "gimp-dock"
    <&&> role /=? "gimp-image-window"       -?> doShiftAndGo "gimp" <+> doFloat
  , className  =? "Gimp"                    -?> doShiftAndGo "gimp"
  ]
  where
    doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
    role = stringProperty "WM_WINDOW_ROLE"
    addProperty prop value = ask >>= \w -> do
      liftX $ withDisplay $ \d -> do
        a <- io $ internAtom d prop False
        t <- io $ internAtom d "STRING" False
        io $ changeProperty8 d w a t propModeReplace (map castCharToCChar value)
      idHook




-- Log  --{{{2

myLogHook h = do
  home <- io $ getEnv "HOME"
  float <- withWindowSet isFloat
  let dzenIcon = wrap ("^i(" ++ home ++ "/.dzen/") ")"
      layoutIcon x = case x of
        "Tall" -> dzenIcon "layout-tall-black.xbm"
        "Wide" -> dzenIcon "layout-mirror-black.xbm"
        "Full" -> dzenIcon "layout-full-black.xbm"
        _      -> x
  dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor "#222222" "#63afaf" . wrap (dzenIcon "square.xbm") " "
    , ppHidden          = dzenColor "#dcdccc" "" . wrap (dzenIcon "square3.xbm") " "
    , ppHiddenNoWindows = wrap "^p(8)" " "
    , ppUrgent          = dzenColor "#dcdccc" "#444444" . wrap (dzenIcon "square3.xbm") " "
    , ppSep             = dzenColor "#666666" "" " | "
    , ppWsSep           = ""
    , ppTitle           = if float then (dzenIcon "square.xbm" ++) . dzenEscape else dzenEscape
    , ppLayout          = dzenColor "#63afaf" "" . layoutIcon . last . words
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
  , ("M-S-<Return>", safeSpawnProg $ XMonad.terminal conf)
  , ("M-S-<Space>",  setLayout $ XMonad.layoutHook conf)

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

  , ("M-b",          sendMessage $ ToggleStrut D)
  , ("M-f",          sendMessage ToggleLayout)

  , ("M-S-c",        kill)
  , ("M-S-q",        io $ exitWith ExitSuccess)
  , ("M-q",          spawn "xmonad --recompile && xmonad --restart")
  , ("M-r",          refresh)

  , ("M-p",          shellPrompt myXPConfig)

  , ("M-M1-j",       withFocused $ snapMove D Nothing)
  , ("M-M1-k",       withFocused $ snapMove U Nothing)
  , ("M-M1-h",       withFocused $ snapMove L Nothing)
  , ("M-M1-l",       withFocused $ snapMove R Nothing)

  , ("M-S-<Esc>",    spawn "sleep 1; xset dpms force off")

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
              ++ " -fn '" ++ myFont ++ "' -dock -e 'onstart=lower'"

main = do
  statusPipe <- spawnPipe myStatusbar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { terminal           = "urxvt"
    , normalBorderColor  = "#666666"
    , focusedBorderColor = "#63afaf"
    , borderWidth        = 2

    , workspaces         = ["work", "web", "misc", "media", "gimp"]
    , modMask            = mod4Mask
    , keys               = myKeys

    , layoutHook         = myLayoutHook
    , manageHook         = manageDocks <+> myManageHook
    , logHook            = myLogHook statusPipe
    , startupHook        = setWMName "LG3D"

    , focusFollowsMouse  = True
    }




-- __END__  --{{{1
-- vim: expandtab softtabstop=2 shiftwidth=2
-- vim: foldmethod=marker
