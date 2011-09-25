-- My xmonad.hs
-- Import  --{{{1

import XMonad hiding (Tall)
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.Promote
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.IM
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TrackFloating
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W

import Data.List (isInfixOf)
import Graphics.X11.Xlib.Extras (changeProperty8, propModeReplace)
import System.Directory (getHomeDirectory)
import System.Exit (exitWith, ExitCode(..))
import Text.Printf (printf)

import qualified Data.Map as M




-- Lauout  --{{{1

myLayoutHook = avoidStruts $ smartBorders $
  onWorkspace "gimp" (toggleLayouts Full $ gimpIM tabbedLayout) $
  toggleLayouts Full $ imLayout $ tallLayout ||| wideLayout
  where
    basicLayout = ResizableTall 1 (2/100) (1/2) []
    tallLayout = named "Tall" $ basicLayout
    wideLayout = named "Wide" $ Mirror basicLayout
    tabbedLayout = named "Tabbed" $ tabbed shrinkText myTheme
    imLayout = reflectHoriz . withIM 0.15 imWindows
             . reflectHoriz . trackFloating
    imWindows = foldr1 Or [ ClassName "Pidgin" `And` Role "buddy_list"
                          , ClassName "Skype" `And` Role "MainWindow"
                          ]
    gimpIM = named "Gimp" . withIM 0.15 (Role "gimp-toolbox")
           . reflectHoriz . withIM 0.20 (Role "gimp-dock")
           . reflectHoriz . trackFloating




-- Manage  --{{{1

myManageHook = composeOne
  [ isDialog                               -?> doCenterFloat
  , isFullscreen                           -?> doFullFloat
  , className =? "Uim-tomoe-gtk"           -?> doFloat
  , className =? "Firefox"
    <&&> appName /=? "Navigator"           -?> doFloat
  , className =? "qemu-system-x86_64"      -?> doFloat
  , className =? "rdesktop"                -?> doFloat
  , className =? "MPlayer"                 -?> doCenterFloat
  , className =? "XFontSel"                -?> doCenterFloat
  , className =? "Xmessage"                -?> doCenterFloat
  , className =? "feh"                     -?> doCenterFloat
  , className =? "GQview"                  -?> doShiftAndGo "misc"
  , className =? "Inkscape"                -?> doShiftAndGo "misc"
  , className =? "fontforge"               -?> doShiftAndGo "misc" <+> doFloat
  , className =? "libreoffice-startcenter" -?> doShiftAndGo "misc"
  , className =? "Gimp"
    <&&> role /=? "gimp-toolbox"
    <&&> role /=? "gimp-dock"
    <&&> role /=? "gimp-image-window"      -?> doShiftAndGo "gimp" <+> doFloat
  , className =? "Gimp"                    -?> doShiftAndGo "gimp"
  , className =? "Skype"
    <&&> fmap (isInfixOf "(Beta)") title   -?> addProperty "WM_WINDOW_ROLE" "MainWindow"
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
    addProperty prop value = do
      d <- liftX $ asks display
      w <- ask
      a <- io $ internAtom d prop False
      t <- io $ internAtom d "STRING" False
      io $ changeProperty8 d w a t propModeReplace $ map (fromIntegral . fromEnum) value
      idHook




-- Log  --{{{1

myLogHook h = do
  home  <- io getHomeDirectory
  float <- withWindowSet isFloat
  let dzenIcon     = wrap ("^i(" ++ home ++ "/.dzen/") ")"
      layoutIcon x = case last $ words x of
        "Tall" -> dzenIcon "layout-tall-black.xbm"
        "Wide" -> dzenIcon "layout-mirror-black.xbm"
        "Full" -> dzenIcon "layout-full-black.xbm"
        _      -> x
  dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor "#222222" "#63afaf" . wrap (dzenIcon "square.xbm") " "
    , ppHidden          = dzenColor "#dcdccc" "" . wrap (dzenIcon "square3.xbm") " "
    , ppHiddenNoWindows = wrap "^p(8)" " "
    , ppUrgent          = dzenColor "#222222" "#cc9393" . wrap (dzenIcon "square3.xbm") " "
    , ppSep             = dzenColor "#666666" "" " | "
    , ppWsSep           = ""
    , ppTitle           = if float then (dzenIcon "square.xbm" ++) . dzenEscape else dzenEscape
    , ppLayout          = dzenColor "#63afaf" "" . layoutIcon
    , ppOutput          = hPutStrLn h
    }
  where
    isFloat ws = return $ case W.peek ws of
      Nothing -> False
      Just w  -> M.member w $ W.floating ws




-- Theme  --{{{1

myFont = "-artwiz-glisp-medium-r-normal--*-*-*-*-*-*-*-*, -mplus-gothic-medium-r-normal--10-*-*-*-*-*-*-*"

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
  , ("M-S-<Return>", safeSpawnProg $ terminal conf)
  , ("M-S-<Space>",  setLayout $ layoutHook conf)

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

  , ("M-=",          safeSpawn "amixer" ["-q", "set", "Master", "5%+"])
  , ("M--",          safeSpawn "amixer" ["-q", "set", "Master", "5%-"])
  , ("M-0",          safeSpawn "amixer" ["-q", "set", "Master", "toggle"])

  , ("M-\\",         safeSpawn "ncmpcpp" ["toggle"])
  , ("M-[",          safeSpawn "ncmpcpp" ["prev"])
  , ("M-]",          safeSpawn "ncmpcpp" ["next"])

  , ("M-<Esc> s",    spawn "sleep 1; xset dpms force off")
  , ("M-<Esc> y",    do home <- io getHomeDirectory
                        safeSpawn "scrot"
                                  ["-e", printf "mv $f %s/Desktop" home, "%Y-%m-%d_%H-%M-%S.png"])
  ]
  ++
  [ ("M-x " ++ m ++ k, safeSpawnProg a)
  | (k, a) <- [ ("c", "google-chrome")
              , ("e", "gvim")
              , ("f", "firefox-bin")
              , ("g", "gimp")
              , ("j", "jd")
              , ("o", "opera-next")
              , ("v", "gqview")
              ]
  , m <- [ "", "M-"]
  ]
  ++
  [ (m ++ k, windows $ f w)
  | (w, k) <- zip (XMonad.workspaces conf) (map show [1..9])
  , (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
  ]




-- MouseBondings  --{{{1

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
  [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
  , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((modMask, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)
  , ((modMask, 8), \w -> focus w >> findWorkspace getSortByIndex Prev AnyWS 1
                                 >>= windows . W.shift)
  , ((modMask, 9), \w -> focus w >> findWorkspace getSortByIndex Next AnyWS 1
                                 >>= windows . W.shift)
  ]




-- Main  --{{{1

myStatusbar = "dzen2 -x 0 -y 0 -w 1920 -h 14 -ta l -fg '#dcdccc' -bg '#222222'"
              ++ " -fn '" ++ myFont ++ "' -dock -e 'onstart=lower'"

main = do
  statusPipe <- spawnPipe myStatusbar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { borderWidth        = 2
    , workspaces         = ["main", "another", "misc", "gimp"]
    , terminal           = "urxvt"
    , normalBorderColor  = "#666666"
    , focusedBorderColor = "#63afaf"

    , modMask            = mod4Mask
    , keys               = myKeys
    , mouseBindings      = myMouseBindings

    , layoutHook         = myLayoutHook
    , logHook            = myLogHook statusPipe
    , startupHook        = setDefaultCursor xC_left_ptr <+> setWMName "LG3D"
    , manageHook         = manageDocks <+> myManageHook
    , handleEventHook    = fullscreenEventHook

    , focusFollowsMouse  = True
    }




-- __END__  --{{{1
-- vim: expandtab softtabstop=2 shiftwidth=2
-- vim: foldmethod=marker
