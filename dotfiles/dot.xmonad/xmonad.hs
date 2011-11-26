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




-- Config  --{{{1

myFont = "-nil-profont-medium-r-normal--11-*-*-*-*-*-*-*, -mplus-gothic-medium-r-normal--10-*-*-*-*-*-*-*"

myBorderWidth = 2
myStatusbarHeight = 14
myWorkspaces = ["main", "another", "work", "misc"]

myNormalBorderColor = "#474747"
myNormalFGColor = "#e2e2e2"
myNormalBGColor = "#171717"
myFocusedBorderColor = "#4580c3"
myFocusedBGColor = "#4580c3"
myFocusedFGColor = "#171717"

myStatusbar = printf "dzen2 -x 0 -y 0 -w 960 -h %d -ta l -fg '%s' -bg '%s' -fn '%s' -dock -e 'onstart=lower'"
                     myStatusbarHeight myNormalFGColor myNormalBGColor myFont

myXPConfig = defaultXPConfig
  { font              = myFont
  , fgColor           = myNormalFGColor
  , bgColor           = myNormalBGColor
  , fgHLight          = myFocusedBGColor
  , bgHLight          = myNormalBGColor
  , borderColor       = myNormalBorderColor
  , promptBorderWidth = 0
  , position          = Top
  , height            = myStatusbarHeight
  , historyFilter     = deleteAllDuplicates
  }




-- Lauout  --{{{1

myLayoutHook = avoidStruts $ smartBorders $
  toggleLayouts Full $ imLayout $ tallLayout ||| wideLayout
  where
    basicLayout = ResizableTall 1 (2/100) (1/2) []
    tallLayout = named "Tall" $ basicLayout
    wideLayout = named "Wide" $ Mirror basicLayout
    imLayout = reflectHoriz . withIM 0.15 imWindows
             . reflectHoriz . trackFloating
    imWindows = foldr1 Or [ ClassName "Pidgin" `And` Role "buddy_list"
                          , ClassName "Skype" `And` Role "MainWindow"
                          ]




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
  , className =? "Geeqie"                  -?> doShiftAndGo "work"
  , className =? "Inkscape"                -?> doShiftAndGo "work"
  , className =? "fontforge"               -?> doShiftAndGo "work" <+> doFloat
  , className =? "libreoffice-startcenter" -?> doShiftAndGo "work"
  , className =? "Gimp"
    <&&> role /=? "gimp-toolbox"
    <&&> role /=? "gimp-dock"
    <&&> role /=? "gimp-image-window"      -?> doShiftAndGo "work" <+> doFloat
  , className =? "Gimp"                    -?> doShiftAndGo "work"
  , className =? "Skype"
    <&&> fmap (isInfixOf "(Beta)") title   -?> addProperty "WM_WINDOW_ROLE" "MainWindow"
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"
    addProperty prop value = do
      d <- liftX $ asks display
      w <- ask
      a <- io $ internAtom d prop False
      t <- io $ internAtom d "STRING" False
      io $ changeProperty8 d w a t propModeReplace $ map (fromIntegral . fromEnum) value
      idHook
    doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws




-- Log  --{{{1

myLogHook h = do
  home  <- io getHomeDirectory
  floated <- withWindowSet isFloat
  let dzenIcon     = wrap ("^i(" ++ home ++ "/.dzen/xmonad/") ")"
      layoutIcon x = case last $ words x of
        "Tall" -> dzenIcon "layout-tall-black.xbm"
        "Wide" -> dzenIcon "layout-mirror-black.xbm"
        "Full" -> dzenIcon "layout-full-black.xbm"
        _      -> x
  dynamicLogWithPP $ defaultPP
    { ppCurrent         = dzenColor myFocusedFGColor myFocusedBGColor
                        . wrap (dzenIcon "square.xbm") " "
    , ppHidden          = wrap (dzenIcon "square3.xbm") " "
    , ppHiddenNoWindows = wrap "^p(8)" " "
    , ppUrgent          = dzenColor myNormalBGColor myNormalFGColor
                        . wrap (dzenIcon "square3.xbm") " "
    , ppSep             = dzenColor myNormalBorderColor "" " ^p(;2)^r(1x8)^p() "
    , ppWsSep           = ""
    , ppTitle           = if floated then (dzenIcon "square.xbm" ++) . dzenEscape else dzenEscape
    , ppLayout          = dzenColor myFocusedBGColor "" . layoutIcon
    , ppOutput          = hPutStrLn h
    }
  where
    isFloat ws = return $ case W.peek ws of
      Nothing -> False
      Just w  -> M.member w $ W.floating ws




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

  , ("M-<Esc>",      safeSpawnProg "keytray")
  , ("M-C-l",        spawn "sleep 1; xset dpms force off")
  , ("M-C-y",        do home <- io getHomeDirectory
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
              , ("v", "geeqie")
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

main = do
  statusPipe <- spawnPipe myStatusbar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { borderWidth        = myBorderWidth
    , workspaces         = myWorkspaces
    , terminal           = "urxvt"
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

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
