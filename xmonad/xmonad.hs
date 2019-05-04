-- My xmonad.hs
-- Import  --{{{1

import XMonad hiding (Tall)
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.NoBorders
import XMonad.Actions.Promote
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Cursor
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as W

import Control.Monad (filterM)
import Data.Monoid (All(..))
import System.Directory (getHomeDirectory)
import System.Exit (exitWith, ExitCode(..))
import System.IO (BufferMode(..), hSetBuffering)
import System.Posix.IO (FdOption(..), closeFd, createPipe, dupTo, fdToHandle, setFdOption, stdInput, stdOutput)
import System.Posix.Process (executeFile)

import qualified Data.Map as M




-- Config  --{{{1

myTerminal = "urxvt"
myBorderWidth = 2
myModMask = mod4Mask
myWorkspaces = map show [1..9]

myFont = "-artwiz-gelly-medium-*-*-*-*-*-*-*-*-*-*-*, -mplus-gothic-medium-r-normal--10-*-*-*-*-*-*-*"
myNormalBorderColor = "#474747"
myNormalFGColor = "#e2e2e2"
myNormalBGColor = "#171717"
myFocusedBorderColor = "#4580c3"
myFocusedBGColor = "#4580c3"
myFocusedFGColor = "#171717"

myStatusbarHeight = 14

myXPConfig ref = defaultXPConfig
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
  , promptKeymap      = M.union myKeymap emacsLikeXPKeymap
  }
  where
    myKeymap = M.fromList $
      [ ((controlMask, xK_u), setInput "")
      , ((controlMask, xK_h), deleteString Prev)
      , ((controlMask, xK_w), killWord Prev)
      , ((controlMask, xK_p), historyUpMatching ref)
      , ((controlMask, xK_n), historyDownMatching ref)
      ]




-- EventHook  --{{{1

removeBorderEventHook :: Query Bool -> Event -> X All
removeBorderEventHook query ev = do
  whenX (query `runQuery` w) $ do
    d <- asks display
    io $ setWindowBorderWidth d w 0
  return (All True)
  where
    w = ev_window ev

myEventHook = fullscreenEventHook
          <+> docksEventHook
          <+> removeBorderEventHook (className =? "Wine")




-- LayoutHook  --{{{1

myLayoutHook = avoidStruts $ smartBorders $ toggleLayouts Full $
               (tallLayout ||| wideLayout)
  where
    basicLayout = ResizableTall 1 (2/100) (1/2) []
    tallLayout = basicLayout
    wideLayout = Mirror basicLayout




-- LogHook  --{{{1

myLogHook h = do
  home  <- io getHomeDirectory
  floated <- withWindowSet isFloat
  let dzenIcon     = wrap ("^i(" ++ home ++ "/.dzen/xmonad/") ")"
      layoutIcon name = case name of
        "ResizableTall"        -> dzenIcon "layout-tall-black.xbm"
        "Mirror ResizableTall" -> dzenIcon "layout-mirror-black.xbm"
        "Full"                 -> dzenIcon "layout-full-black.xbm"
        "Spiral"               -> dzenIcon "layout-spiral-black.xbm"
        _                      -> name
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




-- ManageHook  --{{{1

myManageHook = manageDocks
  <+> composeOne
    [ isDialog                               -?> doCenterFloat
    , isFullscreen                           -?> doFullFloat
    ]
  <+> composeAll
    [ title     =? "Wine System Tray"                     --> doHideIgnore
    , className =? "Uim-tomoe-gtk"                        --> doFloat
    , className =? "Firefox" <&&> appName /=? "Navigator" --> doFloat
    , className =? "qemu-system-x86_64"                   --> doFloat
    , className =? "rdesktop"                             --> doFloat
    , className =? "XFontSel"                             --> doCenterFloat
    , className =? "Xmessage"                             --> doCenterFloat
    , className =? "feh"                                  --> doCenterFloat
    , className =? "mpv"                                  --> doCenterFloat
    , className =? "Geeqie"                               --> doShiftEmptyAndGo
    , className =? "Inkscape"                             --> doShiftEmptyAndGo
    , className =? "fontforge"                            --> doShiftEmptyAndGo <+> doFloat
    , className =? "libreoffice-startcenter"              --> doShiftEmptyAndGo
    , className =? "Gimp"                                 --> doShiftEmptyAndGo
    , className =? "Gimp-2.8"                             --> doShiftEmptyAndGo
    , className =? "Gimp-2.8"                             --> doShiftEmptyAndGo
    , role      =? "pop-up"                               --> doFloat
    ]
  where
    isSticky = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_STATE_STICKY"
    doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
    doShiftEmptyAndGo = do
      w <- ask
      c <- liftX $ runQuery className w
      xs <- liftX $ withWindowSet $ filterM (runQuery $ className =? c) . W.index
      case xs of
        [] -> do
          ws <- liftX $ findWorkspace getSortByIndex Next EmptyWS 1
          doShiftAndGo ws
        _  -> idHook
    role = stringProperty "WM_WINDOW_ROLE"




-- StartupHook  --{{{1

spawnConkybar = do
  (rd, wd) <- createPipe

  setFdOption rd CloseOnExec True

  fdToHandle wd >>= \h -> hSetBuffering h LineBuffering

  _ <- xfork $ do
    _ <- dupTo rd stdInput
    executeFile "dzen2"
                True
                [ "-x" , "960"
                , "-y" , "0"
                , "-w" , "960"
                , "-h" , show myStatusbarHeight
                , "-ta" , "r"
                , "-fg" , myNormalFGColor
                , "-bg" , myNormalBGColor
                , "-fn" , myFont
                , "-dock"
                , "-e" , "'onstart=lower'"
                ]
                Nothing

  _ <- xfork $ do
    _ <- dupTo wd stdOutput
    executeFile "conky" True [] Nothing

  closeFd rd
  closeFd wd

  return ()

myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr
  io spawnConkybar




-- Keys  --{{{1

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask,                 xK_Return),       promote)
  , ((modMask .|. shiftMask,   xK_Return),       safeSpawnProg $ terminal conf)

  , ((modMask,                 xK_space),        sendMessage NextLayout)
  , ((modMask .|. shiftMask,   xK_space),        setLayout $ layoutHook conf)

  , ((modMask,                 xK_Tab),          moveTo Next NonEmptyWS)
  , ((modMask .|. shiftMask,   xK_Tab),          moveTo Prev NonEmptyWS)

  , ((modMask,                 xK_j),            windows W.focusDown)
  , ((modMask,                 xK_k),            windows W.focusUp)
  , ((modMask .|. shiftMask,   xK_j),            windows W.swapDown)
  , ((modMask .|. shiftMask,   xK_k),            windows W.swapUp)

  , ((modMask,                 xK_h),            sendMessage Shrink)
  , ((modMask,                 xK_l),            sendMessage Expand)
  , ((modMask .|. shiftMask,   xK_h),            sendMessage MirrorExpand)
  , ((modMask .|. shiftMask,   xK_l),            sendMessage MirrorShrink)

  , ((modMask,                 xK_f),            sendMessage ToggleLayout)
  , ((modMask,                 xK_m),            windows W.focusMaster)
  , ((modMask,                 xK_t),            withFocused $ windows . W.sink)
  , ((modMask,                 xK_b),            withFocused toggleBorder)

  , ((modMask,                 xK_comma),        sendMessage $ IncMasterN 1)
  , ((modMask,                 xK_period),       sendMessage $ IncMasterN (-1))

  , ((modMask .|. shiftMask,   xK_c),            kill)
  , ((modMask .|. shiftMask,   xK_q),            io $ exitWith ExitSuccess)
  , ((modMask,                 xK_r),            refresh)
  , ((modMask .|. shiftMask,   xK_r),            spawn "killall dzen2; xmonad --recompile && xmonad --restart")

  , ((modMask,                 xK_p),            initMatches >>= shellPrompt . myXPConfig)

  , ((modMask,                 xK_equal),        safeSpawn "amixer" ["-q", "set", "Master", "5%+"])
  , ((modMask,                 xK_minus),        safeSpawn "amixer" ["-q", "set", "Master", "5%-"])
  , ((modMask,                 xK_0),            safeSpawn "amixer" ["-q", "set", "Master", "toggle"])

  , ((modMask,                 xK_Print),        safeSpawn "scrot" ["-e", "mv $f \\$HOME/Desktop/"])

  , ((modMask,                 xK_backslash),    safeSpawn "mpc" ["toggle"])
  , ((modMask,                 xK_bracketleft),  safeSpawn "mpc" ["prev"])
  , ((modMask,                 xK_bracketright), safeSpawn "mpc" ["next"])

  , ((modMask .|. controlMask, xK_l),            spawn "sleep 1; xset dpms force off")

  , ((modMask .|. mod1Mask,    xK_j),            withFocused $ snapMove D Nothing)
  , ((modMask .|. mod1Mask,    xK_k),            withFocused $ snapMove U Nothing)
  , ((modMask .|. mod1Mask,    xK_h),            withFocused $ snapMove L Nothing)
  , ((modMask .|. mod1Mask,    xK_l),            withFocused $ snapMove R Nothing)

  , ((modMask,                 xK_grave),        safeSpawnProg "keytray")

  , ((modMask,                 xK_x),            submap $ M.fromList $
                                                        [ ((m, k), safeSpawnProg p)
                                                        | m <- [0, modMask]
                                                        , (k, p) <- lancherKeys
                                                        ])

  , ((0, xK_Super_L), return ())
  , ((0, xK_Super_R), return ())
  ] ++ workspaceKeys
  where
    workspaceKeys = [ ((m, k), windows $ f w)
                    | (m, f) <- [(modMask, W.greedyView), (modMask .|. shiftMask, W.shift)]
                    , (k, w) <- zip [xK_1 .. xK_9] (XMonad.workspaces conf)
                    ]

    lancherKeys = [ (xK_2, "v2c")
                  , (xK_c, "google-chrome-stable")
                  , (xK_f, "firefox-bin")
                  , (xK_p, "pavucontrol")
                  , (xK_t, "transmission-gtk")
                  , (xK_v, "geeqie")
                  ]




-- Mouse  --{{{1

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList
  [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
  , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((modMask, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  , ((modMask, 8),       \w -> focus w >> findWorkspace getSortByIndex Prev AnyWS 1
                                       >>= windows . W.shift)
  , ((modMask, 9),       \w -> focus w >> findWorkspace getSortByIndex Next AnyWS 1
                                       >>= windows . W.shift)
  ]




-- Main  --{{{1

spawnStatusbar = do
  (rd, wd) <- createPipe

  setFdOption rd CloseOnExec True

  h <- fdToHandle wd

  hSetBuffering h LineBuffering

  _ <- xfork $ do
    _ <- dupTo rd stdInput
    executeFile "dzen2"
                True
                [ "-x" , "0"
                , "-y" , "0"
                , "-w" , "960"
                , "-h" , show myStatusbarHeight
                , "-ta" , "l"
                , "-fg" , myNormalFGColor
                , "-bg" , myNormalBGColor
                , "-fn" , myFont
                , "-dock"
                , "-e" , "'onstart=lower'"
                ]
                Nothing

  closeFd rd

  return h

main = do
  statusPipe <- spawnStatusbar

  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ def
    { terminal           = myTerminal
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces

    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

    , keys               = myKeys
    , mouseBindings      = myMouseBindings

    , handleEventHook    = myEventHook
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook statusPipe
    , manageHook         = myManageHook
    , startupHook        = myStartupHook

    , focusFollowsMouse  = True
    }




-- __END__  --{{{1
-- vim: expandtab softtabstop=2 shiftwidth=2
-- vim: foldmethod=marker
