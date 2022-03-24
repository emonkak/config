{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-name-shadowing #-}
-- My xmonad.hs
-- Import  --{{{1

import XMonad
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
import XMonad.Layout.Gaps
import XMonad.Layout.MultiPanes
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
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
import qualified Data.Map as M




-- Configs  --{{{1

myTerminal = "alacritty"
myBorderWidth = 2
myModMask = mod4Mask
myWorkspaces = map show [(1 :: Int)..9]

myFont = "xft:Monospace:pixelsize=11"
myNormalBorderColor = "#21272b"
myNormalFGColor = "#f5f6f7"
myNormalBGColor = "#21272b"

myFocusedBorderColor = "#1c95e6"
myFocusedFGColor = "#5ebaf7"
myFocusedBGColor = "#21272b"

myAlternateFGColor = "#869096"

myStatusbarHeight = 21

myXPConfig ref = (def :: XPConfig)
  { font              = myFont
  , fgColor           = myNormalFGColor
  , bgColor           = myNormalBGColor
  , fgHLight          = myFocusedFGColor
  , bgHLight          = myFocusedBGColor
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
      , ((controlMask, xK_m), setSuccess True >> setDone True)
      , ((controlMask, xK_n), historyDownMatching ref)
      , ((controlMask, xK_p), historyUpMatching ref)
      , ((controlMask, xK_w), killWord Prev)
      ]




-- Hooks  --{{{1
-- EventHook  --{{{2

myEventHook = removeBorderEventHook (className =? "Wine")

removeBorderEventHook :: Query Bool -> XMonad.Event -> X All
removeBorderEventHook query e = do
  whenX (query `runQuery` w) $ do
    d <- asks display
    io $ setWindowBorderWidth d w 0
  return $ All True
  where
    w = ev_window e





-- LayoutHook  --{{{2

myLayoutHook = avoidStruts $ smartBorders $ toggleLayouts Full $
               spacing 4 $ gaps [(U, 4), (D, 4), (L, 4), (R, 4)] $
               (tallLayout ||| threeColLayout)
  where
    tallLayout = named "Tall" $ MultiPanes (3/100) ((3/100) * (16/9))
      [ Pane (PaneSlotAtMost 1) (1/2) []
      , Pane (PaneSlotAtLeast 1) (1/2) []
      ]
    threeColLayout = named "ThreeCol" $ MultiPanes (3/100) ((3/100) * (16/9))
      [ Pane (PaneSlotAtMost 1) (2/4) []
      , Pane (PaneSlotAtLeast 1) (1/4) []
      , Pane (PaneSlotAtLeast 2) (1/4) []
      ]




-- LogHook  --{{{2

myLogHook h = do
  home  <- io getHomeDirectory
  floated <- withWindowSet isFloat
  let icon = wrap ("<icon=" ++ home ++ "/.xmonad/icons/") "/>"
      layoutIcon name = case name of
        "Spacing Tall"     -> icon "layout-tall.xbm"
        "Spacing ThreeCol" -> icon "layout-threecol.xbm"
        "Full"             -> icon "layout-full.xbm"
        _                  -> xmobarRaw name
  dynamicLogWithPP $ def
    { ppOutput           = hPutStrLn h
    , ppCurrent          = xmobarColor myFocusedFGColor "" . wrap "[" "]"
    , ppHidden           = wrap " " " "
    , ppHiddenNoWindows  = xmobarColor myAlternateFGColor "" . wrap " " " "
    , ppUrgent           = wrap "*" " "
    , ppSep              = xmobarColor myAlternateFGColor "" " | "
    , ppWsSep            = ""
    , ppTitle            = if floated then ("<fn=1>\xe069</fn> " ++) else id
    , ppTitleSanitize    = xmobarRaw
    , ppLayout           = xmobarColor myFocusedFGColor "" . layoutIcon
    }
  where
    isFloat ws = return $ case W.peek ws of
      Nothing -> False
      Just w  -> M.member w $ W.floating ws




-- ManageHook  --{{{2

myManageHook = manageDocks
  <+> composeOne
    [ className =? "Firefox" <&&> appName /=? "Navigator" -?> doFloat
    , className =? "Geeqie"                               -?> doShiftEmptyAndGo
    , className =? "Gimp"                                 -?> doShiftEmptyAndGo
    , className =? "Inkscape"                             -?> doShiftEmptyAndGo
    , className =? "Pavucontrol"                          -?> doCenterFloat
    , className =? "XFontSel"                             -?> doCenterFloat
    , className =? "Xmessage"                             -?> doCenterFloat
    , className =? "commeon.exe"                          -?> doFloat
    , className =? "feh"                                  -?> doCenterFloat
    , className =? "fontforge"                            -?> doShiftEmptyAndGo <+> doFloat
    , className =? "libreoffice-startcenter"              -?> doShiftEmptyAndGo
    , className =? "mpv"                                  -?> doCenterFloat
    , title     =? "Wine System Tray"                     -?> doHideIgnore
    ]
  <+> composeOne
    [ isFullscreen     -?> doFullFloat
    , isDialog         -?> doCenterFloat
    , isUnknown        -?> doFloat
    , role =? "pop-up" -?> doFloat
    ]
  where
    doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
    doShiftEmptyAndGo = do
      w <- ask
      c <- liftX $ runQuery className w
      ws <- liftX $ withWindowSet $ filterM (runQuery $ className =? c) . W.index
      case ws of
        [] -> do
          workspace <- liftX (W.workspace . W.current <$> gets windowset)
          case W.stack workspace of
            Nothing -> doShiftAndGo $ W.tag workspace
            _       -> doShiftAndGo =<< liftX (findWorkspace getSortByIndex Next emptyWS 1)
        _ -> idHook
    isUnknown = (not <$> hasProperty "WM_CLASS") <&&> (not <$> hasProperty "WM_WINDOW_ROLE")
    hasProperty p = ask >>= \w -> liftX $ withDisplay $ \d ->
      maybe False (const True) <$> getStringProperty d w p
    role = stringProperty "WM_WINDOW_ROLE"




-- StartupHook  --{{{2

myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr
  return ()




-- Keys  --{{{1

myKeys conf@(XConfig { XMonad.modMask = modMask }) = M.fromList $
  [ ((modMask,                 xK_Return),       promote)
  , ((modMask .|. shiftMask,   xK_Return),       safeSpawnProg $ terminal conf)

  , ((modMask,                 xK_space),        sendMessage NextLayout)
  , ((modMask .|. shiftMask,   xK_space),        setLayout $ layoutHook conf)

  , ((modMask,                 xK_Tab),          moveTo Next (Not emptyWS))
  , ((modMask .|. shiftMask,   xK_Tab),          moveTo Prev (Not emptyWS))

  , ((modMask,                 xK_j),            windows W.focusDown)
  , ((modMask,                 xK_k),            windows W.focusUp)
  , ((modMask .|. shiftMask,   xK_j),            windows W.swapDown)
  , ((modMask .|. shiftMask,   xK_k),            windows W.swapUp)

  , ((modMask,                 xK_h),            sendMessage Shrink)
  , ((modMask,                 xK_l),            sendMessage Expand)
  , ((modMask .|. shiftMask,   xK_h),            sendMessage ShrinkClient)
  , ((modMask .|. shiftMask,   xK_l),            sendMessage ExpandClient)

  , ((modMask,                 xK_b),            sendMessage ToggleStruts)
  , ((modMask,                 xK_f),            sendMessage ToggleLayout)
  , ((modMask,                 xK_g),            withFocused toggleBorder)
  , ((modMask,                 xK_m),            windows W.focusMaster)
  , ((modMask,                 xK_t),            withFocused $ windows . W.sink)

  , ((modMask,                 xK_comma),        sendMessage $ IncMasterN (-1))
  , ((modMask,                 xK_period),       sendMessage $ IncMasterN (1))

  , ((modMask .|. shiftMask,   xK_c),            kill)
  , ((modMask .|. shiftMask,   xK_q),            io $ exitWith ExitSuccess)
  , ((modMask,                 xK_r),            refresh)
  , ((modMask .|. shiftMask,   xK_r),            spawn "xmonad --recompile && xmonad --restart")

  , ((modMask,                 xK_p),            initMatches >>= shellPrompt . myXPConfig)

  , ((modMask,                 xK_equal),        safeSpawn "amixer" ["-q", "set", "Master", "5%+"])
  , ((modMask,                 xK_minus),        safeSpawn "amixer" ["-q", "set", "Master", "5%-"])
  , ((modMask,                 xK_0),            safeSpawn "amixer" ["-q", "set", "Master", "toggle"])

  , ((modMask .|. shiftMask,   xK_equal),        safeSpawn "mpc" ["volume", "+5"])
  , ((modMask .|. shiftMask,   xK_minus),        safeSpawn "mpc" ["volume", "-5"])

  , ((modMask,                 xK_Print),        safeSpawn "scrot" ["-e", "mv $f \\$HOME/Desktop/"])

  , ((modMask,                 xK_backslash),    safeSpawn "mpc" ["toggle"])
  , ((modMask,                 xK_bracketleft),  safeSpawn "mpc" ["prev"])
  , ((modMask,                 xK_bracketright), safeSpawn "mpc" ["next"])

  , ((modMask .|. shiftMask,   xK_bracketleft),  spawn "pactl set-card-profile alsa_card.pci-0000_00_1f.3 output:analog-stereo && pactl set-card-profile alsa_card.pci-0000_03_00.1 off")
  , ((modMask .|. shiftMask,   xK_bracketright), spawn "pactl set-card-profile alsa_card.pci-0000_03_00.1 output:hdmi-stereo && pactl set-card-profile alsa_card.pci-0000_00_1f.3 off")

  , ((modMask .|. controlMask, xK_l),            spawn "sleep 1; xset dpms force off")

  , ((modMask .|. mod1Mask,    xK_j),            withFocused $ snapMove D Nothing)
  , ((modMask .|. mod1Mask,    xK_k),            withFocused $ snapMove U Nothing)
  , ((modMask .|. mod1Mask,    xK_h),            withFocused $ snapMove L Nothing)
  , ((modMask .|. mod1Mask,    xK_l),            withFocused $ snapMove R Nothing)

  , ((modMask,                 xK_x),            submap $ M.fromList $
                                                        [ ((mask .|. mask', key), action)
                                                        | ((mask, key), action) <- lancherKeys
                                                        , mask' <- [0, modMask]
                                                        ])
  ] ++ workspaceKeys
  where
    workspaceKeys = [ ((m, k), windows $ f w)
                    | (m, f) <- [(modMask, W.greedyView), (modMask .|. shiftMask, W.shift)]
                    , (k, w) <- zip [xK_1 .. xK_9] (XMonad.workspaces conf)
                    ]

    lancherKeys = [ ((0,         xK_2), safeSpawn "v2c" [])
                  , ((0,         xK_c), safeSpawn "brave-bin" [])
                  , ((shiftMask, xK_c), safeSpawn "brave-bin" ["--profile-directory=Profile 1"])
                  , ((0,         xK_f), safeSpawn "firefox-bin" [])
                  , ((0,         xK_g), safeSpawn "geeqie" [])
                  , ((0,         xK_p), safeSpawn "pavucontrol" [])
                  , ((0,         xK_t), safeSpawn "transmission-gtk" [])
                  ]




-- Mouse Bindings --{{{1

myMouseBindings (XConfig { XMonad.modMask = modMask }) = M.fromList
  [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)
  , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((modMask, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  , ((modMask, 8),       \w -> focus w >> findWorkspace getSortByIndex Prev anyWS 1
                                       >>= windows . W.shift)
  , ((modMask, 9),       \w -> focus w >> findWorkspace getSortByIndex Next anyWS 1
                                       >>= windows . W.shift)
  ]




-- Main  --{{{1

main = do
  statusPipe <- spawnPipe "xmobar"

  xmonad $ withUrgencyHook NoUrgencyHook $ docks $ ewmhFullscreen $ ewmh $ def
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
