import Control.Monad (filterM)
import Data.Map qualified as M
import Data.Maybe (isJust)
import Data.Monoid (All (..))
import Sound.Pulse.Pactl (listPulseCards, switchPulseCardProfile)
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatSnap
import XMonad.Actions.Promote
import XMonad.Actions.Submap
import XMonad.Actions.ToggleFullFloat
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.StableColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.StackSet qualified as W
import XMonad.Util.Cursor
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

myTerminal = "alacritty"

myBorderWidth = 2

myModMask = mod4Mask

myWorkspaces = map show [(1 :: Int) .. 9]

myStatusbarHeight = 20

myFont = "xft:Sono:size=9"

myPrimaryColor = "#5686d7"

myBlackColor = "#334454"

myForegroundColor = "#d5dae9"

myBackgroundColor = "#23262e"

myXPConfig ref =
  (def :: XPConfig)
    { font = myFont,
      fgColor = myForegroundColor,
      bgColor = myBackgroundColor,
      fgHLight = myPrimaryColor,
      bgHLight = myBackgroundColor,
      borderColor = myBackgroundColor,
      promptBorderWidth = 0,
      position = Top,
      height = myStatusbarHeight,
      historyFilter = deleteAllDuplicates,
      promptKeymap = M.union myKeymap emacsLikeXPKeymap
    }
  where
    myKeymap =
      M.fromList
        [ ((controlMask, xK_u), setInput ""),
          ((controlMask, xK_h), deleteString Prev),
          ((controlMask, xK_m), setSuccess True >> setDone True),
          ((controlMask, xK_n), historyDownMatching ref),
          ((controlMask, xK_p), historyUpMatching ref),
          ((controlMask, xK_w), killWord Prev)
        ]

myEventHook =
  removeBorderEventHook $
    foldr
      (<||>)
      (pure False)
      [ className =? "Wine",
        className =? "photoshop.exe",
        className =? "transmission-gtk" <&&> title =? "" <&&> isDialog
      ]

removeBorderEventHook :: Query Bool -> XMonad.Event -> X All
removeBorderEventHook query e = do
  whenX (runQuery query w) $ do
    d <- asks display
    wa <- io $ getWindowAttributes d w
    io $
      configureWindow d w 0b11111 $
        WindowChanges
          { wc_x = wa_x wa,
            wc_y = wa_y wa,
            wc_width = wa_width wa + wa_border_width wa * 2,
            wc_height = wa_height wa + wa_border_width wa * 2,
            wc_border_width = 0,
            wc_sibling = 0,
            wc_stack_mode = 0
          }
  return $ All True
  where
    w = ev_window e

myLayoutHook =
  smartBorders $
    toggleLayouts (avoidStrutsOn [] Full) $
      avoidStruts $
        spacing 4 $
          gaps
            [(U, 4), (D, 4), (L, 4), (R, 4)]
            (tallLayout ||| threeColLayout)
  where
    tallLayout =
      renamed [Replace "Tall"] $
        stableColumns
          (3 / 100)
          ((3 / 100) * (16 / 9))
          [ staticColumn 1 (1 / 2) [],
            dynamicColumn 1 (1 / 2) []
          ]
    threeColLayout =
      renamed [Replace "ThreeCol"] $
        stableColumns
          (3 / 100)
          ((3 / 100) * (16 / 9))
          [ staticColumn 1 (1 / 2) [],
            dynamicColumn 1 (1 / 4) [],
            dynamicColumn 3 (1 / 4) []
          ]

myManageHook =
  manageDocks
    <+> composeOne
      [ className =? "Firefox" <&&> appName /=? "Navigator" -?> doFloat,
        className =? "Geeqie" -?> doShiftEmptyAndGo,
        className =? "Gimp" -?> doShiftEmptyAndGo,
        className =? "Inkscape" -?> doShiftEmptyAndGo,
        className =? "XFontSel" -?> doCenterFloat,
        className =? "Xmessage" -?> doCenterFloat,
        className =? "feh" -?> doCenterFloat,
        className =? "fontforge" -?> doShiftEmptyAndGo <+> doFloat,
        className =? "libreoffice-startcenter" -?> doShiftEmptyAndGo,
        className =? "pavucontrol" -?> doCenterFloat,
        className =? "mpv" -?> doCenterFloat
      ]
    <+> composeOne
      [ isWineSystemTray -?> doHideIgnore,
        isFullscreen -?> doFullFloat,
        isDialog -?> doCenterFloat,
        role =? "browser" -?> ewmhDesktopsManageHook,
        role =? "bubble" -?> doIgnore, -- for Chromeium tooltip
        role =? "pop-up" -?> doFloat,
        isAnonymousWindow -?> doFloat
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
            _ -> doShiftAndGo =<< liftX (findWorkspace getSortByIndex Next emptyWS 1)
        _ -> idHook
    role = stringProperty "WM_WINDOW_ROLE"
    isAnonymousWindow = (not <$> hasStringProperty "WM_CLASS") <&&> (not <$> hasStringProperty "WM_WINDOW_ROLE")
    isWineSystemTray = className =? "explorer.exe" <&&> title =? "Wine System Tray"
    hasStringProperty p =
      ask >>= \w -> liftX $ withDisplay $ \d -> do
        a <- io $ internAtom d p False
        isJust <$> (io $ getWindowProperty8 d a w)

myStartupHook = do
  setDefaultCursor xC_left_ptr
  return ()

myKeys conf@(XConfig {XMonad.modMask = modMask}) =
  M.fromList $
    [ ((modMask, xK_Return), promote),
      ((modMask .|. shiftMask, xK_Return), safeSpawnProg $ terminal conf),
      ((modMask, xK_space), sendMessage NextLayout),
      ((modMask .|. shiftMask, xK_space), setLayout $ layoutHook conf),
      ((modMask, xK_Tab), moveTo Next (Not emptyWS)),
      ((modMask .|. shiftMask, xK_Tab), moveTo Prev (Not emptyWS)),
      ((modMask, xK_j), windows W.focusDown),
      ((modMask, xK_k), windows W.focusUp),
      ((modMask .|. shiftMask, xK_j), windows W.swapDown),
      ((modMask .|. shiftMask, xK_k), windows W.swapUp),
      ((modMask, xK_h), sendMessage Shrink),
      ((modMask, xK_l), sendMessage Expand),
      ((modMask .|. shiftMask, xK_h), sendMessage ShrinkRow),
      ((modMask .|. shiftMask, xK_l), sendMessage ExpandRow),
      ((modMask, xK_f), sendMessage ToggleLayout),
      ((modMask, xK_m), windows W.focusMaster),
      ((modMask, xK_t), withFocused $ windows . W.sink),
      ((modMask, xK_comma), sendMessage $ IncMasterN (-1)),
      ((modMask, xK_period), sendMessage $ IncMasterN 1),
      ((modMask .|. shiftMask, xK_c), kill),
      ((modMask .|. shiftMask, xK_q), io exitSuccess),
      ((modMask, xK_r), refresh),
      ((modMask .|. shiftMask, xK_r), spawn "xmonad --recompile && xmonad --restart"),
      ((modMask, xK_p), initMatches >>= shellPrompt . myXPConfig),
      ((modMask, xK_equal), safeSpawn "amixer" ["-q", "set", "Master", "5%+"]),
      ((modMask, xK_minus), safeSpawn "amixer" ["-q", "set", "Master", "5%-"]),
      ((modMask, xK_0), safeSpawn "amixer" ["-q", "set", "Master", "toggle"]),
      ((modMask .|. shiftMask, xK_equal), safeSpawn "mpc" ["volume", "+5"]),
      ((modMask .|. shiftMask, xK_minus), safeSpawn "mpc" ["volume", "-5"]),
      ((modMask, xK_backslash), safeSpawn "mpc" ["toggle"]),
      ((modMask, xK_bracketleft), safeSpawn "mpc" ["prev"]),
      ((modMask, xK_bracketright), safeSpawn "mpc" ["next"]),
      ((modMask, xK_apostrophe), listPulseCards >>= mapM_ (switchPulseCardProfile ["output:analog-stereo", "output:hdmi-stereo"])),
      ((modMask, xK_Print), safeSpawn "scrot" ["-e", "mv $f \\$HOME/Desktop/"]),
      ((modMask .|. mod1Mask, xK_j), withFocused $ snapMove D Nothing),
      ((modMask .|. mod1Mask, xK_k), withFocused $ snapMove U Nothing),
      ((modMask .|. mod1Mask, xK_h), withFocused $ snapMove L Nothing),
      ((modMask .|. mod1Mask, xK_l), withFocused $ snapMove R Nothing),
      ( (modMask, xK_x),
        submap $
          M.fromList $
            [ ((mask .|. mask', key), action)
              | ((mask, key), action) <- launcherKeys,
                mask' <- [0, modMask]
            ]
      )
    ]
      ++ workspaceKeys
  where
    workspaceKeys =
      [ ((m, k), windows $ f w)
        | (m, f) <- [(modMask, W.greedyView), (modMask .|. shiftMask, W.shift)],
          (k, w) <- zip [xK_1 .. xK_9] (XMonad.workspaces conf)
      ]

    launcherKeys =
      [ ((0, xK_b), safeSpawn "brave-browser-stable" []),
        ((0, xK_c), safeSpawn "google-chrome-stable" []),
        ((0, xK_f), safeSpawn "firefox-bin" []),
        ((0, xK_g), safeSpawn "geeqie" []),
        ((0, xK_p), safeSpawn "pavucontrol" []),
        ((0, xK_t), safeSpawn "transmission-gtk" [])
      ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) =
  M.fromList
    [ ( (modMask, button1),
        \w ->
          focus w
            >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow),
      ((modMask, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster),
      ( (modMask, 8),
        \w ->
          focus w
            >> findWorkspace getSortByIndex Prev anyWS 1
            >>= windows . W.shift
      ),
      ( (modMask, 9),
        \w ->
          focus w
            >> findWorkspace getSortByIndex Next anyWS 1
            >>= windows . W.shift
      )
    ]

myStatusBar :: StatusBarConfig
myStatusBar =
  def
    { sbLogHook = return (),
      sbStartupHook = spawnStatusBar "polybar",
      sbCleanupHook = killStatusBar "polybar"
    }

main = do
  xmonad $
    withUrgencyHook NoUrgencyHook $
      withSB myStatusBar $
        toggleFullFloatEwmhFullscreen $
          ewmhFullscreen $
            ewmh $
              docks $
                def
                  { terminal = myTerminal,
                    borderWidth = myBorderWidth,
                    modMask = myModMask,
                    workspaces = myWorkspaces,
                    normalBorderColor = myBlackColor,
                    focusedBorderColor = myPrimaryColor,
                    keys = myKeys,
                    mouseBindings = myMouseBindings,
                    handleEventHook = myEventHook,
                    layoutHook = myLayoutHook,
                    manageHook = myManageHook,
                    startupHook = myStartupHook,
                    focusFollowsMouse = True
                  }
