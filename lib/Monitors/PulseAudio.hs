module Monitors.PulseAudio (PulseAudio(..)) where

import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CBool(..), CFloat(..), CInt(..))
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullPtr)
import System.Console.GetOpt (ArgDescr(..), OptDescr(..))
import Xmobar (Args, Exec(..))
import Xmobar.Plugins.Monitors.Common

data PulseAudio = PulseAudio Args
  deriving (Read, Show)

instance Exec PulseAudio where
  alias (PulseAudio _) = "pulseaudio"
  start (PulseAudio args) cb = startPulseAudio args cb

pulseAudioConfig :: IO MConfig
pulseAudioConfig = mkMConfig "<volume>% <status>" ["volume", "status"]

data PulseAudioOpts = PulseAudioOpts
    { onString :: String
    , offString :: String
    }

defaultOpts :: PulseAudioOpts
defaultOpts = PulseAudioOpts
    { onString = "[on] "
    , offString = "[off]"
    }

optDescriptions :: [OptDescr (PulseAudioOpts -> PulseAudioOpts)]
optDescriptions =
    [ Option "O" ["on"] (ReqArg (\x o -> o { onString = x }) "") ""
    , Option "o" ["off"] (ReqArg (\x o -> o { offString = x }) "") ""
    ]

startPulseAudio :: Args -> (String -> IO ()) -> IO ()
startPulseAudio args outputCallback = do
  volumeMonitorPtr <- pa_volume_monitor_init
  if volumeMonitorPtr /= nullPtr
    then runPulseAudio args outputCallback volumeMonitorPtr
    else outputCallback "<failed to init>"

runPulseAudio :: Args -> (String -> IO ()) -> Ptr PaVolumeMonitor -> IO ()
runPulseAudio args outputCallback volumeMonitorPtr = do
  callbackFunPtr <- wrapVolumeCallback $ \volume muted -> do
    let run argv = do
        opts <- io $ parseOptsWith optDescriptions defaultOpts argv
        volumeStr <- formatVolume $ realToFrac volume
        statusStr <- formatStatus opts $ not $ muted /= 0
        parseTemplate [volumeStr, statusStr]

    output <- pulseAudioConfig >>= (runReaderT $ doArgs args run (\_ -> pure True))

    outputCallback output

  pa_volume_monitor_set_callback volumeMonitorPtr callbackFunPtr

  result <- pa_volume_monitor_run volumeMonitorPtr

  when (result /= 0) $ do
    errorPtr <- pa_volume_monitor_get_error volumeMonitorPtr
    when (errorPtr /= nullPtr) $ do
      errorStr <- peekCString errorPtr
      outputCallback errorStr

  pa_volume_monitor_free volumeMonitorPtr
  freeHaskellFunPtr callbackFunPtr

formatVolume :: Float -> Monitor String
formatVolume volume = showPercentWithColors $ volume

formatStatus :: PulseAudioOpts -> Bool -> Monitor String
formatStatus opts True = return $ onString opts
formatStatus opts False = return $ offString opts

data PaVolumeMonitor

type VolumeCallback = CFloat -> CBool -> IO ()

foreign import ccall unsafe "pa_volume_monitor_init"
  pa_volume_monitor_init :: IO (Ptr PaVolumeMonitor)

foreign import ccall unsafe "pa_volume_monitor_set_callback"
  pa_volume_monitor_set_callback :: Ptr PaVolumeMonitor -> FunPtr VolumeCallback -> IO ()

foreign import ccall safe "pa_volume_monitor_run"
  pa_volume_monitor_run :: Ptr PaVolumeMonitor -> IO CInt

foreign import ccall unsafe "pa_volume_monitor_get_error"
  pa_volume_monitor_get_error :: Ptr PaVolumeMonitor -> IO CString

foreign import ccall unsafe "pa_volume_monitor_free"
  pa_volume_monitor_free :: Ptr PaVolumeMonitor -> IO ()

foreign import ccall "wrapper"
  wrapVolumeCallback :: VolumeCallback -> IO (FunPtr VolumeCallback)
