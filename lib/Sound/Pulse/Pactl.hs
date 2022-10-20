{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, NamedFieldPuns #-}

module Sound.Pulse.Pactl
  ( listPulseCards
  , switchPulseCardProfile
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON(..), decode)
import Data.Aeson.Types (Options(..), defaultOptions, genericParseJSON)
import Data.List (find)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

import qualified Data.Map as M

data Card = Card
  { index :: Int
  , name :: String
  , driver :: String
  , owner_module :: String
  , properties :: M.Map String String
  , profiles :: M.Map String Profile
  , active_profile :: String
  , ports :: M.Map String Port
  }
  deriving (Show, Generic, FromJSON)

data Profile = Profile
  { description :: String
  , sinks :: Int
  , sources :: Int
  , priority :: Int
  , available :: Bool
  }
  deriving (Show, Generic, FromJSON)

data Port = Port
  { description :: String
  , _type :: String
  , priority :: Int
  , latency_offset :: String
  , availability_group :: String
  , availability :: String
  , properties :: M.Map String String
  , profiles :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Port where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = keywordFieldLabelModifier }

keywordFieldLabelModifier :: String -> String
keywordFieldLabelModifier "_type" = "type"
keywordFieldLabelModifier x = x

listPulseCards :: (MonadIO m) => m (Maybe [Card])
listPulseCards = do
  output <- runProcessWithInput "pactl" ["-f", "json", "list", "cards"] ""
  return $ decode $ fromString output

switchPulseCardProfile :: (MonadIO m) => [String] -> [Card] -> m ()
switchPulseCardProfile profiles cards = do
  case find isTargetCard cards of
    (Just (Card { name, active_profile })) ->
      let nextProfile = swtich profiles active_profile
       in mapM_ (\profile -> safeSpawn "pactl" ["set-card-profile", name, profile]) nextProfile
    _ -> return ()
  where
    isTargetCard (Card { driver = "module-alsa-card.c", profiles = availableProfiles }) =
       all (\profile -> M.member profile availableProfiles) profiles

    isTargetCard _ = False

swtich :: Eq a => [a] -> a -> Maybe a
swtich [] _ = Nothing
swtich candidates active =
  let i = maybe (l - 1) fst $ find ((== active) . snd) $ zip [0..] candidates
      j = (i + 1) `mod` length candidates
   in Just (candidates !! j)
  where
    l = length candidates
