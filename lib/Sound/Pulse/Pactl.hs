{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields, NamedFieldPuns #-}

module Sound.Pulse.Pactl
  ( listPulseCards
  , togglePulseCardProfile
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON(..), ToJSON(..), decode)
import Data.Aeson.Types (Options(..), defaultOptions, genericParseJSON, genericToJSON)
import Data.List (find)
import Data.Map (Map)
import Data.String (IsString(..))
import GHC.Generics (Generic)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

data Card = Card
  { index :: Int
  , name :: String
  , driver :: String
  , owner_module :: String
  , properties :: Map String String
  , profiles :: Map String Profile
  , active_profile :: String
  , ports :: Map String Port
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Profile = Profile
  { description :: String
  , sinks :: Int
  , sources :: Int
  , priority :: Int
  , available :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Port = Port
  { description :: String
  , _type :: String
  , priority :: Int
  , latency_offset :: String
  , availability_group :: String
  , availability :: String
  , properties :: Map String String
  , profiles :: [String]
  }
  deriving (Show, Generic)

instance ToJSON Port where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = keywordFieldLabelModifier }

instance FromJSON Port where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = keywordFieldLabelModifier }

keywordFieldLabelModifier :: String -> String
keywordFieldLabelModifier "_type" = "type"
keywordFieldLabelModifier x = x

listPulseCards :: (MonadIO m) => m (Maybe [Card])
listPulseCards = do
  output <- runProcessWithInput "pactl" ["-f", "json", "list", "cards"] ""
  return $ decode $ fromString output

togglePulseCardProfile :: (MonadIO m) => (String, String) -> m ()
togglePulseCardProfile (profile1, profile2) = do
  cards <- listPulseCards
  case cards >>= find isAlsaCard of
    (Just (Card { name, active_profile })) ->
      let profile = if profile1 == active_profile then profile2 else profile1
      in safeSpawn "pactl" ["set-card-profile", name, profile]
    _ -> return ()
  where
    isAlsaCard (Card { driver })
      | driver == "module-alsa-card.c" = True
      | otherwise                      = False
