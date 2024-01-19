{-# LANGUAGE ScopedTypeVariables #-}

module Xmobar.Monitors.CatNum (CatNum (..)) where

import Control.Exception (IOException, catch)
import Numeric (showFFloat)
import System.Console.GetOpt (ArgDescr (..), OptDescr (..))
import Xmobar (Args, Exec (..), Rate)
import Xmobar.Plugins.Monitors.Common (MConfig (..), Monitor, getConfigValue, io, mkMConfig, parseFloat, parseOptsWith, parseTemplate, runM, showDigits, showWithColors)

data CatNum = CatNum String [FilePath] Args Rate
  deriving (Read, Show)

data CatNumOpts = CatNumOpts
  { catNumDivier :: Float,
    catNumSuffix :: String
  }

defaultOpts :: CatNumOpts
defaultOpts =
  CatNumOpts
    { catNumDivier = 1,
      catNumSuffix = ""
    }

optDescriptions :: [OptDescr (CatNumOpts -> CatNumOpts)]
optDescriptions =
  [ Option "D" ["divier"] (ReqArg (\x o -> o {catNumDivier = parseFloat x}) "") "",
    Option "S" ["suffix"] (ReqArg (\x o -> o {catNumSuffix = x}) "") ""
  ]

data UnitPrefix = Kilo | Mega | Giga | Tera | Peta | Exa | Zetta | Yotta

unitPrefixSymbol :: UnitPrefix -> String
unitPrefixSymbol Kilo = "K"
unitPrefixSymbol Mega = "M"
unitPrefixSymbol Giga = "G"
unitPrefixSymbol Tera = "T"
unitPrefixSymbol Peta = "P"
unitPrefixSymbol Exa = "E"
unitPrefixSymbol Zetta = "Z"
unitPrefixSymbol Yotta = "Y"

unitPrefixScale :: (Floating a) => UnitPrefix -> a
unitPrefixScale Kilo = 10 ** 3
unitPrefixScale Mega = 10 ** 6
unitPrefixScale Giga = 10 ** 9
unitPrefixScale Tera = 10 ** 12
unitPrefixScale Peta = 10 ** 15
unitPrefixScale Exa = 10 ** 18
unitPrefixScale Zetta = 10 ** 21
unitPrefixScale Yotta = 10 ** 24

calcUnitPrefix :: (Floating a, Ord a) => a -> Maybe UnitPrefix
calcUnitPrefix n
  | n < 10 ** 3 = Nothing
  | n < 10 ** 6 = Just Kilo
  | n < 10 ** 9 = Just Mega
  | n < 10 ** 12 = Just Giga
  | n < 10 ** 15 = Just Tera
  | n < 10 ** 18 = Just Peta
  | n < 10 ** 21 = Just Exa
  | n < 10 ** 24 = Just Zetta
  | otherwise = Just Yotta

formatUnitNum :: (RealFloat a) => Int -> a -> String
formatUnitNum digits n =
  let prefix = calcUnitPrefix n
      scale = maybe 1 unitPrefixScale prefix
      symbol = maybe "" unitPrefixSymbol prefix
   in showFFloat (Just digits) (n / scale) symbol

catNumConfig :: Int -> IO MConfig
catNumConfig numExports = mkMConfig "<n>" $ map (('n' :) . show) [0 .. numExports]

safeReadFile :: FilePath -> IO String
safeReadFile path = catch (readFile path) $ \(_ :: IOException) -> return ""

safeDiv :: (Eq a, Fractional a) => a -> a -> a
safeDiv n d
  | d /= 0 = n / d
  | otherwise = n

runCatNum :: [FilePath] -> [String] -> Monitor String
runCatNum paths argv = do
  digits <- getConfigValue decDigits
  hasUnit <- getConfigValue useSuffix
  opts <- io $ parseOptsWith optDescriptions defaultOpts argv
  let divier = catNumDivier opts
      suffix = catNumSuffix opts
      format = (++ suffix) . (if hasUnit then formatUnitNum digits else showDigits digits) . (`safeDiv` divier)
  mapM ((>>= (showWithColors format . parseFloat)) . io . safeReadFile) paths >>= parseTemplate

instance Exec CatNum where
  alias (CatNum name _ _ _) = name
  start (CatNum _ paths args rate) = runM args (catNumConfig $ length paths) (runCatNum paths) rate
