{-#LANGUAGE RecordWildCards#-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Plugins.Monitors.Strings
-- Copyright: (c) 2018, 2019, 2020 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Dec 02, 2018 04:25
--
--
-- Utilities for formatting monitor outputs
--
------------------------------------------------------------------------------


module Xmobar.Plugins.Monitors.Common.Output ( IconPattern
                                             , parseIconPattern
                                             , padString
                                             , colorizeString
                                             , showWithPadding
                                             , showWithColors
                                             , showWithColors'
                                             , showPercentWithColors
                                             , showPercentsWithColors
                                             , showPercentBar
                                             , showVerticalBar
                                             , showIconPattern
                                             , showLogBar
                                             , showLogVBar
                                             , showLogIconPattern
                                             , showWithUnits
                                             , takeDigits
                                             , showDigits
                                             , floatToPercent
                                             , parseFloat
                                             , parseInt
                                             , stringParser
                                             , pShowPercentsWithColors
                                             , pShowPercentBar
                                             , pShowVerticalBar
                                             , pShowIconPattern
                                             , pShowPercentWithColors
                                             ) where

import Data.Char
import Data.List (intercalate, sort)
import qualified Data.ByteString.Lazy.Char8 as B
import Numeric
import Control.Monad (zipWithM)
import Control.Monad.IO.Class (MonadIO(..))
import Xmobar.Plugins.Monitors.Common.Types

type IconPattern = Int -> String

pShowVerticalBar :: (MonadIO m) => MonitorConfig -> Float -> Float -> m String
pShowVerticalBar p v x = pColorizeString p v [convert $ 100 * x]
  where convert :: Float -> Char
        convert val
          | t <= 9600 = ' '
          | t > 9608 = chr 9608
          | otherwise = chr t
          where t = 9600 + (round val `div` 12)

pShowPercentsWithColors :: (MonadIO m) => MonitorConfig -> [Float] -> m [String]
pShowPercentsWithColors p fs =
  do let fstrs = map (pFloatToPercent p) fs
         temp = map (*100) fs
     zipWithM (pShowWithColors p . const) fstrs temp

pShowPercentWithColors :: (MonadIO m) => MonitorConfig -> Float -> m String
pShowPercentWithColors p f = fmap head $ pShowPercentsWithColors p [f]

pShowPercentBar :: (MonadIO m) => MonitorConfig -> Float -> Float -> m String
pShowPercentBar p@MonitorConfig{..} v x = do
  let len = min pBarWidth $ round (fromIntegral pBarWidth * x)
  s <- pColorizeString p v (take len $ cycle pBarFore)
  return $ s ++ take (pBarWidth - len) (cycle pBarBack)

pShowWithColors :: (Num a, Ord a, MonadIO m) => MonitorConfig -> (a -> String) -> a -> m String
pShowWithColors p f x = do
  let str = pShowWithPadding p (f x)
  pColorizeString p x str

pColorizeString :: (Num a, Ord a, MonadIO m) => MonitorConfig -> a -> String -> m String
pColorizeString p x s = do
    let col = pSetColor p s
        [ll,hh] = map fromIntegral $ sort [pLow p, pHigh p] -- consider high < low
    pure $ head $ [col pHighColor   | x > hh ] ++
                  [col pNormalColor | x > ll ] ++
                  [col pLowColor    | True]

pSetColor :: MonitorConfig -> String -> PSelector (Maybe String) -> String
pSetColor config str s =
    do let a = getPConfigValue config s
       case a of
            Nothing -> str
            Just c -> "<fc=" ++ c ++ ">" ++ str ++ "</fc>"

pShowWithPadding :: MonitorConfig -> String -> String
pShowWithPadding MonitorConfig {..} =
  padString pMinWidth pMaxWidth pPadChars pPadRight pMaxWidthEllipsis

pFloatToPercent :: MonitorConfig -> Float -> String
pFloatToPercent MonitorConfig{..} n = let p = showDigits 0 (n * 100)
                                          ps = if pUseSuffix then "%" else ""
                                      in padString pPpad pPpad pPadChars pPadRight "" p ++ ps

parseIconPattern :: String -> IconPattern
parseIconPattern path =
    let spl = splitOnPercent path
    in \i -> intercalate (show i) spl
  where splitOnPercent [] = [[]]
        splitOnPercent ('%':'%':xs) = [] : splitOnPercent xs
        splitOnPercent (x:xs) =
            let rest = splitOnPercent xs
            in (x : head rest) : tail rest

type Pos = (Int, Int)

takeDigits :: Int -> Float -> Float
takeDigits d n =
    fromIntegral (round (n * fact) :: Int) / fact
  where fact = 10 ^ d

showDigits :: (RealFloat a) => Int -> a -> String
showDigits d n = showFFloat (Just d) n ""

showWithUnits :: Int -> Int -> Float -> String
showWithUnits d n x
  | x < 0 = '-' : showWithUnits d n (-x)
  | n > 3 || x < 10^(d + 1) = show (round x :: Int) ++ units n
  | x <= 1024 = showDigits d (x/1024) ++ units (n+1)
  | otherwise = showWithUnits d (n+1) (x/1024)
  where units = (!!) ["B", "K", "M", "G", "T"]

padString :: Int -> Int -> String -> Bool -> String -> String -> String
padString mnw mxw pad pr ellipsis s =
  let len = length s
      rmin = if mnw <= 0 then 1 else mnw
      rmax = if mxw <= 0 then max len rmin else mxw
      (rmn, rmx) = if rmin <= rmax then (rmin, rmax) else (rmax, rmin)
      rlen = min (max rmn len) rmx
  in if rlen < len then
       take rlen s ++ ellipsis
     else let ps = take (rlen - len) (cycle pad)
          in if pr then s ++ ps else ps ++ s

parseFloat :: String -> Float
parseFloat s = case readFloat s of
  (v, _):_ -> v
  _ -> 0

parseInt :: String -> Int
parseInt s = case readDec s of
  (v, _):_ -> v
  _ -> 0

floatToPercent :: Float -> Monitor String
floatToPercent n =
  do pad <- getConfigValue ppad
     pc <- getConfigValue padChars
     pr <- getConfigValue padRight
     up <- getConfigValue useSuffix
     let p = showDigits 0 (n * 100)
         ps = if up then "%" else ""
     return $ padString pad pad pc pr "" p ++ ps

stringParser :: Pos -> B.ByteString -> String
stringParser (x,y) =
     B.unpack . li x . B.words . li y . B.lines
    where li i l | length l > i = l !! i
                 | otherwise    = B.empty

setColor :: String -> Selector (Maybe String) -> Monitor String
setColor str s =
    do a <- getConfigValue s
       case a of
            Nothing -> return str
            Just c -> return $
                "<fc=" ++ c ++ ">" ++ str ++ "</fc>"

showWithPadding :: String -> Monitor String
showWithPadding s =
    do mn <- getConfigValue minWidth
       mx <- getConfigValue maxWidth
       p <- getConfigValue padChars
       pr <- getConfigValue padRight
       ellipsis <- getConfigValue maxWidthEllipsis
       return $ padString mn mx p pr ellipsis s

colorizeString :: (Num a, Ord a) => a -> String -> Monitor String
colorizeString x s = do
    h <- getConfigValue high
    l <- getConfigValue low
    let col = setColor s
        [ll,hh] = map fromIntegral $ sort [l, h] -- consider high < low
    head $ [col highColor   | x > hh ] ++
           [col normalColor | x > ll ] ++
           [col lowColor    | True]

showWithColors :: (Num a, Ord a) => (a -> String) -> a -> Monitor String
showWithColors f x = showWithPadding (f x) >>= colorizeString x

showWithColors' :: (Num a, Ord a) => String -> a -> Monitor String
showWithColors' str = showWithColors (const str)

showPercentsWithColors :: [Float] -> Monitor [String]
showPercentsWithColors fs =
  do fstrs <- mapM floatToPercent fs
     zipWithM (showWithColors . const) fstrs (map (*100) fs)

showPercentWithColors :: Float -> Monitor String
showPercentWithColors f = fmap head $ showPercentsWithColors [f]

showPercentBar :: Float -> Float -> Monitor String
showPercentBar v x = do
  bb <- getConfigValue barBack
  bf <- getConfigValue barFore
  bw <- getConfigValue barWidth
  let c = bw < 1
      w = if c then length bf else bw
      len = min w $ round (fromIntegral w * x)
      bfs = if c then [bf !! max 0 (len - 1)] else take len $ cycle bf
  s <- colorizeString v bfs
  return $ s ++ if c then "" else take (bw - len) (cycle bb)

showIconPattern :: Maybe IconPattern -> Float -> Monitor String
showIconPattern Nothing _ = return ""
showIconPattern (Just str) x = return $ str $ convert $ 100 * x
  where convert val
          | t <= 0 = 0
          | t > 8 = 8
          | otherwise = t
          where t = round val `div` 12

pShowIconPattern :: Maybe IconPattern -> Float -> IO String
pShowIconPattern Nothing _ = return ""
pShowIconPattern (Just str) x = return $ str $ convert $ 100 * x
  where convert val
          | t <= 0 = 0
          | t > 8 = 8
          | otherwise = t
          where t = round val `div` 12

showVerticalBar :: Float -> Float -> Monitor String
showVerticalBar v x = colorizeString v [convert $ 100 * x]
  where convert :: Float -> Char
        convert val
          | t <= 9600 = ' '
          | t > 9608 = chr 9608
          | otherwise = chr t
          where t = 9600 + (round val `div` 12)

logScaling :: Float -> Float -> Monitor Float
logScaling f v = do
  h <- fromIntegral `fmap` getConfigValue high
  l <- fromIntegral `fmap` getConfigValue low
  bw <- fromIntegral `fmap` getConfigValue barWidth
  let [ll, hh] = sort [l, h]
      scaled x | x == 0.0 = 0
               | x <= ll = 1 / bw
               | otherwise = f + logBase 2 (x / hh) / bw
  return $ scaled v

showLogBar :: Float -> Float -> Monitor String
showLogBar f v = logScaling f v >>= showPercentBar v

showLogVBar :: Float -> Float -> Monitor String
showLogVBar f v = logScaling f v >>= showVerticalBar v

showLogIconPattern :: Maybe IconPattern -> Float -> Float -> Monitor String
showLogIconPattern str f v = logScaling f v >>= showIconPattern str
