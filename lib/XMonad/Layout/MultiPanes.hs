{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module XMonad.Layout.MultiPanes
  ( MultiPanes(..)
  , ResizeClient(..)
  , Pane(..)
  , PaneStrategy(..)
  )
where

import Control.Monad (guard, msum)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Monoid (Last(..))
import qualified Data.IntMap as IM
import qualified Data.Map as M

import XMonad (Rectangle(..), gets)
import XMonad.Core (LayoutClass(..), Message, XState(..), fromMessage)
import XMonad.Layout (IncMasterN(..), Resize(..))
import qualified XMonad.StackSet as W

data MultiPanes a = MultiPanes
  { multiPanesPaneIncrement :: Rational
  , multiPanesClientIncrement :: Rational
  , multiPanesPanes :: [Pane]
  }
  deriving (Read, Show)

instance LayoutClass MultiPanes a where
  doLayout (MultiPanes _ _ panes) rectangle stack =
    let windows = W.integrate stack
        rectangles = tile panes rectangle $ length windows
     in return (zip windows rectangles, Nothing)

  handleMessage layout@(MultiPanes _ _ panes) message = do
    currentStack <- W.stack . W.workspace . W.current <$> gets windowset
    floatingWindows <- M.keys . W.floating <$> gets windowset
    return $ do
      stack <- currentStack

      guard $ W.focus stack `notElem` floatingWindows

      let stack' = stack { W.up = W.up stack \\ floatingWindows
                         , W.down = W.down stack \\ floatingWindows
                         }
          numWindows = length $ W.integrate stack'
          focusIndex = length $ W.up stack'
          clients = placeClients numWindows panes

      (paneIndex, startIndex, endIndex) <- findInclusiveIndex focusIndex clients

      let clientIndex = focusIndex - startIndex
          numClients = endIndex - startIndex
          lastPaneIndex = fromMaybe 0 $
                          getLast $
                          foldMap (Last . Just . fst) $
                          filter ((> 0) . snd) $
                          zip [0..] clients

      msum [ fmap (handlePaneResize layout paneIndex lastPaneIndex) $ fromMessage message
           , fmap (handleResizeClient layout paneIndex clientIndex numClients) $ fromMessage message
           , fmap (handleIncMaster layout paneIndex) $ fromMessage message
           ]

  description _ = "MultiPanes"

findInclusiveIndex :: (Num a, Ord a) => a -> [a] -> Maybe (a, a, a)
findInclusiveIndex index sizes = findInclusiveIndex' 0 0 index sizes

findInclusiveIndex' :: (Num a, Ord a) => a -> a -> a -> [a] -> Maybe (a, a, a)
findInclusiveIndex' _ _ _ [] = Nothing
findInclusiveIndex' cursor start index (size:sizes)
  | start <= index && index < end = Just (cursor, start, end)
  | otherwise                     = findInclusiveIndex' (cursor + 1) (start + size) index sizes
  where
    end = start + size

type NumWindows = Int

type NumClients = Int

type PaneIndex = Int

type ClientIndex = Int

handlePaneResize :: MultiPanes a -> PaneIndex -> PaneIndex -> Resize -> MultiPanes a
handlePaneResize (MultiPanes paneIncrement clientIncrement panes) index lastPaneIndex message =
  MultiPanes paneIncrement clientIncrement $ (<$> zip [0..] panes) $
    case (message, index == lastPaneIndex) of
      (Shrink, True)  -> apply [(index, doExpand), (index - 1, doShrink)]
      (Shrink, False) -> apply [(index, doShrink), (index + 1, doExpand)]
      (Expand, True)  -> apply [(index, doShrink), (index - 1, doExpand)]
      (Expand, False) -> apply [(index, doExpand), (index + 1, doShrink)]
  where
    apply [] (_, pane) = pane
    apply ((i, f):fs) (j, pane) = apply fs $
      if i == j
        then (j, pane { paneRatio = f (paneRatio pane) })
        else (j, pane)
    doShrink ratio = ratio - paneIncrement / 2
    doExpand ratio = ratio + paneIncrement / 2

data ResizeClient = ShrinkClient | ExpandClient
  deriving (Show)

instance Message ResizeClient

handleResizeClient :: MultiPanes a -> PaneIndex -> ClientIndex -> NumClients -> ResizeClient -> MultiPanes a
handleResizeClient (MultiPanes paneIncrement clientIncrement panes) paneIndex clientIndex numClients message =
  MultiPanes paneIncrement clientIncrement $ apply paneIndex (resizeClient message clientIndex numClients clientIncrement) <$> zip [0..] panes
  where
    apply i f (j, pane) = if i == j
      then pane { paneClientRatios = f (paneClientRatios pane) }
      else pane

resizeClient :: RealFrac a => ResizeClient -> ClientIndex -> NumClients -> a -> [a] -> [a]
resizeClient message clientIndex numClients increment ratios =
  (<$> take numClients (zip [0..] $ ratios ++ repeat 1)) $ case (message, clientIndex == numClients - 1) of
    (ShrinkClient, True)  -> apply [(clientIndex, doExpand), (clientIndex - 1, doShrink)]
    (ShrinkClient, False) -> apply [(clientIndex, doShrink), (clientIndex + 1, doExpand)]
    (ExpandClient, True)  -> apply [(clientIndex, doShrink), (clientIndex - 1, doExpand)]
    (ExpandClient, False) -> apply [(clientIndex, doExpand), (clientIndex + 1, doShrink)]
  where
    apply [] (_, ratio) = ratio
    apply ((i, f):fs) (index, ratio) = apply fs $
      if i == index
        then (index, f ratio)
        else (index, ratio)
    doShrink ratio = ratio - increment / 2
    doExpand ratio = ratio + increment / 2

handleIncMaster :: MultiPanes a -> Int -> IncMasterN -> MultiPanes a
handleIncMaster (MultiPanes paneIncrement clientIncrement panes) index (IncMasterN n) =
  MultiPanes paneIncrement clientIncrement $ apply index (incrementCapacity n) <$> zip [0..] panes
  where
    apply i f (j, pane) = if i == j
      then pane { paneStrategy = f (paneStrategy pane) }
      else pane

data Pane = Pane
  { paneStrategy :: PaneStrategy
  , paneRatio :: Rational
  , paneClientRatios :: [Rational]
  }
  deriving (Read, Show)

data PaneStrategy
  = PaneStrategyAtMost Int
  | PaneStrategyAtLeast Int
  deriving (Read, Show)

getReservedCapacity :: PaneStrategy -> Int
getReservedCapacity (PaneStrategyAtMost n) = n
getReservedCapacity (PaneStrategyAtLeast n) = n

incrementCapacity :: Int -> PaneStrategy -> PaneStrategy
incrementCapacity delta (PaneStrategyAtMost n) = PaneStrategyAtMost (max 1 (n + delta))
incrementCapacity delta (PaneStrategyAtLeast n) = PaneStrategyAtLeast (max 0 (n + delta))

tile :: [Pane] -> Rectangle -> NumWindows -> [Rectangle]
tile panes viewport numWindows =
  let clients = placeClients numWindows panes
      paneRectangles = splitHorizontally (map getFraction $ zip panes clients) viewport
      clientRectangles = map calcClientRectangles $
                         zip3 panes clients paneRectangles
   in concat clientRectangles
  where
    getFraction (pane, numClients)
      | numClients > 0 = paneRatio pane
      | otherwise      = 0

    calcClientRectangles (pane, numClients, rectangle) =
      splitVertically (take numClients $ paneClientRatios pane ++ repeat 1) rectangle

placeClients :: NumWindows -> [Pane] -> [NumClients]
placeClients numWindows panes =
  let indexedPanes = zip [0..] $ panes
      primaryPanes = indexedPanes
      secondaryPanes = filter (isSecondary . paneStrategy . snd) $ indexedPanes
   in map snd $ IM.toAscList $ foldr
    (uncurry $ IM.insertWith (+))
    IM.empty
    (placeClients' numWindows primaryPanes $ secondaryPanes `orElse` primaryPanes)
  where
    orElse [] ys = ys
    orElse xs _ = xs

    isSecondary (PaneStrategyAtMost _) = False
    isSecondary (PaneStrategyAtLeast _) = True

placeClients' :: NumWindows -> [(PaneIndex, Pane)] -> [(PaneIndex, Pane)] -> [(PaneIndex, NumClients)]
placeClients' _ [] [] = []
placeClients' 0 primaryPanes _ = zip (map fst primaryPanes) (repeat 0)
placeClients' numWindows [] secondaryPanes =
  let (division, remainder) = quotRem numWindows $ length secondaryPanes
      (xs, ys) = splitAt remainder secondaryPanes
   in map ((, division + 1) . fst) xs ++ map ((, division) . fst) ys
placeClients' numWindows ((index, pane):primaryPanes) secondaryPanes =
  let capacity = getReservedCapacity $ paneStrategy pane
      fills    = min numWindows capacity
   in (index, fills):placeClients' (numWindows - fills) primaryPanes secondaryPanes

splitHorizontally :: RealFrac a => [a] -> Rectangle -> [Rectangle]
splitHorizontally ratios viewport = splitHorizontally' ratios (sum ratios) viewport

splitHorizontally' :: RealFrac a => [a] -> a -> Rectangle -> [Rectangle]
splitHorizontally' [] _ _ = []
splitHorizontally' (_:[]) _ viewport = viewport:[]
splitHorizontally' (ratio:ratios) totalRatios (Rectangle x y width height) =
  let rectangle = Rectangle
        { rect_x = x
        , rect_y = y
        , rect_width = floor $ (fromIntegral width) * (ratio / totalRatios)
        , rect_height = height
        }
      nextViewport = Rectangle
        { rect_x = x + (fromIntegral $ rect_width rectangle)
        , rect_y = y
        , rect_width = width - rect_width rectangle
        , rect_height = height
        }
      in rectangle:splitHorizontally' ratios (totalRatios - ratio) nextViewport

splitVertically :: RealFrac a => [a] -> Rectangle -> [Rectangle]
splitVertically ratios viewport = splitVertically' ratios (sum ratios) viewport

splitVertically' :: RealFrac a => [a] -> a -> Rectangle -> [Rectangle]
splitVertically' [] _ _ = []
splitVertically' (_:[]) _ viewport = viewport:[]
splitVertically' (ratio:ratios) totalRatios (Rectangle x y width height) =
  let rectangle = Rectangle
        { rect_x = x
        , rect_y = y
        , rect_width = width
        , rect_height = floor $ (fromIntegral height) * (ratio / totalRatios)
        }
      nextViewport = Rectangle
        { rect_x = x
        , rect_y = y + (fromIntegral $ rect_height rectangle)
        , rect_width = width
        , rect_height = height - rect_height rectangle
        }
      in rectangle:splitVertically' ratios (totalRatios - ratio) nextViewport
