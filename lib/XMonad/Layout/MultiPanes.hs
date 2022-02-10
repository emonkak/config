{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module XMonad.Layout.MultiPanes
  ( MultiPanes(..)
  , ResizeClient(..)
  , Pane(..)
  , PaneSlot(..)
  )
where

import Control.Monad (guard, msum)
import Data.List ((\\))
import Data.Maybe (fromMaybe, mapMaybe)
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

      (paneIndex, startIndex, endIndex) <- inclusiveIndex focusIndex clients

      let clientIndex = focusIndex - startIndex
          numClients = endIndex - startIndex
          lastPaneIndex = fromMaybe 0 $
                          getLast $
                          foldMap (Last . Just . fst) $
                          filter ((> 0) . snd) $
                          indexed clients

      msum [ fmap (handleResizePane layout paneIndex lastPaneIndex) $ fromMessage message
           , fmap (handleResizeClient layout paneIndex clientIndex numClients) $ fromMessage message
           , fmap (handleIncMaster layout paneIndex) $ fromMessage message
           ]

  description _ = "MultiPanes"

inclusiveIndex :: (Num a, Ord a) => a -> [a] -> Maybe (a, a, a)
inclusiveIndex index sizes = inclusiveIndex' 0 0 index sizes

inclusiveIndex' :: (Num a, Ord a) => a -> a -> a -> [a] -> Maybe (a, a, a)
inclusiveIndex' _ _ _ [] = Nothing
inclusiveIndex' cursor start index (size:sizes)
  | start <= index && index < end = Just (cursor, start, end)
  | otherwise                     = inclusiveIndex' (cursor + 1) (start + size) index sizes
  where
    end = start + size

type NumWindows = Int

type NumClients = Int

type PaneIndex = Int

type ClientIndex = Int

handleResizePane :: MultiPanes a -> PaneIndex -> PaneIndex -> Resize -> MultiPanes a
handleResizePane (MultiPanes paneIncrement clientIncrement panes) paneIndex lastPaneIndex message =
  MultiPanes paneIncrement clientIncrement $ (<$> indexed panes) $
    case (message, paneIndex == lastPaneIndex) of
      (Shrink, True)  -> apply [(paneIndex, doExpand), (paneIndex - 1, doShrink)]
      (Shrink, False) -> apply [(paneIndex, doShrink), (paneIndex + 1, doExpand)]
      (Expand, True)  -> apply [(paneIndex, doShrink), (paneIndex - 1, doExpand)]
      (Expand, False) -> apply [(paneIndex, doExpand), (paneIndex + 1, doShrink)]
  where
    apply [] (_, pane) = pane
    apply ((i, f):fs) (index, pane)
      | i == index = apply fs $ (index, pane { paneRatio = f (paneRatio pane) })
      | otherwise  = apply fs $ (index, pane)
    doShrink ratio = ratio - paneIncrement / 2
    doExpand ratio = ratio + paneIncrement / 2

data ResizeClient = ShrinkClient | ExpandClient
  deriving (Show)

instance Message ResizeClient

handleResizeClient :: MultiPanes a -> PaneIndex -> ClientIndex -> NumClients -> ResizeClient -> MultiPanes a
handleResizeClient (MultiPanes paneIncrement clientIncrement panes) paneIndex clientIndex numClients message =
  MultiPanes paneIncrement clientIncrement $
  apply paneIndex (resizeClient clientIndex numClients message clientIncrement) <$> indexed panes
  where
    apply i f (index, pane)
      | i == index = pane { paneClientRatios = f (paneClientRatios pane) }
      | otherwise  = pane

resizeClient :: RealFrac a => ClientIndex -> NumClients -> ResizeClient -> a -> [a] -> [a]
resizeClient clientIndex numClients message increment ratios =
  (<$> take numClients (indexed $ ratios ++ repeat 1)) $
  case (message, clientIndex == numClients - 1) of
    (ShrinkClient, True)  -> apply [(clientIndex, doExpand), (clientIndex - 1, doShrink)]
    (ShrinkClient, False) -> apply [(clientIndex, doShrink), (clientIndex + 1, doExpand)]
    (ExpandClient, True)  -> apply [(clientIndex, doShrink), (clientIndex - 1, doExpand)]
    (ExpandClient, False) -> apply [(clientIndex, doExpand), (clientIndex + 1, doShrink)]
  where
    apply [] (_, ratio) = ratio
    apply ((i, f):fs) (index, ratio)
      | i == index = apply fs $ (index, f ratio)
      | otherwise  = apply fs $ (index, ratio)
    doShrink ratio = ratio - increment / 2
    doExpand ratio = ratio + increment / 2

handleIncMaster :: MultiPanes a -> PaneIndex -> IncMasterN -> MultiPanes a
handleIncMaster (MultiPanes paneIncrement clientIncrement panes) paneIndex (IncMasterN n) =
  MultiPanes paneIncrement clientIncrement $
  apply paneIndex (incrementCapacity n) <$> indexed panes
  where
    apply i f (index, pane)
      | i == index = pane { paneSlot = f (paneSlot pane) }
      | otherwise  = pane

data Pane = Pane
  { paneSlot :: PaneSlot
  , paneRatio :: Rational
  , paneClientRatios :: [Rational]
  }
  deriving (Read, Show)

data PaneSlot
  = PaneSlotAtMost Int
  | PaneSlotAtLeast Int
  deriving (Read, Show)

getReservedCapacity :: PaneSlot -> Int
getReservedCapacity (PaneSlotAtMost n) = n
getReservedCapacity (PaneSlotAtLeast n) = n

incrementCapacity :: Int -> PaneSlot -> PaneSlot
incrementCapacity delta (PaneSlotAtMost n) = PaneSlotAtMost (max 1 (n + delta))
incrementCapacity delta (PaneSlotAtLeast n) = PaneSlotAtLeast (max 0 (n + delta))

tile :: [Pane] -> Rectangle -> NumWindows -> [Rectangle]
tile panes viewport numWindows =
  let clients = placeClients numWindows panes
      paneRectangles = splitHorizontally (mapMaybe getRatio $ zip panes clients) viewport
      clientRectangles = map calcClientRectangles $
                         zip3 panes clients paneRectangles
   in concat clientRectangles
  where
    getRatio (pane, numClients)
      | numClients > 0 = Just $ paneRatio pane
      | otherwise      = Nothing

    calcClientRectangles (pane, numClients, rectangle) =
      splitVertically (take numClients $ paneClientRatios pane ++ repeat 1) rectangle

placeClients :: NumWindows -> [Pane] -> [NumClients]
placeClients numWindows panes =
  let primaryPanes = indexed panes
      secondaryPanes = filter (isSecondary . paneSlot . snd) $ indexed panes
   in map snd $ IM.toAscList $ foldr
    (uncurry $ IM.insertWith (+))
    IM.empty
    (placeClients' numWindows primaryPanes secondaryPanes)
  where
    isSecondary (PaneSlotAtMost _) = False
    isSecondary (PaneSlotAtLeast _) = True

placeClients' :: NumWindows -> [(PaneIndex, Pane)] -> [(PaneIndex, Pane)] -> [(PaneIndex, NumClients)]
placeClients' _ [] [] = []
placeClients' 0 primaryPanes _ = zip (map fst primaryPanes) (repeat 0)
placeClients' numWindows [] secondaryPanes =
  let (division, remainder) = quotRem numWindows $ length secondaryPanes
      (xs, ys) = splitAt remainder secondaryPanes
   in map ((, division + 1) . fst) xs <> map ((, division) . fst) ys
placeClients' numWindows ((index, pane):primaryPanes) secondaryPanes =
  let capacity = getReservedCapacity $ paneSlot pane
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

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]
