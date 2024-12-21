{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module XMonad.Layout.StableColumns
  ( StableColumns,
    ResizeRow (..),
    Column,
    stableColumns,
    staticColumn,
    dynamicColumn,
  )
where

import Control.Monad (guard, msum)
import Data.IntMap qualified as IM
import Data.List ((\\))
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid (Last (..))
import XMonad (Rectangle (..), gets)
import XMonad.Core (LayoutClass (..), Message, XState (..), fromMessage)
import XMonad.Layout (IncMasterN (..), Resize (..))
import XMonad.StackSet qualified as W

data StableColumns a = StableColumns
  { stableColumnsColumnIncrement :: Rational,
    stableColumnsRowIncrement :: Rational,
    stableColumnsColumns :: [Column]
  }
  deriving (Read, Show)

instance LayoutClass StableColumns a where
  doLayout (StableColumns _ _ columns) rectangle stack =
    let windows = W.integrate stack
        rectangles = concat $ tile columns rectangle $ length windows
     in return (zip windows rectangles, Nothing)

  handleMessage layout@(StableColumns _ _ columns) message = do
    currentStack <- W.stack . W.workspace . W.current <$> gets windowset
    floatingWindows <- M.keys . W.floating <$> gets windowset
    return $ do
      stack <- currentStack

      guard $ W.focus stack `notElem` floatingWindows

      let stack' =
            stack
              { W.up = W.up stack \\ floatingWindows,
                W.down = W.down stack \\ floatingWindows
              }
          numWindows = length $ W.integrate stack'
          focusIndex = length $ W.up stack'
          rows = placeRows numWindows columns

      (columnIndex, startIndex, endIndex) <- inclusiveIndex focusIndex rows

      let rowIndex = focusIndex - startIndex
          numRows = endIndex - startIndex
          lastColumnIndex =
            fromMaybe 0 $
              getLast $
                foldMap (Last . Just . fst) $
                  filter ((> 0) . snd) $
                    indexed rows

      msum
        [ handleResizeColumn layout columnIndex lastColumnIndex <$> fromMessage message,
          handleResizeRow layout columnIndex rowIndex numRows <$> fromMessage message,
          handleIncMaster layout columnIndex <$> fromMessage message
        ]

  description _ = "StableColumns"

stableColumns :: Rational -> Rational -> [Column] -> StableColumns a
stableColumns = StableColumns

inclusiveIndex :: (Num a, Ord a) => a -> [a] -> Maybe (a, a, a)
inclusiveIndex = inclusiveIndex' 0 0

inclusiveIndex' :: (Num a, Ord a) => a -> a -> a -> [a] -> Maybe (a, a, a)
inclusiveIndex' _ _ _ [] = Nothing
inclusiveIndex' cursor start index (size : sizes)
  | start <= index && index < end = Just (cursor, start, end)
  | otherwise = inclusiveIndex' (cursor + 1) (start + size) index sizes
  where
    end = start + size

type NumWindows = Int

type NumRows = Int

type ColumnIndex = Int

type RowIndex = Int

handleResizeColumn :: StableColumns a -> ColumnIndex -> ColumnIndex -> Resize -> StableColumns a
handleResizeColumn (StableColumns columnIncrement rowIncrement columns) columnIndex lastColumnIndex message =
  StableColumns columnIncrement rowIncrement $
    (<$> indexed columns) $
      case (message, columnIndex == lastColumnIndex) of
        (Shrink, True) -> apply [(columnIndex, doExpand), (columnIndex - 1, doShrink)]
        (Shrink, False) -> apply [(columnIndex, doShrink), (columnIndex + 1, doExpand)]
        (Expand, True) -> apply [(columnIndex, doShrink), (columnIndex - 1, doExpand)]
        (Expand, False) -> apply [(columnIndex, doExpand), (columnIndex + 1, doShrink)]
  where
    apply [] (_, column) = column
    apply ((i, f) : fs) (index, column)
      | i == index = apply fs (index, column {columnRatio = f (columnRatio column)})
      | otherwise = apply fs (index, column)
    doShrink ratio = ratio - columnIncrement / 2
    doExpand ratio = ratio + columnIncrement / 2

data ResizeRow = ShrinkRow | ExpandRow
  deriving (Show)

instance Message ResizeRow

handleResizeRow :: StableColumns a -> ColumnIndex -> RowIndex -> NumRows -> ResizeRow -> StableColumns a
handleResizeRow (StableColumns columnIncrement rowIncrement columns) columnIndex rowIndex numRows message =
  StableColumns columnIncrement rowIncrement $
    apply columnIndex (resizeRow rowIndex numRows message rowIncrement) <$> indexed columns
  where
    apply i f (index, column)
      | i == index = column {columnRowRatios = f (columnRowRatios column)}
      | otherwise = column

resizeRow :: (RealFrac a) => RowIndex -> NumRows -> ResizeRow -> a -> [a] -> [a]
resizeRow rowIndex numRows message increment ratios =
  (<$> take numRows (indexed $ ratios ++ repeat 1)) $
    case (message, rowIndex == numRows - 1) of
      (ShrinkRow, True) -> apply [(rowIndex, doExpand), (rowIndex - 1, doShrink)]
      (ShrinkRow, False) -> apply [(rowIndex, doShrink), (rowIndex + 1, doExpand)]
      (ExpandRow, True) -> apply [(rowIndex, doShrink), (rowIndex - 1, doExpand)]
      (ExpandRow, False) -> apply [(rowIndex, doExpand), (rowIndex + 1, doShrink)]
  where
    apply [] (_, ratio) = ratio
    apply ((i, f) : fs) (index, ratio)
      | i == index = apply fs (index, f ratio)
      | otherwise = apply fs (index, ratio)
    doShrink ratio = ratio - increment / 2
    doExpand ratio = ratio + increment / 2

handleIncMaster :: StableColumns a -> ColumnIndex -> IncMasterN -> StableColumns a
handleIncMaster (StableColumns columnIncrement rowIncrement columns) columnIndex (IncMasterN n) =
  StableColumns columnIncrement rowIncrement $
    apply columnIndex <$> indexed columns
  where
    apply i (index, column)
      | i == index = column {columnKind = incrementCapacity n (columnKind column)}
      | otherwise = column

data Column = Column
  { columnKind :: ColumnKind,
    columnRatio :: Rational,
    columnRowRatios :: [Rational]
  }
  deriving (Read, Show)

data ColumnKind
  = ColumnKindStatic Int
  | ColumnKindDynamic Int
  deriving (Read, Show)

staticColumn :: Int -> Rational -> [Rational] -> Column
staticColumn n = Column (ColumnKindStatic n)

dynamicColumn :: Int -> Rational -> [Rational] -> Column
dynamicColumn n = Column (ColumnKindDynamic n)

getCapacity :: ColumnKind -> Int
getCapacity (ColumnKindStatic n) = n
getCapacity (ColumnKindDynamic n) = n

incrementCapacity :: Int -> ColumnKind -> ColumnKind
incrementCapacity delta (ColumnKindStatic n) = ColumnKindStatic $ max 1 (n + delta)
incrementCapacity delta (ColumnKindDynamic n) = ColumnKindDynamic $ max 1 (n + delta)

tile :: [Column] -> Rectangle -> NumWindows -> [[Rectangle]]
tile columns viewport numWindows =
  let rows = placeRows numWindows columns
      columnRectangles = splitHorizontally (mapMaybe getRatio $ zip columns rows) viewport
      rowRectangles =
        map calcRowRectangles $
          zip3 columns rows columnRectangles
   in rowRectangles
  where
    getRatio (column, numRows)
      | numRows > 0 = Just $ columnRatio column
      | otherwise = Nothing

    calcRowRectangles (column, numRows, rectangle) =
      splitVertically (take numRows $ columnRowRatios column ++ repeat 1) rectangle

placeRows :: NumWindows -> [Column] -> [NumRows]
placeRows numWindows columns =
  let allColumns = indexed columns
      dynamicColumns = filter (isDynamic . columnKind . snd) allColumns
   in map snd $
        IM.toAscList $
          foldr
            (uncurry $ IM.insertWith (+))
            IM.empty
            (placeRows' numWindows allColumns dynamicColumns)
  where
    isDynamic (ColumnKindStatic _) = False
    isDynamic (ColumnKindDynamic _) = True

placeRows' :: NumWindows -> [(ColumnIndex, Column)] -> [(ColumnIndex, Column)] -> [(ColumnIndex, NumRows)]
placeRows' _ [] [] = []
placeRows' 0 allColumns _ = map ((,0) . fst) allColumns
placeRows' numWindows [] dynamicColumns =
  let (division, remainder) = quotRem numWindows $ length dynamicColumns
      (xs, ys) = splitAt remainder dynamicColumns
   in map ((,division + 1) . fst) xs <> map ((,division) . fst) ys
placeRows' numWindows ((index, column) : allColumns) dynamicColumns =
  let capacity = getCapacity $ columnKind column
      fills = min numWindows capacity
   in (index, fills) : placeRows' (numWindows - fills) allColumns dynamicColumns

splitHorizontally :: (RealFrac a) => [a] -> Rectangle -> [Rectangle]
splitHorizontally ratios = splitHorizontally' ratios (sum ratios)

splitHorizontally' :: (RealFrac a) => [a] -> a -> Rectangle -> [Rectangle]
splitHorizontally' [] _ _ = []
splitHorizontally' [_] _ viewport = [viewport]
splitHorizontally' (ratio : ratios) totalRatios (Rectangle x y width height) =
  let rectangle =
        Rectangle
          { rect_x = x,
            rect_y = y,
            rect_width = floor $ fromIntegral width * (ratio / totalRatios),
            rect_height = height
          }
      nextViewport =
        Rectangle
          { rect_x = x + fromIntegral (rect_width rectangle),
            rect_y = y,
            rect_width = width - rect_width rectangle,
            rect_height = height
          }
   in rectangle : splitHorizontally' ratios (totalRatios - ratio) nextViewport

splitVertically :: (RealFrac a) => [a] -> Rectangle -> [Rectangle]
splitVertically ratios = splitVertically' ratios (sum ratios)

splitVertically' :: (RealFrac a) => [a] -> a -> Rectangle -> [Rectangle]
splitVertically' [] _ _ = []
splitVertically' [_] _ viewport = [viewport]
splitVertically' (ratio : ratios) totalRatios (Rectangle x y width height) =
  let rectangle =
        Rectangle
          { rect_x = x,
            rect_y = y,
            rect_width = width,
            rect_height = floor $ fromIntegral height * (ratio / totalRatios)
          }
      nextViewport =
        Rectangle
          { rect_x = x,
            rect_y = y + fromIntegral (rect_height rectangle),
            rect_width = width,
            rect_height = height - rect_height rectangle
          }
   in rectangle : splitVertically' ratios (totalRatios - ratio) nextViewport

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]
