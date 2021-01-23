{-# LANGUAGE CPP, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Files
-- Copyright   :  (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Juraj Hercek <juhe_haskell@hck.sk>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Specialized helpers to access files and their contents
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Common.Files (checkedDataRetrieval) where

#if __GLASGOW_HASKELL__ < 800
import Control.Applicative
#endif

import Data.Char hiding (Space)
import Data.Function
import Data.List
import Data.Maybe
import System.Directory

import Xmobar.Plugins.Monitors.Common.Types
import Xmobar.Plugins.Monitors.Common.Parsers
import Xmobar.Plugins.Monitors.Common.Output

checkedDataRetrieval :: (Ord a, Num a)
                     => String -> [[String]] -> Maybe (String, String -> Int)
                     -> (Double -> a) -> (a -> String) -> Monitor String
checkedDataRetrieval msg paths lbl trans fmt =
  fmap (fromMaybe msg . listToMaybe . catMaybes) $
    mapM (\p -> retrieveData p lbl trans fmt) paths

retrieveData :: (Ord a, Num a)
             => [String] -> Maybe (String, String -> Int)
             -> (Double -> a) -> (a -> String) -> Monitor (Maybe String)
retrieveData path lbl trans fmt = do
  pairs <- map snd . sortBy (compare `on` fst) <$>
             (mapM readFiles =<< findFilesAndLabel path lbl)
  if null pairs
    then return Nothing
    else Just <$> (     parseTemplate
                    =<< mapM (showWithColors fmt . trans . read) pairs
                  )

-- | Represents the different types of path components
data Comp = Fix String
          | Var [String]
          deriving Show

-- | Used to represent parts of file names separated by slashes and spaces
data CompOrSep = Slash
               | Space
               | Comp String
               deriving (Eq, Show)

-- | Function to turn a list of of strings into a list of path components
pathComponents :: [String] -> [Comp]
pathComponents = joinComps . drop 2 . intercalate [Space] . map splitParts
  where
    splitParts p | (l, _:r) <- break (== '/') p = Comp l : Slash : splitParts r
                 | otherwise                    = [Comp p]

    joinComps = uncurry joinComps' . partition isComp

    isComp (Comp _) = True
    isComp _        = False

    fromComp (Comp s) = s
    fromComp _        = error "fromComp applied to value other than (Comp _)"

    joinComps' cs []     = [Fix $ fromComp $ head cs] -- cs should have only one element here,
                                                      -- but this keeps the pattern matching
                                                      -- exhaustive
    joinComps' cs (p:ps) = let (ss, ps') = span (== p) ps
                               ct        = if null ps' || (p == Space) then length ss + 1
                                                                       else length ss
                               (ls, rs)  = splitAt (ct+1) cs
                               c         = case p of
                                             Space -> Var $ map fromComp ls
                                             Slash -> Fix $ intercalate "/" $ map fromComp ls
                                             _     -> error "Should not happen"
                           in  if null ps' then [c]
                                           else c:joinComps' rs (drop ct ps)

-- | Function to find all files matching the given path and possible label file.
-- The path must be absolute (start with a leading slash).
findFilesAndLabel :: [String] -> Maybe (String, String -> Int)
          -> Monitor [(String, Either Int (String, String -> Int))]
findFilesAndLabel path lbl  =  catMaybes
                   <$> (     mapM addLabel . zip [0..] . sort
                         =<< recFindFiles (pathComponents path) "/"
                       )
  where
    addLabel (i, f) = maybe (return $ Just (f, Left i))
                            (uncurry (justIfExists f))
                            lbl

    justIfExists f s t = let f' = take (length f - length s) f ++ s
                         in  ifthen (Just (f, Right (f', t))) Nothing <$> io (doesFileExist f')

    recFindFiles [] d  =  ifthen [d] []
                      <$> io (if null d then return False else doesFileExist d)
    recFindFiles ps d  =  ifthen (recFindFiles' ps d) (return [])
                      =<< io (if null d then return True else doesDirectoryExist d)

    recFindFiles' []         _  =  error "Should not happen"
    recFindFiles' (Fix p:ps) d  =  recFindFiles ps (d ++ "/" ++ p)
    recFindFiles' (Var p:ps) d  =  concat
                               <$> ((mapM (recFindFiles ps
                                           . (\f -> d ++ "/" ++ f))
                                      . filter (matchesVar p))
                                     =<< io (getDirectoryContents d)
                                   )

    matchesVar []     _  = False
    matchesVar [v]    f  = v == f
    matchesVar (v:vs) f  = let f'  = drop (length v) f
                               f'' = dropWhile isDigit f'
                           in  and [ v `isPrefixOf` f
                                   , not (null f')
                                   , isDigit (head f')
                                   , matchesVar vs f''
                                   ]

-- | Function to read the contents of the given file(s)
readFiles :: (String, Either Int (String, String -> Int))
          -> Monitor (Int, String)
readFiles (fval, flbl) = (,) <$> either return (\(f, ex) -> fmap ex
                                                            $ io $ readFile f) flbl
                             <*> io (readFile fval)

-- | Function that captures if-then-else
ifthen :: a -> a -> Bool -> a
ifthen thn els cnd = if cnd then thn else els
