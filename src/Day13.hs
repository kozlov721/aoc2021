{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day13 where

import Data.List.Split (splitOn)
import Data.Matrix     (Matrix)
import Data.Set        (Set)
import Day1            (readInt)

import qualified Data.Bifunctor as Bi
import qualified Data.Matrix    as M
import qualified Data.Set       as Set

data Fold = AlongX Int | AlongY Int deriving (Show)
type Paper = Set (Int, Int)


parsePaper :: String -> (Paper, [Fold])
parsePaper str = (parseSheet sheet, parseFolds folds)
  where
    sheet = head halved
    folds = last halved
    halved = splitOn "\n\n" str
    parseSheet = Set.fromList
        . map
            ( Bi.bimap readInt readInt
            . (\[x, y] -> (x, y))
            . splitOn ","
            )
        . lines
    parseFolds = map
            ( toFold
            . splitOn "="
            . last
            . words
            )
        . lines
    toFold [d, n]
        | d == "x" = AlongX (readInt n)
        | otherwise = AlongY (readInt n)

foldPaper :: Paper -> Fold -> Paper
foldPaper p f = Set.map (projectDot f) p
  where
    projectDot (AlongX n) (x, y) = (if x > n then 2 * n - x else x, y)
    projectDot (AlongY n) (x, y) = (x, if y > n then 2 * n - y else y)

foldAll :: Paper -> [Fold] -> Paper
foldAll = foldl foldPaper

paperToCharMatrix :: Paper -> Matrix Char
paperToCharMatrix = foldl (\m (y, x)
        -> M.setElem '#' (x + 1, y + 1)
        (M.extendTo '.' (x + 1) (y + 1) m))
    (M.fromLists [[]])

day13 :: String -> IO ()
day13 str = do
    let (p, f) = parsePaper str
    putStr "Number of dots after first fold: "
    print $ Set.size $ foldPaper p (head f)
    print "Hidden message:"
    print $ paperToCharMatrix $ foldAll p f

