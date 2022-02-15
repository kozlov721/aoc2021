module Day9 where

import Data.Char   (digitToInt)
import Data.List   (sort)
import Data.Matrix (Matrix)
import Data.Maybe  (mapMaybe)


import qualified Data.Bifunctor as Bi
import qualified Data.Matrix    as M

type Heights = Matrix Int

readIntMatrix :: String -> Matrix Int
readIntMatrix = M.fromLists
    . map (map digitToInt)
    . lines

neighbours4 :: Int -> Int -> [(Int, Int)]
neighbours4 i j = [(i, j + 1), (i, j - 1), (i + 1, j), (i - 1, j)]

getLowPoints :: Heights -> [(Int, Int)]
getLowPoints h = filter isLowPoint
      [(i, j) | i <- [1..M.nrows h], j <- [1..M.ncols h]]
  where
    isLowPoint (i, j) = all (>M.getElem i j h)
        $ mapMaybe (\(x, y) -> M.safeGet x y h)
        $ neighbours4 i j

sumLowPoints :: Heights -> Int
sumLowPoints h = sum
    $ map ((+1) . (h M.!))
    $ getLowPoints h

largestBasins :: Heights -> Int
largestBasins h = product
    $ take 3
    $ reverse
    $ sort
    $ map (`getBasin` h)
    $ getLowPoints h

getBasin :: (Int, Int) -> Heights -> Int
getBasin c h = snd $ go c h
  where
    go :: (Int, Int) -> Heights -> (Heights, Int)
    go (0, _) h = (h, 0)
    go (_, 0) h = (h, 0)
    go (i, j) h
        | i > M.nrows h = (h, 0)
        | j > M.ncols h = (h, 0)
        | h M.! (i, j) == 9 = (h, 0)
        | otherwise = foldl
            (\(h, s) (u, v)
                -> Bi.second (+s)
                $ go (u, v)
                $ M.setElem 9 (i, j) h)
            (h, 1)
            $ neighbours4 i j

day9 :: String -> IO ()
day9 str = do
    let heights = readIntMatrix str
    print $ sumLowPoints heights
    print $ largestBasins heights

