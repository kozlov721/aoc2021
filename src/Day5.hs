{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day5 where

import Data.List       (sort)
import Data.List.Split (splitOn, splitWhen)
import Day1            (readInt)

type Point = (Int, Int)
type Line = (Point, Point)


parseLines :: String -> [Line]
parseLines = map (
          toTuple
        . map toPoint
        . splitOn "->"
    ) . lines
  where
    toPoint = toTuple
        . map readInt
        . splitOn ","
    toTuple [x, y] = (x, y)

generatePoints :: Line -> [Point]
generatePoints (begin@(x1, y1), end@(x2, y2))
    | x1 == x2 && y1 == y2 = [(x2, y2)]
    | otherwise = begin : generatePoints (newPoint, end)
  where
    newPoint = (x1 + dx, y1 + dy)
    dx = signum (x2 - x1)
    dy = signum (y2 - y1)

countSorted :: Eq a => [a] -> [(a, Int)]
countSorted [] = []
countSorted arr@(x:_) = (x, length (takeWhile (==x) arr))
    : (countSorted . dropWhile (==x)) arr

countIntersectsAll :: [Line] -> Int
countIntersectsAll = length
    . filter ((/= 1) . snd)
    . countSorted
    . sort
    . concatMap generatePoints

countIntersectsNonDiag :: [Line] -> Int
countIntersectsNonDiag = countIntersectsAll
    . filter (\((x1, y1), (x2, y2))
        -> x1 == x2 || y1 == y2)

day5 :: String -> IO ()
day5 input = do
    let lines = parseLines input
    putStr "All overlaps without diagonals: "
    print $ countIntersectsNonDiag lines
    putStr "All overlaps: "
    print $ countIntersectsAll lines

