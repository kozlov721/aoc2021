-- TODO: complete

module Day19 where


import Data.HashSet    (HashSet)
import Data.List       (permutations)
import Data.List.Split (splitOn)
import Data.Maybe      (fromJust, isNothing)
import Day1            (readInt)

import qualified Data.HashSet as HashSet


type Position3D = (Int, Int, Int)
type Scanner = HashSet Position3D


parseScanners :: String -> [Scanner]
parseScanners = map
        ( HashSet.fromList
        . map
            ( (\[x, y, z] -> (x, y, z))
            . map readInt
            . splitOn ","
            )
        . tail
        . lines
        )
    . splitOn "\n\n"

mergeScanners :: Scanner -> Scanner -> Maybe Scanner
mergeScanners a b = HashSet.map (back anchor) <$> go shiftedA bs
  where
    bList = HashSet.toList b
    anchor = head $ HashSet.toList a
    shiftedA = moveToOrigin anchor a
    moveToOrigin o s = HashSet.map (shift o) s
    shift (x1, y1, z1) (x2, y2, z2) = (x2 - x1, y2 - y1, z2 - z1)
    back (x1, y1, z1) (x2, y2, z2) = (x2 + x1, y2 + y1, z2 + z1)
    bs = concatMap
        (\b -> [ HashSet.map f b
        | f <- [ \x -> (fs $ f x, gs $ g x, hs $ h x)
               | [f, g, h] <- selectors
               , [fs, gs, hs] <- turners
               ]
        ]) bsShifted
    bsShifted = [moveToOrigin o b | o <- bList]
    selectors = permutations
        [\(x, _, _) -> x, \(_, y, _) -> y, \(_, _, z) -> z]
    turners = [ [id, id, id]
              , [negate, id, id]
              , [id, negate, id]
              , [id, id, negate]
              , [negate, negate, id]
              , [negate, id, negate]
              , [id, negate, negate]
              , [negate, negate, negate]
              ]
    go :: Scanner -> [Scanner] -> Maybe Scanner
    go a [] = Nothing
    go a (b:bs) = if HashSet.size (HashSet.intersection a b) >= 12
                  then Just $ HashSet.union a b
                  else go a bs

findAllBeacons :: [Scanner] -> Int -> [Scanner]
findAllBeacons x 0 = x
findAllBeacons x n
    | length res == length x = findAllBeacons (tail x ++ [head x]) n
    | length res == 1 = res
    | otherwise = findAllBeacons res (n - 1)
 where
    res = go x
    go :: [Scanner] -> [Scanner]
    go [] = []
    go (x:xs) = acc : go rest
      where
        (acc, rest) = foldl ( \(s, r) x ->
            let m = mergeScanners s x
            in if isNothing m
               then (s, x:r)
               else (fromJust m, r))
            (x, [])
            xs

day19 :: String -> IO ()
day19 str = do
    let scanners = parseScanners str
    putStr "The number of beacons: "
    -- print $ length $ findAllBeacons scanners

