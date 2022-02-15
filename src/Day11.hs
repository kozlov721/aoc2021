module Day11 where

import Data.Matrix (Matrix)
import Data.Maybe (mapMaybe)
import Day9 (readIntMatrix)

import qualified Data.Bifunctor as Bi
import qualified Data.Matrix    as Matrix

type Octopuses = Matrix Int


discharge :: Octopuses -> [(Int, Int)] -> Octopuses
discharge = foldl (flip $ Matrix.setElem 0)

neighbours8 :: Int -> Int -> [(Int, Int)]
neighbours8 i j = [(i + di, j + dj)
        | di <- [-1..1]
        , dj <- [-1..1]
        , di /= 0 || dj /= 0]

flash :: Octopuses -> (Int, Octopuses)
flash m
    | nFlashes == 0 = (0, m)
    | otherwise = Bi.bimap
        (+nFlashes)
        (`discharge` toDischarge)
        $ flash
        $ discharge newM toDischarge
  where
    nFlashes = length toDischarge
    toDischarge = [(i, j)
        | i <- [1..Matrix.nrows m]
        , j <- [1..Matrix.ncols m]
        , m Matrix.! (i, j) > 9]

    newM = Matrix.mapPos
        ( \(i, j) e
            -> e + length
            ( filter (>9)
            $ mapMaybe (\(u, v) -> Matrix.safeGet u v m)
            $ neighbours8 i j )
        ) m

nextStep :: Octopuses -> (Int, Octopuses)
nextStep = flash
    . Matrix.mapPos (\_ c -> c + 1)

simulateNSteps :: Octopuses -> Int -> (Octopuses, Int)
simulateNSteps m 0 = (m, 0)
simulateNSteps m n = Bi.second (+f) $ simulateNSteps nM (n - 1)
  where
    (f, nM) = nextStep m

findFirstBigFlash :: Octopuses -> Int
findFirstBigFlash m
    | m == Matrix.zero (Matrix.nrows m) (Matrix.ncols m) = 0
    | otherwise = (+) 1 $ findFirstBigFlash $ snd $ nextStep m

day11 :: String -> IO ()
day11 str = do
    let m = readIntMatrix str
    putStr "Flashes after 100 steps: "
    print $ snd $ simulateNSteps m 100
    putStr "First big flash in step: "
    print $ findFirstBigFlash m


