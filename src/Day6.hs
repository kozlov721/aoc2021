module Day6 where

import Data.List.Split (splitOn)
import Data.Matrix     (Matrix)
import Day1            (readInt)

import qualified Data.Matrix as M

breedLanternFish :: M.Matrix Int -> Int -> Int
breedLanternFish fishVector days = sum
    $ power days `M.multStd` fishVector
  where
    power n = foldl M.multStd (M.identity 9)
        $ replicate n leslieMatrix
    leslieMatrix = M.fromLists
        [ [0, 1, 0, 0, 0, 0, 0, 0, 0]
        , [0, 0, 1, 0, 0, 0, 0, 0, 0]
        , [0, 0, 0, 1, 0, 0, 0, 0, 0]
        , [0, 0, 0, 0, 1, 0, 0, 0, 0]
        , [0, 0, 0, 0, 0, 1, 0, 0, 0]
        , [0, 0, 0, 0, 0, 0, 1, 0, 0]
        , [1, 0, 0, 0, 0, 0, 0, 1, 0]
        , [0, 0, 0, 0, 0, 0, 0, 0, 1]
        , [1, 0, 0, 0, 0, 0, 0, 0, 0]
        ]

countFishes :: [Int] -> M.Matrix Int
countFishes = foldl
    (\v f -> M.setElem (v M.! (f + 1, 1) + 1) (f + 1, 1) v)
    (M.zero 9 1)

day6 :: String -> IO ()
day6 str = do
    let fishes = countFishes $ map readInt $ splitOn "," str
    mapM_ (\x
        -> putStr ("After " ++ show x ++ " days: ")
        >> print (breedLanternFish fishes x))
        [80, 256, 365]

