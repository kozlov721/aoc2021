module Day7 where


import Data.List.Split (splitOn)
import Day1            (readInt)


parseCrabs :: String -> [Int]
parseCrabs = map readInt . splitOn ","

-- Backtracking would be nicer, but this bruteforce
-- solution was just too much more simple to write.
findLeastFuel :: [Int] -> Int
findLeastFuel nums = minimum
    $ map center [minimum nums..maximum nums]
  where
    center c = sum . map (abs . (c-)) $ nums

findLeastFuelIncrementing :: [Int] -> Int
findLeastFuelIncrementing nums = minimum
    $ map center [minimum nums..maximum nums]
  where
    center c = sum . map (arSum . abs . (c-)) $ nums
    arSum n = ((1 + n) * n) `div` 2

day7 :: String -> IO ()
day7 str = do
    let nums = parseCrabs str
    putStr "The needed fuel for part one: "
    print $ findLeastFuel nums
    putStr "The needed fuel for part two: "
    print $ findLeastFuelIncrementing nums

