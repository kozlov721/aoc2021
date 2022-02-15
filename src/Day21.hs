module Day21 where


import Data.List.Split (splitOn)
import Day1            (readInt)


type Players = [Int]


parsePlayers :: String -> Players
parsePlayers = map ((+ (-1))
        . readInt
        . last
        . splitOn ": ")
    . lines



-- runDeterministic :: Players -> Int
-- runDeterministic = (\(x, y) -> x * y) . go 0
  -- where
    -- go :: Int -> Players -> (Int, Int)
    -- go n players = players (!!) (n `mod` length players)

day21 :: String -> IO ()
day21 _ = print ""


