{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day4 where

import Data.List       (transpose)
import Data.List.Split (splitOn, splitWhen)
import Day1            (readInt)

type Board = [[Int]]
type Call = Int
type Calls = [Call]
type Score = Int

parseBingo :: String -> (Calls, [Board])
parseBingo str = (parseCalls str, parseBoards str)

parseCalls :: String -> Calls
parseCalls = map readInt
    . splitOn ","
    . head
    . lines

parseBoards :: String -> [Board]
parseBoards = map parseBoard
    . tail
    . splitWhen null
    . tail
    . lines

parseBoard :: [String] -> Board
parseBoard = map $ map readInt . words

crossNumber :: Int -> [Board] -> [Board]
crossNumber n = map
    $ map
    $ map
    $ \x -> if x == n then -1 else x

isFinished :: Board -> Bool
isFinished board = checkHorizontal board || checkVertical board
  where
    checkHorizontal = any ((==True) . all (== negate 1))
    checkVertical = checkHorizontal . transpose

computeScore :: Call -> Board -> Score
computeScore c = (*c) . sum . filter (/= negate 1) . concat

play :: [Board] -> Calls -> Score
play b = go b 0
  where
    go :: [Board] -> Call -> Calls -> Score
    go boards lastCall (c:cs)
        | (not . null) finishedBoards = computeScore lastCall
            $ head finishedBoards
        | otherwise = go (crossNumber c boards) c cs
      where
        finishedBoards = filter isFinished boards

playLast :: [Board] -> Calls -> Score
playLast b = go b 0
  where
    go :: [Board] -> Call -> Calls -> Score
    go boards lastCall (c:cs)
        | null boardsToContinue = computeScore lastCall $ head boards
        | otherwise = go (crossNumber c boardsToContinue) c cs
      where
        boardsToContinue = filter (not . isFinished) boards

day4PartOne :: String -> IO ()
day4PartOne = print . (uncurry . flip) play . parseBingo

day4PartTwo :: String -> IO ()
day4PartTwo = print . (uncurry . flip) playLast . parseBingo

day4 :: String -> IO ()
day4 str = day4PartOne str
    >> print ""
    >> day4PartTwo str

