module Day10 where

import Data.Either               (fromLeft, fromRight, isLeft, isRight)
import Data.List                 (sort)
import Data.Map                  (Map)
import Numeric.Statistics.Median (median)

import qualified Data.Map as Map

points :: Map Char Int
points = Map.fromList [
      (')' , 3    )
    , (']' , 57   )
    , ('}' , 1197 )
    , ('>' , 25137)
    ]

pointsComplete :: Map Char Double
pointsComplete = Map.fromList [
      ('(' , 1)
    , ('[' , 2)
    , ('{' , 3)
    , ('<' , 4)
    ]

bracketPairs :: Map Char Char
bracketPairs = Map.fromList [
      (')' , '(')
    , (']' , '[')
    , ('}' , '{')
    , ('>' , '<')
    ]

findError :: String -> Either Char [Char]
findError = (`go` [])
  where
    go :: String -> [Char] -> Either Char [Char]
    go [] stack = Right stack
    go (c:cs) [] = go cs [c]
    go (c:cs) (top:stack)
        | opening c = go cs (c:top:stack)
        | otherwise = if bracketPairs Map.! c == top
                      then go cs stack
                      else Left c
      where
        opening c
            | c == '(' = True
            | c == '[' = True
            | c == '{' = True
            | c == '<' = True
            | otherwise = False

computeErrorScore :: String -> Int
computeErrorScore = sum
    . map ((points Map.!) . ('(' `fromLeft`))
    . filter isLeft
    . map findError
    . lines

computeCompletionScore :: String -> Int
computeCompletionScore = round
    . median
    . sort
    . filter (/= 0)
    . map
        ( foldl (\x y -> 5 * x + y) 0
        . map (pointsComplete Map.!)
        . fromRight ""
        . findError )
    . lines

day10 :: String -> IO ()
day10 str = do
    let errorScore = computeErrorScore str
    putStr "Error Score: "
    print errorScore
    let completionScore = computeCompletionScore str
    putStr "Completion Score: "
    print completionScore


