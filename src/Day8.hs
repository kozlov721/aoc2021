module Day8 where

import Data.List.Split (splitOn)
import Data.Map        (Map)
import Data.Maybe      (isJust, mapMaybe, fromJust)


import qualified Data.Map as Map

type Segment = Char
type Digit = [Segment]
type Observation = ([Digit], [Digit])
type WiresTable = Map Char [Char]


correctSegments :: Map Digit Char
correctSegments = Map.fromList [
      ("abcefg" , '0')
    , ("cf"     , '1')
    , ("acdeg"  , '2')
    , ("acdfg"  , '3')
    , ("bcdf"   , '4')
    , ("abdfg"  , '5')
    , ("abdefg" , '6')
    , ("acf"    , '7')
    , ("abcdefg", '8')
    , ("abcdfg" , '9')
    ]

parseDigits :: String -> [Observation]
parseDigits = map (
          (\[x, y] -> (x, y))
        . (map words . splitOn " | ")
    ) . lines

countEasyDigits :: [Digit] -> Int
countEasyDigits = length
    . filter (/= negate 1)
    . map spotEasyOnes
  where
    spotEasyOnes x
        | length x == 2 = 1
        | length x == 4 = 4
        | length x == 3 = 7
        | length x == 7 = 8
        | otherwise = -1

deduceWiring' :: WiresTable -> [Digit] -> Maybe WiresTable
deduceWiring' table digits
    | not $ correct table digits = Nothing
    | complete table = Just table
    | otherwise = head
        $ filter isJust
        $ map (`deduceWiring'` digits)
        $ generateNewTables table

generateNewTables :: WiresTable -> [WiresTable]
generateNewTables _ = error "notImplemented"

correct :: WiresTable -> [Digit] -> Bool
correct table digits = all (`Map.member` correctSegments)
    $ mapMaybe translate digits
  where
    translate [] = Just []
    translate (d:ds)
        | length r == 1 = (head r:) <$> translate ds
        | otherwise = Nothing
      where
        r = table Map.! d

complete :: WiresTable -> Bool
complete = all (==1) . Map.map length

deduceWiring :: [Digit] -> WiresTable
deduceWiring = fromJust
    . deduceWiring'
    ( Map.fromList [(key, ['a'..'g']) | key <- ['a'..'g']] )

-- deduceDigits :: [Digit] -> Int
-- deduceDigits = readInt
    -- . map (correctSegments !)
    -- . reverse
    -- . take 4
    -- . reverse
    -- . deduceWiring

day8PartOne :: [Observation] -> Int
day8PartOne = sum . map (countEasyDigits . snd)

day8 :: String -> IO ()
day8 = print . day8PartOne . parseDigits

