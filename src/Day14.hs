module Day14 where


import Data.List.Split (splitOn)
import Data.Map        (Map)

import           Data.List (sortOn)
import qualified Data.Map  as Map

type Rules = Map (Char, Char) Char

-- Begin naïve approach

parsePolymersNaive :: String -> (String, Rules)
parsePolymersNaive str = (start, parseRules rules)
  where
    [start, rules] = splitOn "\n\n" str
    parseRules = foldl insertRule Map.empty . lines
    insertRule m str = Map.insert key value m
      where
        key = (\[x, y] -> (x, y)) $ head $ splitOn " -> " str
        value = head $ last $ splitOn " -> " str

nextPolymerNaive :: String -> Rules -> String
nextPolymerNaive p r = head p : concat
    (zipWith insertPolymer p (tail p))
  where
    insertPolymer x y =
        if (x, y) `Map.member` r
        then [r Map.! (x, y), y]
        else [x, y]

generateNthPolymerNaive :: Int -> String -> Rules -> String
generateNthPolymerNaive 0 p _ = p
generateNthPolymerNaive n p r = generateNthPolymerNaive
    (n - 1)
    (nextPolymerNaive p r) r

-- End naïve approach

-- I believe it would also be possible to solve
-- with some kind of Leslie matrices.
type Polymer = (Map (Char, Char) Int, Map Char Int)

parsePolymers :: String -> (Polymer, Rules)
parsePolymers str = (polymer, rules)
  where
    (polStr, rules) = parsePolymersNaive str
    polymer = (pairs, chars)
    pairs = foldl (\m k -> Map.insertWith (+) k 1 m) Map.empty
        $ zip polStr (tail polStr)
    chars = foldl (\m k -> Map.insertWith (+) k 1 m) Map.empty polStr

nextPolymer :: Polymer -> Rules -> Polymer
nextPolymer (pairs, chars) rules = Map.foldrWithKey
    updatePolymer (Map.map (const 0) pairs, chars) pairs
  where
    updatePolymer (a, b) nPairs (pairs, chars) = (newPairs, newChars)
      where
        newPairs = Map.insertWith (+) (c, b) nPairs
            $ Map.insertWith (+) (a, c) nPairs pairs
        newChars = Map.insertWith (+) c nPairs chars
        c = rules Map.! (a, b)

generateNthPolymer :: Int -> Polymer -> Rules -> Polymer
generateNthPolymer 0 p _ = p
generateNthPolymer n p r = generateNthPolymer
    (n - 1)
    (nextPolymer p r) r

day14 :: String -> IO ()
day14 str = do
    let (p, r) = parsePolymers str
    let p10 = generateNthPolymer 10 p r
    let p40 = generateNthPolymer 30 p10 r
    putStr "Most common - least common (10 steps): "
    print $ difference p10
    putStr "Most common - least common (40 steps): "
    print $ difference p40
  where
    difference = (\x
        -> snd (last x) - snd (head x))
        .  sortOn snd . Map.toList . snd


