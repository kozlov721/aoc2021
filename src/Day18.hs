{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day18 where


import qualified Data.Bifunctor as Bi
import           Data.Char      (isDigit)
import           Data.Maybe     (fromJust, isJust)


-- [(Number, Depth)]
type SfNumber = [(Int, Int)]


parseSfNumber :: String -> SfNumber
parseSfNumber = go 0
  where
    go :: Int -> String -> SfNumber
    go _ [] = []
    go d (x:xs)
        | x == '[' = go (d + 1) xs
        | x == ']' = go (d - 1) xs
        | x == ',' = go d xs
        | otherwise = (num, d) : go d rest
      where
        num = read (x : takeWhile isDigit xs) :: Int
        rest = dropWhile isDigit xs

parseAllSfNumbers :: String -> [SfNumber]
parseAllSfNumbers = map parseSfNumber . lines

addSfNumbers :: SfNumber -> SfNumber -> SfNumber
addSfNumbers a b = reduceSfNumber $ map (Bi.second (+1)) $ a ++ b

splitSfNumber :: SfNumber -> Maybe SfNumber
splitSfNumber [] = Nothing
splitSfNumber ((n, d):xs)
    | n >= 10 = Just $ (l, d + 1) : (r, d + 1) : xs
    | otherwise = ((n, d):) <$> splitSfNumber xs
  where
    l = n `div` 2
    r = ceiling $ (fromIntegral n :: Double) / 2

explodeSfNumber :: SfNumber -> Maybe SfNumber
-- The exploding number is the first one.
explodeSfNumber ((n1, d1):(n2, d2):(n3, d3):xs)
    | d1 > 4 && d2 == d1 = Just
        $ (0, d1 - 1) : (n2 + n3, d3) : xs
explodeSfNumber n = go n False
  where
    -- The exploding number is the last one.
    go [(n1, d1), (n2, d2), (n3, d3)] _
        | d1 <= 4 && d2 == d3 && d2 > 4 =
            Just [(n1 + n2, d1), (0, d2 - 1)]
    -- Otherwise
    go [] _ = Nothing
    go ((n1, d1):(n2, d2):xs) second
        | d2 > 4 && not second = ((n1 + n2, d1):) <$> go xs True
        | d1 > 4 && second = Just $ (0, d1 - 1) : (n1 + n2, d2) : xs
    go (x:xs) s = (x:) <$> go xs s

reduceSfNumber :: SfNumber -> SfNumber
reduceSfNumber x
    | isJust exploded = reduceSfNumber $ fromJust exploded
    | isJust split = reduceSfNumber $ fromJust split
    | otherwise = x
  where
    exploded = explodeSfNumber x
    split = splitSfNumber x

-- This is very unefficient
computeMagnitude :: SfNumber -> Int
computeMagnitude n = go [] n
  where
    go :: SfNumber -> SfNumber -> Int
    go prev [(n, _)] = n
    go prev ((n1, d1):(n2, d2):xs)
        | d1 == d2 = computeMagnitude
            $ prev ++ ((3 * n1 + 2 * n2, d1 - 1) : xs)
        | otherwise = go (prev ++ [(n1, d1)]) ((n2, d2) : xs)

findLargestMagnitude :: [SfNumber] -> Int
findLargestMagnitude = maximum
    . map (computeMagnitude . uncurry addSfNumbers)
    . (\l -> [(x,y) | x <- l, y <- l])

day18 :: String -> IO ()
day18 str = do
    let (n:ns) = parseAllSfNumbers str
    let result = foldl addSfNumbers n ns
    putStr "The magnitude of the sum: "
    print $ computeMagnitude result
    putStr "The largest magnitude: "
    print $ findLargestMagnitude (n:ns)

