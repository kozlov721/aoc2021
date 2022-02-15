module Day12 where


import Data.Char       (isUpper)
import Data.List.Split (splitOn)
import Data.Map        (Map)

import qualified Data.Map as Map


data Cave = Big String | Small String deriving (Show, Eq, Ord)
type Graph = Map Cave [Cave]
type CavePath = [Cave]


parseCaves :: String -> Graph
parseCaves = foldl addToGraph Map.empty . lines
  where
    addToGraph g str = Map.insertWith (++) value [key]
        $ Map.insertWith (++) key [value] g
      where
        key = parseCave $ head $ splitOn "-" str
        value = parseCave $ last $ splitOn "-" str
    parseCave :: String -> Cave
    parseCave str
        | isBig str      = Big str
        | otherwise      = Small str
    isBig = all isUpper

findPathsNoRepeat :: Graph -> [CavePath]
findPathsNoRepeat g = filter (\x -> last x == Small "end")
    $ go g [] (Small "start")
  where
    go :: Graph -> [Cave] -> Cave -> [CavePath]
    go _ _ c@(Small "end") = [[c]]
    go g visitedSmall c@(Small str)
        | not $ Map.member c g = [[]]
        | c `elem` visitedSmall = [[]]
        | otherwise = concatMap
            (map (c:) . go g (c:visitedSmall))
            (g Map.! c)
    go g h c
        | not $ Map.member c g = [[]]
        | otherwise = concatMap
            (map (c:) . go g h)
            (g Map.! c)

findPathsOneRepeat :: Graph -> [CavePath]
findPathsOneRepeat g = filter (\x -> last x == Small "end")
    $ go g False [] (Small "start")
  where
    go :: Graph -> Bool -> [Cave] -> Cave -> [CavePath]
    go _ _ _ c@(Small "end") = [[c]]
    go g b visitedSmall c@(Small str)
        | not $ Map.member c g = [[]]
        | str == "start" && (not . null) visitedSmall = [[]]
        | isElem && b = [[]]
        | otherwise = concatMap
            (map (c:) . go g (if b then b else isElem)
            (c:visitedSmall))
            (g Map.! c)
          where
            isElem = c `elem` visitedSmall
    go g b h c
        | not $ Map.member c g = [[]]
        | otherwise = concatMap
            (map (c:) . go g b h)
            (g Map.! c)

day12 :: String -> IO ()
day12 str = do
    let g = parseCaves str
    putStr "Number of paths with no repeating small caves: "
    print $ length $ findPathsNoRepeat g
    putStr "When one small cave can be visited twice: "
    print $ length $ findPathsOneRepeat g

