module Day20 where


import Data.HashSet    (HashSet)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe      (catMaybes, mapMaybe)
import Data.Vector     (Vector)
import Day2            (Position)

import qualified Data.HashSet as HashSet
import qualified Data.Vector  as Vector


type Image = HashSet Position
type ImageAlgorithm = Vector Int


parseImage :: String -> (Image, ImageAlgorithm)
parseImage str = (img, alg)
  where
    alg = Vector.fromList $ map (fromEnum . (=='#')) algStr
    img = HashSet.fromList
        $ catMaybes
        $ zipWith getPixel [0..]
        $ concat
        $ lines imgStr
    getPixel n '#' = Just (n `mod` width, n `div` width)
    getPixel _ _   = Nothing
    width = length $ head $ lines imgStr
    [algStr, imgStr] = splitOn "\n\n" str

findMinMaxImage :: Image -> (Int, Int, Int, Int)
findMinMaxImage = foldl
        ( \(minX, maxX, minY, maxY) (x, y)
        -> (min minX x, max maxX x, min minY y, max maxY y)
        )
        ( maxBound :: Int
        , minBound :: Int
        , maxBound :: Int
        , minBound :: Int
        )
        . HashSet.toList

nextImage :: Image -> ImageAlgorithm -> Bool -> Image
nextImage img alg rest = HashSet.fromList
    $ mapMaybe (\x -> getNewPixel x $ getSquare x)
    [ (x, y)
    | x <- [minX - 1..maxX + 1]
    , y <- [minY - 1..maxY + 1]]
  where
    getSquare (x, y) =
        [ fromEnum $ isLit u v
        | v <- [y - 1..y + 1]
        , u <- [x - 1..x + 1]
        ]
    isLit x y
        | Vector.head alg == 1
            && Vector.last alg == 0
            && (  x < minX
               || x > maxX
               || y < minY
               || y > maxY
               ) = rest
        | otherwise = HashSet.member (x, y) img
    getNewPixel x l
        | alg Vector.! n == 1 = Just x
        | otherwise = Nothing
      where
        n = sum
            $ zipWith (\x y -> (2 ^ x) * y) [0..]
            $ reverse l
    (minX, maxX, minY, maxY) = findMinMaxImage img

generateNthImage :: Int -> Image -> ImageAlgorithm -> Image
generateNthImage = go 0
  where
    go :: Int -> Int -> Image -> ImageAlgorithm -> Image
    go x n img alg
        | x == n = img
        | otherwise = go
            (x + 1) n (nextImage img alg $ odd x) alg

printImage :: Image -> IO ()
printImage img = mapM_ print
    $ chunksOf (maxX - minX + 1)
    $ [ ((!!) " #" . fromEnum) (HashSet.member (i, j) img)
      | j <- [minX..maxX], i <- [minY..maxY]]
  where
    (minX, maxX, minY, maxY) = findMinMaxImage img

day20 :: String -> IO ()
day20 str = do
    let (img, alg) = parseImage str
    putStr "Pixels after two iterations: "
    print $ HashSet.size $ generateNthImage 2 img alg
    putStr "Pixels after fifty iterations: "
    print $ HashSet.size $ generateNthImage 50 img alg

