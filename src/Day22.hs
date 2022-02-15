{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day22 where


import Data.HashSet    (HashSet)
import Data.List.Split (splitOn)
import Day1            (readInt)
import Day19           (Position3D)
import Day2            (Position)

import qualified Data.HashSet as HashSet


data RebootStep = On (Position, Position, Position)
                | Off (Position, Position, Position)
                deriving Show

type RebootProcedure = [RebootStep]


parseReboot :: String -> RebootProcedure
parseReboot = map parseStep . lines
  where
    parseStep str = case concatMap words
        $ splitOn ","
        $ head
        $ lines str of
        ["on", x, y, z] -> On ( splitCoords x
                              , splitCoords y
                              , splitCoords z )
        ["off", x, y, z] -> Off ( splitCoords x
                                , splitCoords y
                                , splitCoords z )
    splitCoords = (\[x, y] -> (x, y))
        . map readInt
        . splitOn ".."
        . last
        . splitOn "="

rebootSystem :: RebootProcedure -> HashSet Position3D
rebootSystem [] = HashSet.empty
rebootSystem ((On ((minX, maxX), (minY, maxY), (minZ, maxZ))):xs) = rebootSystem xs `HashSet.union`
    HashSet.fromList [ (x, y, z)
                     | x <- [minX..maxX]
                     , y <- [minY..maxY]
                     , z <- [minZ..maxZ]]
rebootSystem ((Off ((minX, maxX), (minY, maxY), (minZ, maxZ))):xs) =
    rebootSystem xs `HashSet.difference`
        HashSet.fromList [ (x, y, z)
                         | x <- [minX..maxX]
                         , y <- [minY..maxY]
                         , z <- [minZ..maxZ]]

day22 :: String -> IO ()
day22 _ = print ""


