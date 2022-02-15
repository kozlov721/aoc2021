{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Day17 where

import Day1              (readInt)
import Day2              (Position)
import Text.RawString.QQ
import Text.Regex.TDFA   (getAllTextMatches, (=~))


type Probe = (Int, Int, Int, Int)
type TargetArea = (Position, Position)


parseTargetArea :: String -> TargetArea
parseTargetArea str = ((left, top), (right, bottom))
  where
    [xMatch, yMatch] = getAllTextMatches (str =~ pattern) :: [String]
    pattern = [r|[xy]=-?[0-9]+\.\.-?[0-9]+|]
    numPattern = [r|-?[0-9]+|]
    [left, right] = map readInt
        (getAllTextMatches (xMatch =~ numPattern) :: [String])
    [bottom, top] = map readInt
        (getAllTextMatches (yMatch =~ numPattern) :: [String])

nextProbeState :: Probe -> Probe
nextProbeState (x, y, xv, yv) =
    ( x + xv
    , y + yv
    , if xv == 0 then 0 else xv - signum xv
    , yv - 1 )

findBestYVelocity :: TargetArea -> (Int, Int)
findBestYVelocity ((_, top), (_, bottom)) = (bestV, yMax)
  where
    bestV = abs top + abs (top - bottom) - 1
    -- This was figured out on paper
    yMax = let v = (2 * bestV - 1) `div` 2
           in (v + 1) * bestV - (v * (1 + v)) `div` 2

-- I think this could be almost completely solved
-- analytically on paper, but I didn't have the time
-- to figure it out.
findAllInitials :: TargetArea -> [(Int, Int)]
findAllInitials ((left, top), (right, bottom)) = (++)
    -- Trivial cases
    [(vx, vy) | vx <- [left..right], vy <- [bottom..top]]
    -- The rest
    [(vx, vy) | vx <- [minXVelocity..maxXVelocity]
              , vy <- [minYVelocity..maxYVelocity]
              , willHit (0, 0, vx, vy)]
  where
    maxYVelocity = abs top + abs (top - bottom) - 1
    minYVelocity = bottom
    maxXVelocity = left - 1
    minXVelocity = findMinXVelocity 0
    findMinXVelocity v
        | let x = ((v + 1) * v) `div` 2
              in x >= left && x <= right = v
        | otherwise = findMinXVelocity (v + 1)
    willHit probe@(x, y, _, _)
        | left <= x && x <= right && bottom <= y && y <= top = True
        | x > right || y < bottom = False
        | otherwise = willHit $ nextProbeState probe

day17 :: String -> IO ()
day17 str = do
    let target = parseTargetArea str
    putStr "Best initial y velocity and the height: "
    print $ findBestYVelocity target
    putStr "Number of all possible initial velocities: "
    print $ length $ findAllInitials target

