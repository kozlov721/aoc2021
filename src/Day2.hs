{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day2 where


data Direction = Down Int | Up Int | Forward Int deriving Show
type Position = (Int, Int)
type CorrectPosition = (Int, Int, Int)


parseDirections :: String -> [Direction]
parseDirections = map (selectDir . words) . lines
  where
    selectDir :: [String] -> Direction
    selectDir [dir, am]
        | dir == "forward" = Forward amount
        | dir == "down" = Down amount
        | dir == "up" = Up amount
      where amount = read am :: Int

move :: Position -> Direction -> Position
move (x, y) (Down n)    = (x, y + n)
move (x, y) (Up n)      = (x, y - n)
move (x, y) (Forward n) = (x + n, y)

moveCorrect :: CorrectPosition -> Direction -> CorrectPosition
moveCorrect (x, y, a) (Down n)    = (x, y, a + n)
moveCorrect (x, y, a) (Up n)      = (x, y, a - n)
moveCorrect (x, y, a) (Forward n) = (x + n, y + a * n, a)

getFinalPosition :: String -> Position
getFinalPosition str = foldl move (0, 0) $ parseDirections str

getFinalPositionCorrect :: String -> CorrectPosition
getFinalPositionCorrect str = foldl
    moveCorrect (0, 0, 0)
    $ parseDirections str

day2 :: String -> IO ()
day2 str = do
    let endPosition@(x, y) = getFinalPosition str
    putStr "End position is "
    print endPosition
    putStr "The product is "
    print (x * y)

    let endPositionCorrect@(x, y, a) = getFinalPositionCorrect str
    putStr "Correct end position is "
    print endPositionCorrect
    putStr "Correct product is "
    print (x * y)

