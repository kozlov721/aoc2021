module Day1 where


readInt :: String -> Int
readInt x = read x :: Int

countAscending :: [Int] -> Int
countAscending nums = sum
    $ zipWith (\x y -> fromEnum (x < y))
    nums (tail nums)

countTripleSum :: [Int] -> Int
countTripleSum nums = countAscending
    $ map (\(x, y, z) -> x + y + z)
    $ zip3 nums (tail nums) ((tail . tail) nums)

day1 :: String -> IO ()
day1 path = readFile path
    >>= print
    . (\x -> (countAscending x, countTripleSum x))
    . map readInt
    . words

