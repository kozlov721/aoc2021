module Day3 where


import Data.Char (digitToInt, intToDigit)


fromBits :: [Int] -> Int
fromBits = sum . zipWith (\x y -> (2 ^ x) * y) [0..] . reverse

getGammaEps :: [String] -> (Int, Int)
getGammaEps nums = (fromBits bits, fromBits $ map (1 -) bits)
  where
    bits = map thresh
        $ foldr gammaFold (replicate ((length . head) nums) 0) nums
    gammaFold = zipWith (\d c -> if d == '1' then c + 1 else c)
    thresh c = fromEnum (c > length nums `div` 2)

filterNumbers :: Int -> (Int -> Int -> Bool) -> [String] -> Int
filterNumbers _ _ [x] = fromBits $ map digitToInt x
filterNumbers n comp x = filterNumbers (n + 1) comp
    $ filter ((==mostCommon) . (!!n)) x
  where
    mostCommon = intToDigit
       $ fromEnum
       $ comp
       (sum (map (digitToInt . (!!n)) x))
       thresh
    thresh = if even len then half else half + 1
    len = length x
    half = length x `div` 2

getOxygenCO2 :: [String] -> (Int, Int)
getOxygenCO2 str = ( filterNumbers 0 (>=) str
                   , filterNumbers 0 (<) str)

day3PartOne :: String -> IO ()
day3PartOne = (\(gamma, eps)
    -> putStr "Gamma: "
    >> print gamma
    >> putStr "Epsilon: "
    >> print eps
    >> putStr "Product: "
    >> print (gamma * eps))
    . getGammaEps
    . words

day3PartTwo :: String -> IO ()
day3PartTwo = (\(oxygen, co2)
    -> putStr "Oxygen: "
    >> print oxygen
    >> putStr "C02: "
    >> print co2
    >> putStr "Product: "
    >> print (oxygen * co2))
    . getOxygenCO2
    . words

day3 :: String -> IO ()
day3 str = day3PartOne str
    >> print ""
    >> day3PartTwo str

