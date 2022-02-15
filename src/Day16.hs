{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day16 where


import Data.Char  (intToDigit)
import Data.Maybe (fromJust)
import Data.Word  (Word8)
import Numeric    (showIntAtBase)

import qualified Data.Bifunctor as Bi


-- It would be much easier to firstly generate
-- a list of ones and zeros from the input, but
-- I wanted to do it this 'lazy' way.

-- ((head, nbits taken), tail)
type BitString = ((Word8, Word8), String)

type LiteralPacket = (Int, Int, Int)
type OperatorPacket = (Int, Int, [Packet])
data Packet = Literal LiteralPacket
    | Operator OperatorPacket deriving (Show)


hexRead :: Char -> Word8
hexRead x = (read ['0', 'x', x] :: Word8) * 2 ^ 4

showAtBase :: (Integral a, Show a) => a -> a -> String
showAtBase b n = showIntAtBase b intToDigit n ""

showBinary :: (Integral a, Show a) => a -> String
showBinary = showAtBase 2

showHex :: (Integral a, Show a) => a -> String
showHex = showAtBase 16

nextBit :: BitString -> Maybe (Word8, BitString)
nextBit ((_, 4), []) = Nothing
nextBit ((_, 4), x:xs) = nextBit ((hexRead x, 0), xs)
nextBit ((byte, n), str) = Just (bit, ((rest, n + 1), str))
  where
    (bit, rest) = (byte `div` 2^7, byte * 2)

nextBitUnsafe :: BitString -> (Word8, BitString)
nextBitUnsafe = fromJust . nextBit

bitNull :: BitString -> Bool
bitNull ((_, 4), []) = True
bitNull _            = False

takeNBits :: Int -> BitString -> Maybe (Int, BitString)
takeNBits n b = go 0 (n - 1) b
  where
    go :: Int -> Int -> BitString -> Maybe (Int, BitString)
    go x n b
        | bitNull b = Nothing
        | x > n = Just (0, b)
        | otherwise = Bi.first (+(bit * 2^(n - x)))
            <$> go (x + 1) n nextB
      where
        (bit, nextB) = Bi.first (\x -> fromIntegral x :: Int)
            $ nextBitUnsafe b

takeNBitsUnsafe :: Int -> BitString -> (Int, BitString)
takeNBitsUnsafe n = fromJust . takeNBits n

bitsLength :: BitString -> Int
bitsLength ((byte, n), str) = 4 - (fromIntegral n :: Int)
    + length str * 4

stringToBitString :: String -> BitString
stringToBitString []     = ((0, 4), [])
stringToBitString (x:xs) = ((hexRead x, 0), xs)

parsePacket :: BitString -> (Packet, BitString)
parsePacket bs
    | typeID == 4 = (Literal literalPacket, finalBitStringL)
    | otherwise = (Operator operatorPacket, finalBitStringOp)
  where
    (version, bitsAfterVersion) = takeNBitsUnsafe 3 bs
    (typeID, bitsAfterHeader) = takeNBitsUnsafe 3 bitsAfterVersion
    literalPacket = (version, typeID, value)
    (value, finalBitStringL) = getValue bitsAfterHeader
    getValue bs = (foldl (\a n -> a * 2^4 + n) 0 nums, rest)
      where
        (nums, rest) = getPartsOfLiteral bs
    getPartsOfLiteral :: BitString -> ([Int], BitString)
    getPartsOfLiteral bs
        | bit == 0 = Bi.first (:[]) $ takeNBitsUnsafe 4 rest
        | otherwise =
            let (val, r) = takeNBitsUnsafe 4 rest
            in Bi.first (val:) (getPartsOfLiteral r)
      where
        (bit, rest) = fromJust $ nextBit bs

    operatorPacket = (version, typeID, opPackets)
    (opPackets, finalBitStringOp) = getOpPackets bitsAfterHeader
    getOpPackets :: BitString -> ([Packet], BitString)
    getOpPackets bs
        | bitI == 0 = (uncurry . flip) getOpPacketsType0
            $ takeNBitsUnsafe 15 rest
        | otherwise = (uncurry . flip) getOpPacketsType1
            $ takeNBitsUnsafe 11 rest
      where
        (bitI, rest) = fromJust $ nextBit bs

    getOpPacketsType0 :: BitString -> Int -> ([Packet], BitString)
    getOpPacketsType0 bs 0 = ([], bs)
    getOpPacketsType0 bs len = Bi.first (packet:)
        $ getOpPacketsType0 rest
        (len - (bitsLength bs - bitsLength rest))
      where
        (packet, rest) = parsePacket bs
    getOpPacketsType1 :: BitString -> Int -> ([Packet], BitString)
    getOpPacketsType1 bs 0 = ([], bs)
    getOpPacketsType1 bs n = Bi.first (packet:)
        $ getOpPacketsType1 rest (n - 1)
      where
        (packet, rest) = parsePacket bs

parseRoot :: BitString -> Packet
parseRoot = fst . parsePacket

sumVersions :: Packet -> Int
sumVersions (Literal (x, _, _))  = x
sumVersions (Operator (x, _, l)) = x + sum (map sumVersions l)

evalPacket :: Packet -> Int
evalPacket (Literal (_, _, x)) = x
evalPacket (Operator (_, t, [a, b]))
    | t == 5 = fromEnum $ evalPacket a > evalPacket b
    | t == 6 = fromEnum $ evalPacket a < evalPacket b
    | t == 7 = fromEnum $ evalPacket a == evalPacket b
evalPacket (Operator (_, t, l))
    | t == 0 = sum     $ map evalPacket l
    | t == 1 = product $ map evalPacket l
    | t == 2 = minimum $ map evalPacket l
    | t == 3 = maximum $ map evalPacket l

day16 :: String -> IO ()
day16 str = do
    let rootPacket = parseRoot $ stringToBitString $ init str
    putStr "Sum of all the version numbers: "
    print $ sumVersions rootPacket
    putStr "The value of the outermost packet: "
    print $ evalPacket rootPacket


