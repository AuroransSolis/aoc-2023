module D1Lib (part1, part2) where

import qualified Data.Char
import qualified Data.Maybe
import qualified Misc

part1 :: String -> Int
part1 str = part1Rec str 0 10 10

secondDigit :: Int -> Int -> Int
secondDigit fst sec
    | sec < 10 = sec
    | otherwise = fst

finalAdd :: Int -> Int -> Int
finalAdd fst sec = fst * 10 + sec

part1Rec :: String -> Int -> Int -> Int -> Int
part1Rec [] sum _ _ = sum
part1Rec (head : tail) sum fst sec
    | head == '\n' = part1Rec tail (sum + new) 10 10
    | Data.Char.isDigit head =
        if fst < 10
            then part1Rec tail sum fst headInt
            else part1Rec tail sum headInt sec
    | otherwise = part1Rec tail sum fst sec
  where
    sec' = secondDigit fst sec
    new = finalAdd fst sec'
    headInt = Data.Char.ord head - Misc.zeroDigit

part2 :: String -> Int
part2 str = part2Rec str 0 10 10

part2Rec :: String -> Int -> Int -> Int -> Int
part2Rec [] sum _ _ = sum
part2Rec all@(head : tail) sum fst sec
    | head == '\n' = part2Rec tail (sum + new) 10 10
    | Data.Char.isDigit head =
        if fst < 10
            then part2Rec tail sum fst headInt
            else part2Rec tail sum headInt sec
    | otherwise = case tryParseInt all of
        (parseTail, Just parsedDigit) ->
            if fst < 10
                then part2Rec parseTail sum fst parsedDigit
                else part2Rec parseTail sum parsedDigit sec
        _ -> part2Rec tail sum fst sec
  where
    sec' = secondDigit fst sec
    new = finalAdd fst sec'
    headInt = Data.Char.ord head - Misc.zeroDigit

tryParseInt :: String -> (String, Maybe Int)
tryParseInt ('o' : rest) = tryParseInt1 rest P1GetN
tryParseInt ('t' : rest) = tryParseInt23 rest P23GetWH
tryParseInt ('f' : rest) = tryParseInt45 rest P45GetOI
tryParseInt ('s' : rest) = tryParseInt67 rest P67GetIE1
tryParseInt ('e' : rest) = tryParseInt8 rest P8GetI
tryParseInt ('n' : rest) = tryParseInt9 rest P9GetI
tryParseInt other = (other, Nothing)

data Parse1Step = P1GetN | P1GetE
tryParseInt1 :: String -> Parse1Step -> (String, Maybe Int)
tryParseInt1 ('n' : rest) P1GetN = tryParseInt1 rest P1GetE
tryParseInt1 all@('e' : _) P1GetE = (all, Just 1)
tryParseInt1 other _ = (other, Nothing)

data Parse23Step = P23GetWH | P23GetO | P23GetR | P23GetE1 | P23GetE2
tryParseInt23 :: String -> Parse23Step -> (String, Maybe Int)
tryParseInt23 ('w' : rest) P23GetWH = tryParseInt23 rest P23GetO
tryParseInt23 all@('o' : _) P23GetO = (all, Just 2)
tryParseInt23 ('h' : rest) P23GetWH = tryParseInt23 rest P23GetR
tryParseInt23 ('r' : rest) P23GetR = tryParseInt23 rest P23GetE1
tryParseInt23 ('e' : rest) P23GetE1 = tryParseInt23 rest P23GetE2
tryParseInt23 all@('e' : _) P23GetE2 = (all, Just 3)
tryParseInt23 other _ = (other, Nothing)

data Parse45Step = P45GetOI | P45GetU | P45GetR | P45GetV | P45GetE
tryParseInt45 :: String -> Parse45Step -> (String, Maybe Int)
tryParseInt45 ('o' : rest) P45GetOI = tryParseInt45 rest P45GetU
tryParseInt45 ('u' : rest) P45GetU = tryParseInt45 rest P45GetR
tryParseInt45 all@('r' : _) P45GetR = (all, Just 4)
tryParseInt45 ('i' : rest) P45GetOI = tryParseInt45 rest P45GetV
tryParseInt45 ('v' : rest) P45GetV = tryParseInt45 rest P45GetE
tryParseInt45 all@('e' : _) P45GetE = (all, Just 5)
tryParseInt45 other _ = (other, Nothing)

data Parse67Step = P67GetIE1 | P67GetX | P67GetV | P67GetE2 | P67GetN
tryParseInt67 :: String -> Parse67Step -> (String, Maybe Int)
tryParseInt67 ('i' : rest) P67GetIE1 = tryParseInt67 rest P67GetX
tryParseInt67 all@('x' : _) P67GetX = (all, Just 6)
tryParseInt67 ('e' : rest) P67GetIE1 = tryParseInt67 rest P67GetV
tryParseInt67 ('v' : rest) P67GetV = tryParseInt67 rest P67GetE2
tryParseInt67 ('e' : rest) P67GetE2 = tryParseInt67 rest P67GetN
tryParseInt67 all@('n' : _) P67GetN = (all, Just 7)
tryParseInt67 other _ = (other, Nothing)

data Parse8Step = P8GetI | P8GetG | P8GetH | P8GetT
tryParseInt8 :: String -> Parse8Step -> (String, Maybe Int)
tryParseInt8 ('i' : rest) P8GetI = tryParseInt8 rest P8GetG
tryParseInt8 ('g' : rest) P8GetG = tryParseInt8 rest P8GetH
tryParseInt8 ('h' : rest) P8GetH = tryParseInt8 rest P8GetT
tryParseInt8 all@('t' : _) P8GetT = (all, Just 8)
tryParseInt8 other _ = (other, Nothing)

data Parse9Step = P9GetI | P9GetN | P9GetE
tryParseInt9 :: String -> Parse9Step -> (String, Maybe Int)
tryParseInt9 ('i' : rest) P9GetI = tryParseInt9 rest P9GetN
tryParseInt9 ('n' : rest) P9GetN = tryParseInt9 rest P9GetE
tryParseInt9 all@('e' : _) P9GetE = (all, Just 9)
tryParseInt9 other _ = (other, Nothing)
