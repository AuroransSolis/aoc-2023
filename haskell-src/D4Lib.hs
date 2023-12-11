module D4Lib (part1, part2) where

import qualified Data.Char
import Debug.Trace
import qualified Misc

trimStart :: String -> String
trimStart str = drop 2 $ dropWhile (/= ':') str

readWins :: String -> [Int] -> ([Int], String)
readWins (h1 : h2 : tail) wins =
    if h1 == '|'
        then (wins, tail)
        else
            let
                wins' = number : wins
                tail' = drop 1 tail
             in
                readWins tail' wins'
  where
    h1Digit = if h1 == ' ' then 0 else Misc.charToDigit h1 * 10
    h2Digit = Misc.charToDigit h2
    number = h1Digit + h2Digit

part1 :: String -> Int
part1 str = part1Rec str
  where
    checkVals wins str val = case str of
        [] -> (val, str)
        all@(h1 : h2 : tail) ->
            if h1 == 'C'
                then (val, all)
                else checkVals wins (drop 1 tail) val'
          where
            h1Digit = if h1 == ' ' then 0 else Misc.charToDigit h1 * 10
            h2Digit = Misc.charToDigit h2
            number = h1Digit + h2Digit
            isWin = number `elem` wins
            val' = case (val, isWin) of
                (0, True) -> 1
                (val, True) -> val * 2
                (val, False) -> val
    getCardVal str = let (wins, tail') = readWins str [] in checkVals wins tail' 0
    part1Rec str = case str of
        [] -> 0
        ('C' : tail) -> let (val, tail') = getCardVal (trimStart tail) in val + part1Rec tail'
        (_ : tail) -> part1Rec tail

part2 :: String -> Int
part2 str = finalEval (part2Rec str []) []
  where
    countWins wins str acc = case str of
        [] -> (acc, str)
        all@(h1 : h2 : tail) ->
            if h1 == 'C'
                then (acc, all)
                else countWins wins (drop 1 tail) (acc + add)
          where
            h1Digit = if h1 == ' ' then 0 else Misc.charToDigit h1 * 10
            h2Digit = Misc.charToDigit h2
            number = h1Digit + h2Digit
            isWin = number `elem` wins
            add = if isWin then 1 else 0
    getCardVal str = let (wins, tail') = readWins str [] in countWins wins tail' 0
    part2Rec str acc = case str of
        [] -> acc
        ('C' : tail) -> let (val, tail') = getCardVal (trimStart tail) in part2Rec tail' (val : acc)
        (_ : tail) -> part2Rec tail acc
    finalEval numList countList = case numList of
        [] -> 0
        (numHead : numTail) -> newSum + finalEval numTail (newSum : countList)
          where
            newSum = 1 + sum (take numHead countList)
