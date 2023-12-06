module D2Lib (part1, part2, readID) where

import qualified Data.Char

maxR :: Int
maxR = 12

maxG :: Int
maxG = 13

maxB :: Int
maxB = 14

zeroDigit :: Int
zeroDigit = Data.Char.ord '0'

data Colour = Red | Green | Blue

part1 :: String -> Int
part1 str = part1Rec str 0

part1Rec :: String -> Int -> Int
part1Rec "" sum = sum
part1Rec (head : tail) sum
    | head == 'G' = part1Rec addRest $ sum + add
    | otherwise = error "???"
    where
        (idRest, id) = readID (drop 4 tail) 0
        (addRest, add) = getAdd idRest id (0, 0, 0)

readID :: String -> Int -> (String, Int)
readID (head : tail) sum
    | Data.Char.isDigit head = readID tail $ sum * 10 + headDigit
    | otherwise = (drop 1 tail, sum)
    where headDigit = Data.Char.ord head - zeroDigit

getAdd :: String -> Int -> (Int, Int, Int) -> (String, Int)
getAdd all@(head : tail) id maxes@(gameMaxR, gameMaxG, gameMaxB)
    | gameMaxR > maxR || gameMaxG > maxG || gameMaxB > maxB = (consumeLine all, 0)
    | head == '\n' = (tail, id)
    | head == ',' || head == ';' = getAdd (drop 1 tail) id maxes
    | Data.Char.isDigit head = getAdd colourRest id updatedMaxes
    | otherwise = getAdd tail id maxes
    where
        headDigit = Data.Char.ord head - zeroDigit
        (countRest, count) = getCount tail headDigit
        (colourRest, colour) = getColour countRest
        updatedMaxes = updateMaxes count colour maxes

consumeLine :: String -> String
consumeLine (head : rest)
    | head == '\n' = rest
    | otherwise = consumeLine rest

getCount :: String -> Int -> (String, Int)
getCount (head : tail) sum
    | Data.Char.isDigit head = getCount tail $ sum * 10 + headDigit
    | otherwise = (tail, sum)
    where headDigit = Data.Char.ord head - zeroDigit

getColour :: String -> (String, Colour)
getColour ('r' : tail) = (drop 2 tail, Red)
getColour ('g' : tail) = (drop 4 tail, Green)
getColour (_ : tail) = (drop 3 tail, Blue)

updateMaxes :: Int -> Colour -> (Int, Int, Int) -> (Int, Int, Int)
updateMaxes new colour (r, g, b) = case colour of
    Red -> (max new r, g, b)
    Green -> (r, max new g, b)
    Blue -> (r, g, max new b)

part2 :: String -> Int
part2 str = part2Rec str 0

part2Rec :: String -> Int -> Int
part2Rec "" sum = sum
part2Rec all@(head : tail) sum
    | head == 'G' = part2Rec powerRest $ sum + power
    | otherwise = error "???"
    where
        getStart (gsHead : gsTail)
            | gsHead == ':' = drop 1 gsTail
            | otherwise = getStart gsTail
        (powerRest, power) = getPower (getStart $ drop 5 tail) (0, 0, 0)

getPower :: String -> (Int, Int, Int) -> (String, Int)
getPower all@(head : tail) maxes@(gameMaxR, gameMaxG, gameMaxB)
    | head == '\n' = (tail, gameMaxR * gameMaxG * gameMaxB)
    | head == ',' || head == ';' = getPower (drop 1 tail) maxes
    | Data.Char.isDigit head = getPower colourRest updatedMaxes
    | otherwise = getPower tail maxes
    where
        headDigit = Data.Char.ord head - zeroDigit
        (countRest, count) = getCount tail headDigit
        (colourRest, colour) = getColour countRest
        updatedMaxes = updateMaxes count colour maxes 
