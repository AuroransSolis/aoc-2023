module D3Lib (part1, part2) where

import qualified Data.Char
import Debug.Trace
import qualified Misc

readMapGrid :: String -> (Char -> a) -> [[a]]
readMapGrid grid map =
    cons
        ( case breakMapGrid grid map of
            (line, rest) ->
                ( line
                , case rest of
                    [] -> []
                    _ : rest' -> readMapGrid rest' map
                )
        )
  where
    cons ~(a, b) = a : b

breakMapGrid :: [Char] -> (Char -> a) -> ([a], String)
breakMapGrid [] _ = ([], [])
breakMapGrid all@(head : tail) map
    | head == '\n' = ([], all)
    | otherwise = let (head', tail') = breakMapGrid tail map in (headVal : head', tail')
  where
    headVal = map head

type BorderRowFn a = [a] -> [a] -> Int
type MiddleRowFn a = [a] -> [a] -> [a] -> Int

data ProcessGrid a = ProcessGrid
    { r0 :: [a]
    , r1 :: [a]
    , r2 :: [a]
    , rest :: [[a]]
    }

makeProcess :: [[a]] -> ProcessGrid a
makeProcess (r1 : r2 : rest) = ProcessGrid [] r1 r2 rest

walkGrid :: ProcessGrid a -> BorderRowFn a -> MiddleRowFn a -> Int
walkGrid (ProcessGrid [] r1 r2 (newR2 : rest)) b m = b r1 r2 + walkGrid (ProcessGrid r1 r2 newR2 rest) b m
-- Flipping the last two rows lets us reuse the same function as for the first two rows
walkGrid (ProcessGrid r0 r1 [] rest) b m = b r1 r0
walkGrid (ProcessGrid r0 r1 r2 (newR2 : rest)) b m = m r0 r1 r2 + walkGrid (ProcessGrid r1 r2 newR2 rest) b m

part1 :: String -> Int
part1 str = walkGrid (makeProcess $ readMapGrid str readCV1) borderRow1 middleRow1

data CellVal1 = CV1Empty | Symbol | CV1Digit Int deriving (Eq, Show)

readCV1 :: Char -> CellVal1
readCV1 char
    | Data.Char.isDigit char = CV1Digit $ Data.Char.ord char - Misc.zeroDigit
    | char == '.' = CV1Empty
    | otherwise = Symbol

toCV1Digit :: CellVal1 -> Int
toCV1Digit (CV1Digit val) = val
toCV1Digit _ = 0

isCV1Digit :: CellVal1 -> Bool
isCV1Digit (CV1Digit _) = True
isCV1Digit _ = False

tmp :: CellVal1
tmp = Symbol

borderRow1 :: BorderRowFn CellVal1
borderRow1 (aHead : aTail) (bHead : bTail) = borderRowRec aTail bTail startHS startVal
  where
    startHS = aHead == Symbol || bHead == Symbol
    startVal = toCV1Digit aHead
    borderRowRec a b hasSymbol prev = case (a, b) of
        ([], []) -> if hasSymbol then prev else 0
        (aHead : aTail, bHead : bTail) ->
            let
                (next, add, hasSymbol') = case aHead of
                    CV1Digit val -> (prev * 10 + val, 0, hasSymbol || vertSymbol)
                    Symbol -> (0, prev, True)
                    CV1Empty -> (0, emptyFinal, vertSymbol)
             in
                add + borderRowRec aTail bTail hasSymbol' next
          where
            vertSymbol = bHead == Symbol
            emptyFinal = if hasSymbol || vertSymbol then prev else 0

middleRow1 :: MiddleRowFn CellVal1
middleRow1 (r0Head : r0Tail) (r1Head : r1Tail) (r2Head : r2Tail) = middleRowRec r0Tail r1Tail r2Tail startHS startVal
  where
    startHS = r1Head == Symbol || r0Head == Symbol || r2Head == Symbol
    startVal = toCV1Digit r1Head
    middleRowRec r0 r1 r2 hasSymbol prev = case (r0, r1, r2) of
        ([], [], []) -> if hasSymbol then prev else 0
        (r0Head : r0Tail, r1Head : r1Tail, r2Head : r2Tail) ->
            let
                (next, add, hasSymbol') = case r1Head of
                    CV1Digit val -> (prev * 10 + val, 0, hasSymbol || vertSymbol)
                    Symbol -> (0, prev, True)
                    CV1Empty -> (0, emptyFinal, vertSymbol)
             in
                add + middleRowRec r0Tail r1Tail r2Tail hasSymbol' next
          where
            vertSymbol = r0Head == Symbol || r2Head == Symbol
            emptyFinal = if hasSymbol || vertSymbol then prev else 0

part2 :: String -> Int
part2 _ = 0

data CellVal2 = CV2Empty | Gear | CV2Digit Int deriving (Eq)

readCellVal2 :: Char -> CellVal2
readCellVal2 char
    | Data.Char.isDigit char = CV2Digit (Data.Char.ord char - Misc.zeroDigit)
    | char == '*' = Gear
    | otherwise = CV2Empty

toCV2Digit :: CellVal2 -> Int
toCV2Digit (CV2Digit val) = val
toCV2Digit _ = 0

isCV2Digit :: CellVal2 -> Bool
isCV2Digit (CV2Digit _) = True
isCV2Digit _ = False

-- borderRow2 :: BorderRowFn CellVal2
-- borderRow2 (aHead : aTail) (bHead : bTail) = borderRowRec