module D3Lib (part1, part2) where

import Control.Monad (when)
import qualified Data.Char
import Data.Functor ((<&>))
import Data.List (uncons)
import qualified Data.Maybe
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

type BorderRowFn1 a = [a] -> [a] -> Int
type MiddleRowFn1 a = [a] -> [a] -> [a] -> Int

data ProcessGrid a = ProcessGrid
    { r0 :: [a]
    , r1 :: [a]
    , r2 :: [a]
    , rest :: [[a]]
    }

makeProcess1 :: [[a]] -> ProcessGrid a
makeProcess1 (r1 : r2 : rest) = ProcessGrid [] r1 r2 rest

walkGrid1 :: ProcessGrid a -> BorderRowFn1 a -> MiddleRowFn1 a -> Int
walkGrid1 (ProcessGrid [] r1 r2 (newR2 : rest)) b m = b r1 r2 + walkGrid1 (ProcessGrid r1 r2 newR2 rest) b m
-- Flipping the last two rows lets us reuse the same function as for the first two rows
walkGrid1 (ProcessGrid r0 r1 [] rest) b m = b r1 r0
walkGrid1 (ProcessGrid r0 r1 r2 rest) b m =
    m r0 r1 r2 + case uncons rest of
        Just (head, tail) -> walkGrid1 (ProcessGrid r1 r2 head tail) b m
        Nothing -> walkGrid1 (ProcessGrid r1 r2 [] []) b m

part1 :: String -> Int
part1 str = walkGrid1 (makeProcess1 $ readMapGrid str readCV1) borderRow1 middleRow1

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

borderRow1 :: BorderRowFn1 CellVal1
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

middleRow1 :: MiddleRowFn1 CellVal1
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
part2 str = walkGrid2 (makeProcess1 $ readMapGrid str readCV2) borderRow2 middleRow2

data CellVal2 = CV2Empty | Gear | CV2Digit Int deriving (Eq)

instance Show CellVal2 where
    show CV2Empty = "."
    show Gear = "*"
    show (CV2Digit val) = show val

readCV2 :: Char -> CellVal2
readCV2 char
    | Data.Char.isDigit char = CV2Digit $ Misc.charToDigit char
    | char == '*' = Gear
    | otherwise = CV2Empty

toCV2Digit :: CellVal2 -> Int
toCV2Digit (CV2Digit val) = val
toCV2Digit _ = 0

isCV2Digit :: CellVal2 -> Bool
isCV2Digit (CV2Digit _) = True
isCV2Digit _ = False

makeProcess2 :: [[a]] -> ProcessGrid a
makeProcess2 all@(r1 : r2 : _) = ProcessGrid [] r1 r2 all

data BorderStepData a = BorderStepData
    { rA :: [a]
    , rB :: [a]
    , prevRA :: [a]
    , prevRB :: [a]
    }

data MiddleStepData a = MiddleStepData
    { msdR0 :: [a]
    , msdR1 :: [a]
    , msdR2 :: [a]
    , prevR0 :: [a]
    , prevR1 :: [a]
    , prevR2 :: [a]
    }

type BorderRowFn2 a = BorderStepData a -> Int
type MiddleRowFn2 a = MiddleStepData a -> Int

walkGrid2 :: (Show a) => ProcessGrid a -> BorderRowFn2 a -> MiddleRowFn2 a -> Int
walkGrid2 (ProcessGrid [] r1 r2 (newR2 : rest)) b m = b (BorderStepData r1 r2 [] []) + walkGrid2 (ProcessGrid r1 r2 newR2 rest) b m
-- Flipping the last two rows lets us reuse the same function as for the first two rows
walkGrid2 (ProcessGrid r0 r1 [] rest) b m = b (BorderStepData r1 r0 [] [])
walkGrid2 (ProcessGrid r0 r1 r2 rest) b m =
    m (MiddleStepData r0 r1 r2 [] [] []) + case uncons rest of
        Just (head, tail) -> walkGrid2 (ProcessGrid r1 r2 head tail) b m
        Nothing -> walkGrid2 (ProcessGrid r1 r2 [] []) b m

borderRow2 :: BorderRowFn2 CellVal2
borderRow2 (BorderStepData aList bList prevA prevB) = case (aList, bList) of
    ([], []) -> 0
    (aCur : aTail, bCur : bTail) -> gearVal + borderRow2 (BorderStepData aTail bTail (aCur : prevA) (bCur : prevB))
      where
        gearVal = case aCur of
            Gear -> case filter (> 0) [rAR, rB, rAL] of
                [n1] -> if doubleB then n1 else 0
                [n1, n2] -> n1 * n2
                _ -> 0
              where
                rAR = snd $ readRight aTail (1, 0)
                (rBMul0, rBAcc0) = readRight bTail (1, 0)
                (rB, doubleB) = case bCur of
                    CV2Digit val -> (readLeft prevB (rBMul0 * 10, rBMul0 * val + rBAcc0), False)
                    _ ->
                        if rBAcc1 > 0 && rBAcc0 > 0
                            then (rBAcc0 * rBAcc1, True)
                            else (rBAcc0 + rBAcc1, False)
                      where
                        rBAcc1 = readLeft prevB (1, 0)
                rAL = readLeft prevA (1, 0)
            _ -> 0

middleRow2 :: MiddleRowFn2 CellVal2
middleRow2 (MiddleStepData r0List r1List r2List r0Prev r1Prev r2Prev) = case (r0List, r1List, r2List) of
    ([], [], []) -> 0
    (r0Cur : r0Tail, r1Cur : r1Tail, r2Cur : r2Tail) -> gearVal + middleRow2 middleStepData'
      where
        r0Prev' = r0Cur : r0Prev
        r1Prev' = r1Cur : r1Prev
        r2Prev' = r2Cur : r2Prev
        middleStepData' = MiddleStepData r0Tail r1Tail r2Tail r0Prev' r1Prev' r2Prev'
        gearVal = case r1Cur of
            Gear -> case filter (> 0) [r0, r1L, r1R, r2] of
                [n1] -> if double then n1 else 0
                [n1, n2] -> n1 * n2
                _ -> 0
              where
                (r0Mul0, r0Acc0) = readRight r0Tail (1, 0)
                (_, r1R) = readRight r1Tail (1, 0)
                (r2Mul0, r2Acc0) = readRight r2Tail (1, 0)
                (r0, double0) = case r0Cur of
                    CV2Digit val -> (readLeft r0Prev (r0Mul0 * 10, r0Mul0 * val + r0Acc0), False)
                    _ ->
                        if r0Acc1 > 0 && r0Acc0 > 0
                            then (r0Acc0 * r0Acc1, True)
                            else (r0Acc0 + r0Acc1, False)
                      where
                        r0Acc1 = readLeft r0Prev (1, 0)
                (r2, double2) = case r2Cur of
                    CV2Digit val -> (readLeft r2Prev (r2Mul0 * 10, r2Mul0 * val + r2Acc0), False)
                    _ ->
                        if r2Acc1 > 0 && r2Acc0 > 0
                            then (r2Acc0 * r2Acc1, True)
                            else (r2Acc0 + r2Acc1, False)
                      where
                        r2Acc1 = readLeft r2Prev (1, 0)
                r1L = readLeft r1Prev (1, 0)
                double = double0 || double2
            _ -> 0

traceShowPfx :: (Show a) => [Char] -> a -> a
traceShowPfx pfx a = trace (pfx ++ ": " ++ show a) a

traceShowIf :: (Show a) => (a -> Bool) -> a -> a
traceShowIf fn a =
    if fn a
        then traceShowId a
        else a

readRight :: [CellVal2] -> (Int, Int) -> (Int, Int)
readRight [] val = val
readRight (head : tail) prev@(mul, acc) = case head of
    CV2Digit digit -> readRight tail (mul * 10, acc * 10 + digit)
    other -> prev

readLeft :: [CellVal2] -> (Int, Int) -> Int
readLeft [] (mul, acc) = acc
readLeft (head : tail) (0, _) = 0
readLeft (head : tail) (mul, acc) = case head of
    CV2Digit digit -> readLeft tail (mul * 10, acc + digit * mul)
    other -> acc
