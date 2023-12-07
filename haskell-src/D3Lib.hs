module D3Lib (part1, part2) where

import qualified Data.Char
import qualified Misc

data CellVal = Empty | Symbol | Digit Int deriving (Show, Eq)
readCellVal :: Char -> CellVal
readCellVal char
    | Data.Char.isDigit char = Digit $ Data.Char.ord char - Misc.zeroDigit
    | isSymbol char = Symbol
    | otherwise = Empty
  where
    isSymbol c = c `elem` ['#', '%', '&', '*', '+', '-', '/', '=', '@', '$']

toDigit :: CellVal -> Int
toDigit (Digit val) = val
toDigit _ = 0

isDigit :: CellVal -> Bool
isDigit (Digit _) = True
isDigit _ = False

part1 :: String -> Int
part1 str = processRowStart $ makeProcess $ part1ReadGrid str

part1ReadGrid :: String -> [[CellVal]]
part1ReadGrid grid =
    cons
        ( case breakGrid grid of
            (line, rest) ->
                ( line
                , case rest of
                    [] -> []
                    _ : rest' -> part1ReadGrid rest'
                )
        )
  where
    cons ~(a, b) = a : b

breakGrid :: [Char] -> ([CellVal], String)
breakGrid [] = ([], [])
breakGrid all@(head : tail)
    | head == '\n' = ([], all)
    | otherwise = let (nextHead, nextTail) = breakGrid tail in (headVal : nextHead, nextTail)
  where
    headVal = readCellVal head

data ProcessGrid = ProcessGrid
    { r0 :: [CellVal]
    , r1 :: [CellVal]
    , r2 :: [CellVal]
    , rest :: [[CellVal]]
    }

makeProcess :: [[CellVal]] -> ProcessGrid
makeProcess all@(r1 : r2 : rest) = ProcessGrid [] r1 r2 all

processRowStart :: ProcessGrid -> Int
-- Start case - no first line
processRowStart (ProcessGrid [] (r10 : r1Rest) (r20 : r2Rest) rest) = case r10 of
    Digit val -> processRow (ProcessGrid [] r1Rest r2Rest rest) (r20 == Symbol) val
    Symbol -> processRow (ProcessGrid [] r1Rest r2Rest rest) True 0
    Empty -> processRow (ProcessGrid [] r1Rest r2Rest rest) (r20 == Symbol) 0
-- End case - no last line
processRowStart (ProcessGrid (r00 : r0Rest) (r10 : r1Rest) [] rest) = case r10 of
    Digit val -> processRow (ProcessGrid r0Rest r1Rest [] rest) (r00 == Symbol) val
    Symbol -> processRow (ProcessGrid r0Rest r1Rest [] rest) True 0
    Empty -> processRow (ProcessGrid r0Rest r1Rest [] rest) (r00 == Symbol) 0
-- General case
processRowStart (ProcessGrid (r00 : r0Rest) (r10 : r1Rest) (r20 : r2Rest) rest) = case r10 of
    Digit val -> processRow (ProcessGrid r0Rest r1Rest r2Rest rest) (r00 == Symbol || r20 == Symbol) val
    Symbol -> processRow (ProcessGrid r0Rest r1Rest r2Rest rest) True 0
    Empty -> processRow (ProcessGrid r0Rest r1Rest r2Rest rest) (r00 == Symbol || r20 == Symbol) 0

processRow :: ProcessGrid -> Bool -> Int -> Int
-- First row, final value
processRow (ProcessGrid [] [r1Head] [r2Head] (newR0 : rest@(newR1 : newR2 : _))) hasSymbol prev = case r1Head of
    Digit val -> digitFinal val + processRowStart (ProcessGrid newR0 newR1 newR2 rest)
    Symbol -> prev + processRowStart (ProcessGrid newR0 newR1 newR2 rest)
    Empty -> emptyFinal + processRowStart (ProcessGrid newR0 newR1 newR2 rest)
  where
    newHasSymbol = hasSymbol || r2Head == Symbol
    digitFinal v = if newHasSymbol then prev * 10 + v else 0
    emptyFinal = if r2Head == Symbol then prev else 0
-- First row, general value
processRow (ProcessGrid [] (r1Head : r1Rest) (r2Head : r2Rest) rest) hasSymbol prev = case r1Head of
    Digit val -> processRow (ProcessGrid [] r1Rest r2Rest rest) newHasSymbol (prev * 10 + val)
    Symbol -> prev + processRow (ProcessGrid [] r1Rest r2Rest rest) True 0
    Empty -> finalValue + processRow (ProcessGrid [] r1Rest r2Rest rest) (r2Head == Symbol) 0
  where
    newHasSymbol = hasSymbol || r2Head == Symbol
    finalValue = if newHasSymbol then prev else 0
-- Final row, final value
processRow (ProcessGrid [r0Head] [r1Head] [] rest) hasSymbol prev = case r1Head of
    Digit val -> digitFinal val
    Symbol -> prev
    Empty -> emptyFinal
  where
    newHasSymbol = hasSymbol || r0Head == Symbol
    digitFinal v = if newHasSymbol then prev * 10 + v else 0
    emptyFinal = if newHasSymbol then prev else 0
-- Final row, general value
processRow (ProcessGrid (r0Head : r0Rest) (r1Head : r1Rest) [] rest) hasSymbol prev = case r1Head of
    Digit val -> processRow (ProcessGrid r0Rest r1Rest [] rest) newHasSymbol (prev * 10 + val)
    Symbol -> prev + processRow (ProcessGrid r0Rest r1Rest [] rest) True 0
    Empty -> finalValue + processRow (ProcessGrid r0Rest r1Rest [] rest) (r0Head == Symbol) 0
  where
    newHasSymbol = hasSymbol || r0Head == Symbol
    finalValue = if newHasSymbol then prev else 0
-- General row, final value
processRow (ProcessGrid [r0Head] [r1Head] [r2Head] (newR0 : rest@(newR1 : newR2 : _))) hasSymbol prev = case r1Head of
    Digit val -> digitFinal val + processRowStart (ProcessGrid newR0 newR1 newR2 rest)
    Symbol -> prev + processRowStart (ProcessGrid newR0 newR1 newR2 rest)
    Empty -> emptyFinal + processRowStart (ProcessGrid newR0 newR1 newR2 rest)
  where
    newHasSymbol = hasSymbol || r0Head == Symbol || r2Head == Symbol
    digitFinal v = if newHasSymbol then prev * 10 + v else 0
    emptyFinal = if newHasSymbol then prev else 0
-- General row, general value
processRow (ProcessGrid (r0Head : r0Rest) (r1Head : r1Rest) (r2Head : r2Rest) rest) hasSymbol prev = case r1Head of
    Digit val -> processRow (ProcessGrid r0Rest r1Rest r2Rest rest) newHasSymbol (prev * 10 + val)
    Symbol -> prev + processRow (ProcessGrid r0Rest r1Rest r2Rest rest) True 0
    Empty -> finalValue + processRow (ProcessGrid r0Rest r1Rest r2Rest rest) vertSymbol 0
  where
    vertSymbol = r0Head == Symbol || r2Head == Symbol
    newHasSymbol = hasSymbol || vertSymbol
    finalValue = if newHasSymbol then prev else 0

part2 :: String -> Int
part2 _ = 0
