module D3Lib (part1, part2) where

import qualified Data.Char
import qualified Misc

data CellVal = Empty | Symbol | Digit Int Int
readCellVal :: Char -> CellVal
readCellVal char
    | Data.Char.isDigit char = Data.Char.ord char - Misc.zeroDigit
    | isSymbol char = Symbol
    | otherwise = Empty
  where
    isSymbol c = 

part1 :: String -> Int
part1 _ = 0

type Coord = (Int, Int)

part1Rec :: String -> Int
part1Rec

part1ReadGrid :: String -> [[CellVal]]
part1ReadGrid grid = cons(case breakGrid grid of
    (l, r) -> (l, case r of
        [] -> []
        _:r' -> breakGrid r'))

breakGrid _ [] = ([], [])
breakGrid all@(head : tail)
    | head == '\n' = ([], all)
    | otherwise = let (nextHead, nextTail) = breakGrid tail in (head:nextHead, nextTail)
  where
    headVal = 
{-
break _ xs@[] =  (xs, xs)
break p xs@(x:xs')
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs' in (x:ys,zs)

break (== 1) [2, 1, 0]

break p all@(2:[1, 0]) -> let (ys, zs) = break p [1, 0] in 

lines s = cons (case break (== '\n') s of
    (l, s') -> (l, case s' of
        [] -> []
        _:s'' -> lines s''))
  where
    cons ~(h, t) =  h : t
-}


part2 :: String -> Int
part2 _ = 0

data Tree a = Empty | Node a (Tree a) (Tree a)

singleton :: a -> Tree a
singleton x = Node x Empty Empty

treeInsert :: (Ord a) => Tree a -> a -> Tree a
treeInsert Empty x = singleton x
treeInsert (Node a l r) b
    | a == b = Node a l r
    | a < b = Node a (treeInsert l b) r
    | a > b = Node a l (treeInsert r b)

treeHas :: (Ord a) => Tree a -> a -> bool
treeHas Nothing a = False
treeHas (Node a l r) b
    | a == b = True
    | a < b = treeHas l b
    | a > b = treeHas r b
