module D7Lib (part1, part2) where

import Data.List (sort)
import qualified Misc

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind deriving (Eq, Ord)

data CardType1 = C12 | C13 | C14 | C15 | C16 | C17 | C18 | C19 | C1T | C1J | C1Q | C1K | C1A deriving (Eq, Ord)
data Hand1 = Hand1 {cards1 :: [CardType1], handType1 :: HandType, wager1 :: Int} deriving (Eq)

cardFromChar1 :: Char -> CardType1
cardFromChar1 char = case char of
    '2' -> C12
    '3' -> C13
    '4' -> C14
    '5' -> C15
    '6' -> C16
    '7' -> C17
    '8' -> C18
    '9' -> C19
    'T' -> C1T
    'J' -> C1J
    'Q' -> C1Q
    'K' -> C1K
    _ -> C1A

-- Retry this using a method similar to `handFromLine2`?
handFromLine1 :: [CardType1] -> Int -> Hand1
handFromLine1 cards@[c0, c1, c2, c3, c4] = Hand1 cards handType
  where
    pairSort a b =
        if a > b
            then [a, b]
            else [b, a]
    -- sorting done by sorting network
    -- [(0, 3), (1, 4)]
    [c01, c31] = pairSort c0 c3
    [c11, c41] = pairSort c1 c4
    c21 = c2
    -- [(0, 2), (1, 3)]
    [c02, c22] = pairSort c01 c21
    [c12, c32] = pairSort c11 c31
    c42 = c41
    -- [(0, 1), (2, 4)]
    [c03, c13] = pairSort c02 c12
    [c23, c43] = pairSort c22 c42
    c33 = c32
    -- [(1, 2), (3, 4)]
    [c14, c24] = pairSort c13 c23
    [c34, c44] = pairSort c33 c43
    c04 = c03
    -- [(2, 3)]
    [c25, c35] = pairSort c24 c34
    c05 = c04
    c15 = c14
    c45 = c44
    -- find hand type
    eq01 = c05 == c15
    eq12 = c15 == c25
    eq23 = c25 == c35
    eq34 = c35 == c45
    is5K = eq01 && eq12 && eq23 && eq34
    mid3K = eq12 && eq23
    is4K = (eq01 || eq34) && mid3K
    srt3K = eq01 && eq12
    end3K = eq23 && eq34
    handType
        | is5K = FiveKind
        | is4K = FourKind
        | (eq34 && srt3K) || (eq01 && end3K) = FullHouse
        | srt3K || mid3K || end3K = ThreeKind
        | otherwise = case filter fst [(eq01, c05), (eq12, c15), (eq23, c25), (eq34, c35)] of
            [(_, p1), (_, p2)] -> TwoPair
            [(_, p1)] -> OnePair
            [] -> HighCard

instance Ord Hand1 where
    compare (Hand1 c1 t1 _) (Hand1 c2 t2 _) = case compare t1 t2 of
        EQ -> compare c1 c2
        other -> other

part1 :: String -> Int
part1 str = getSum hands 1
  where
    readHands :: String -> [Hand1]
    readHands str = case str of
        (c0 : c1 : c2 : c3 : c4 : _ : tail) -> newHand : readHands tail'
          where
            (wager, tail') = readWager 0 tail
            newHand = handFromLine1 (fmap cardFromChar1 [c0, c1, c2, c3, c4]) wager
        [] -> []
    readWager acc (head : tail) = case head of
        '\n' -> (acc, tail)
        other -> readWager (acc * 10 + Misc.charToDigit other) tail
    hands = sort $ readHands str
    getSum list rank = case list of
        [] -> 0
        ((Hand1 _ _ wager) : tail) -> wager * rank + getSum tail (rank + 1)

data CardType2 = C2J | C22 | C23 | C24 | C25 | C26 | C27 | C28 | C29 | C2T | C2Q | C2K | C2A deriving (Eq, Ord)
data Hand2 = Hand2 {cards2 :: [CardType2], handType2 :: HandType, wager2 :: Int} deriving (Eq)

instance Ord Hand2 where
    compare (Hand2 c1 t1 _) (Hand2 c2 t2 _) = case compare t1 t2 of
        EQ -> compare c1 c2
        other -> other

cardFromChar2 :: Char -> CardType2
cardFromChar2 char = case char of
    'J' -> C2J
    '2' -> C22
    '3' -> C23
    '4' -> C24
    '5' -> C25
    '6' -> C26
    '7' -> C27
    '8' -> C28
    '9' -> C29
    'T' -> C2T
    'Q' -> C2Q
    'K' -> C2K
    _ -> C2A

handFromLine2 :: [CardType2] -> Int -> Hand2
handFromLine2 cards = Hand2 cards handType
  where
    pairSort a b = if a > b then [a, b] else [b, a]
    getCards list acc = case list of
        (head : tail) -> case head of
            C2J -> getCards tail acc
            other -> getCards tail (insertCard other acc)
        [] -> acc
    insertCard card list = case list of
        head@(hcount, htype) : tail ->
            if htype == card
                then (hcount + 1, htype) : tail
                else head : insertCard card tail
        [] -> [(1, card)]
    cardTypes = getCards cards []
    handType = case cardTypes of
        -- no other cards means five jokers -> FiveKind
        [] -> FiveKind
        -- only one non-joker means that all jokers can act as that card -> FiveKind
        [_] -> FiveKind
        -- sorted possible cases:
        -- [4, 1] -> FourKind
        -- [3, 2] -> FullHouse
        -- [3, 1] -> FourKind
        -- [2, 2] -> FullHouse
        -- [2, 1] -> FourKind
        -- [1, 1] -> FourKind
        -- special cases:
        --     [3, 2], [2, 3], [2, 2]
        [(c0, _), (c1, _)] -> case [c0, c1] of
            [3, 2] -> FullHouse
            [2, 3] -> FullHouse
            [2, 2] -> FullHouse
            _ -> FourKind
        -- sorted possible cases:
        -- [3, 1, 1] -> ThreeKind
        -- [2, 2, 1] -> TwoPair
        -- [2, 1, 1] -> ThreeKind
        -- [1, 1, 1] -> ThreeKind
        -- only one special case: sum 5, one count is 2
        [(c0, _), (c1, _), (c2, _)] ->
            if c0 + c1 + c2 == 5 && elem 2 [c0, c1, c2]
                then TwoPair
                else ThreeKind
        -- sorted possible cases:
        -- [2, 1, 1, 1] -> OnePair
        -- [1, 1, 1, 1] -> OnePair
        [_, _, _, _] -> OnePair
        -- five cards -> HighCard
        _ -> HighCard

part2 :: String -> Int
part2 str = getSum hands 1
  where
    readHands str = case str of
        (c0 : c1 : c2 : c3 : c4 : _ : tail) -> newHand : readHands tail'
          where
            (wager, tail') = readWager 0 tail
            newHand = handFromLine2 (fmap cardFromChar2 [c0, c1, c2, c3, c4]) wager
        [] -> []
    readWager acc (head : tail) = case head of
        '\n' -> (acc, tail)
        other -> readWager (acc * 10 + Misc.charToDigit other) tail
    hands = sort $ readHands str
    getSum list rank = case list of
        [] -> 0
        ((Hand2 _ _ wager) : tail) -> wager * rank + getSum tail (rank + 1)
