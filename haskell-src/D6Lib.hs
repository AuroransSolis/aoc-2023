module D6Lib (part1, part2) where

import Debug.Trace (trace)
import GHC.Float (int2Double, int2Float, sqrt)
import qualified Misc

part1 :: String -> Int
part1 str = product $ zipWith solvePair times dists
  where
    (line2, times) = readTimes (drop 9 str) [] 0
    readTimes (head : tail) times acc = case head of
        '\n' -> (tail, acc : times)
        ' ' -> readTimes tail times' 0
        other -> readTimes tail times (acc * 10 + Misc.charToDigit other)
      where
        times' =
            if acc > 0
                then acc : times
                else times
    dists = readDists (drop 9 line2) [] 0
    readDists (head : tail) dists acc = case head of
        '\n' -> acc : dists
        ' ' -> readDists tail dists' 0
        other -> readDists tail dists (acc * 10 + Misc.charToDigit other)
      where
        dists' =
            if acc > 0
                then acc : dists
                else dists
    solvePair time dist = end - start'
      where
        time' = int2Float time
        dist' = int2Float dist
        disc = sqrt (time' * time' - 4.0 * dist') / 2.0
        time'' = time' / 2.0
        start = time'' - disc
        start' =
            if Misc.fract start > 0.0
                then floor start
                else floor start + 1
        end = floor (time'' + disc)

part2 :: String -> Int
part2 str = end - start'
  where
    (line2, time) = readTime (drop 9 str) 0
    readTime (head : tail) acc = case head of
        '\n' -> (tail, acc)
        ' ' -> readTime tail acc
        other -> readTime tail (acc * 10 + Misc.charToDigit other)
    dist = readDist (drop 9 line2) 0
    readDist (head : tail) acc = case head of
        '\n' -> acc
        ' ' -> readDist tail acc
        other -> readDist tail (acc * 10 + Misc.charToDigit other)
    time' = int2Double time
    dist' = int2Double dist
    disc = sqrt (time' * time' - 4.0 * dist') / 2.0
    time'' = time' / 2.0
    start = time'' - disc
    start' =
        if Misc.fract start > 0.0
            then floor start
            else floor start + 1
    end = floor (time'' + disc)
