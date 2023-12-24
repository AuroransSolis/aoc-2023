module Misc (aocMain, zeroDigit, charToDigit, fract) where

import qualified Data.Char
import GHC.Float (int2Double, int2Float)
import qualified System.IO

aocMain :: (Show a, Show b) => String -> (String -> a) -> (String -> b) -> IO ()
aocMain filename part1 part2 = do
    contents <- System.IO.readFile filename
    let part1Out = part1 contents
        part2Out = part2 contents
    putStrLn $ "p1: " ++ show part1Out
    putStrLn $ "p2: " ++ show part2Out

zeroDigit :: Int
zeroDigit = Data.Char.ord '0'

charToDigit :: Char -> Int
charToDigit char = Data.Char.ord char - zeroDigit

fract :: (RealFrac a, FromInt a) => a -> a
fract val = val - fromInt (truncate val)

class FromInt a where
    fromInt :: Int -> a

instance FromInt Float where
    fromInt = int2Float

instance FromInt Double where
    fromInt = int2Double
