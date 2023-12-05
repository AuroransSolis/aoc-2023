module Misc (aocMain) where

import qualified System.IO

aocMain :: (Show a, Show b) => String -> (String -> a) -> (String -> b) -> IO ()
aocMain filename part1 part2 = do
    contents <- System.IO.readFile filename
    let part1Out = part1 contents
        part2Out = part2 contents
    putStrLn $ "p1: " ++ show part1Out
    putStrLn $ "p2: " ++ show part2Out
