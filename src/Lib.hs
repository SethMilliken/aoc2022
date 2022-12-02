module Lib
    (
        aoc
      , older
    ) where

import Data.List (sort)
--import Debug.Trace (trace)

aoc :: IO ()
aoc = do
         day1 "aoc/1/input"
         day2 "aoc/2/input"

older :: IO ()
older = do
         day1 "aoc/1/input"

readLines :: FilePath -> IO [String]
readLines filePath = do
                       rawContents <- readFile filePath
                       return $ lines rawContents

day2 :: String -> IO ()
day2 filePath = do
                  print $ "Day 2:"
                  rawLines <- readLines filePath
                  let strategyPairs = parseStrategyPairs rawLines
                  let calculated = map (calculateStrategyValue) strategyPairs
                  let wins = map (calculateWin) calculated
                  let contrivedWins = map (selectRosh) calculated
                  --print $ strategyPairs
                  --print $ calculated
                  --print $ wins
                  --print $ contrivedWins
                  print $ foldr1 (+) wins -- 10941
                  print $ foldr1 (+) contrivedWins -- 13071


parseStrategyPairs :: [String] -> [String]
parseStrategyPairs [] = []
parseStrategyPairs (x:xs) = filter (/=' ') x : parseStrategyPairs xs
--parseStrategyPairs (x:xs) = trace x $ filter (/=' ') x : parseStrategyPairs xs

calculateStrategyValue :: [Char] -> [Int]
calculateStrategyValue xs = (intMap . map (roshValue)) xs
                            where intMap xu = map (read::String->Int) xu

calculateWin :: [Int] -> Int
calculateWin xs = winCalc + last xs
                  where winCalc = roshWin $ foldl1 (subtract) xs

roshValue :: Char -> String
roshValue 'A' = "1" -- Rock
roshValue 'B' = "2" -- Paper
roshValue 'C' = "3" -- Scissors
roshValue 'X' = "1" -- Rock / Lose
roshValue 'Y' = "2" -- Paper / Draw
roshValue 'Z' = "3" -- Scissors / Win
roshValue _ = error "Undefined lookup"

roshWin :: Int -> Int
roshWin (-2) = 6
roshWin (-1) = 0
roshWin 0 = 3
roshWin 1 = 6
roshWin 2 = 0
roshWin _ = error "Undefined lookup"

selectRosh :: [Int] -> Int
selectRosh xs = (selval xs . head) xs + winval xs
                where winval xt = (winValue . last) xt
                      selval xu = (funcLookup . last) xu

winValue :: Int -> Int
winValue 1 = 0
winValue 2 = 3
winValue 3 = 6
winValue _ = error "Undefined lookup"

winLookup :: Int -> Int
winLookup 1 = 2
winLookup 2 = 3
winLookup 3 = 1
winLookup _ = error "Undefined lookup"

lossLookup :: Int -> Int
lossLookup 1 = 3
lossLookup 2 = 1
lossLookup 3 = 2
lossLookup _ = error "Undefined lookup"

drawLookup :: Int -> Int
drawLookup x = x

funcLookup :: Int -> Int -> Int
funcLookup 1 = lossLookup
funcLookup 2 = drawLookup
funcLookup 3 = winLookup
funcLookup _ = error "Undefined lookup"



day1 :: String -> IO ()
day1 filePath = do
                  print $ "Day 1:"
                  rawLines <- readLines filePath
                  let groupSums = map (foldr (+) 0) $ parseCalorieGroups rawLines
                  print $ maximum groupSums -- 72070
                  print $ (foldr (+) 0 . take 3 . reverse . sort) groupSums -- 211805

parseCalorieGroups :: [String] -> [[Int]]
parseCalorieGroups [] = []
parseCalorieGroups xs = [(intMap . fst) extractGroup] ++ (parseCalorieGroups . drop 1 . snd) extractGroup
                        where extractGroup = span (/= "") xs
                              intMap xt = map (read::String->Int) xt
