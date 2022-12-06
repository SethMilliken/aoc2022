module Lib
    (
        aoc
      , older
    ) where

import Data.List (sort)
import Data.List (intersect)
import Data.List (nub)
import Data.List (transpose)
import Data.List (elemIndex)
import Data.Char (isLetter)
import Data.Maybe (fromJust)
--import Debug.Trace (trace)

aoc :: IO ()
aoc = do
         day6 "aoc/6/input"


older :: IO () -- {{{
older = do
         day1 "aoc/1/input"
         day2 "aoc/2/input"
         day3 "aoc/3/input"
         day4 "aoc/4/input"
         day5 "aoc/5/input"
--}}}

readLines :: FilePath -> IO [String]
readLines filePath = do
                       rawContents <- readFile filePath
                       return $ lines rawContents

intMap :: [String] -> [Int]
intMap xs = map (read::String->Int) xs

day6 :: String -> IO () -- {{{
day6 filePath = do
                  print $ "Day 6:"
                  rawLines <- readLines filePath
                  let elfSignal = head rawLines
                  --print $ elfSignal
                  print $ "size of signal: " ++ show (length elfSignal)
                  let takeFours = takeGroups 4
                  let fours = takeFours elfSignal 0
                  let notSopFours = takeWhile (not . detectDistincts 4) fours
                  --print $ notSopFours
                  print $ 4 + length notSopFours -- 1802
                  let takeFourteens = takeGroups 14
                  let fourteens = takeFourteens elfSignal 0
                  let notMessageFourteens = takeWhile (not . detectDistincts 14) fourteens
                  --print $ notMessageFourteens
                  print $ 14 + length notMessageFourteens -- 3551

takeGroups :: (Num t, Eq a) => Int -> [a] -> t -> [[a]]
takeGroups size str idx = ([ next ] ++ takeGroups size remain (idx + 1))
                          where next = take size $ str
                                remain = drop 1 str

detectDistincts :: Eq a => Int -> [a] -> Bool
detectDistincts cnt candidate = length (nub candidate) == cnt

-- }}}
day5 :: String -> IO () -- {{{
day5 filePath = do
                  print $ "Day 5:"
                  rawLines <- readLines filePath
                  let stackLines = take 8 rawLines
                  let parsedStack = map (parseStackLines) stackLines
                  let initialStacks = map (filter (/= ' ')) $ head $ map (transpose) [parsedStack]
                  --print $ zipped
                  let moves = drop 10 rawLines
                  let parsedMoves = fmap (parseMove) $ moves
                  --print $ parsedMoves
                  let cm9000AppliedMoves = crateMover9000 initialStacks parsedMoves
                  print $ cm9000AppliedMoves
                  print $ "part 1: " ++ map (head) cm9000AppliedMoves -- VCTFTJQCG
                  let cm9001AppliedMoves = crateMover9001 initialStacks parsedMoves
                  print $ cm9001AppliedMoves
                  print $ "part 2: " ++ map (head) cm9001AppliedMoves -- GCFGLDNJZ

parseStackLines :: [a] -> [a]
parseStackLines xs = [ xs !! x | x <- take 9 [1,5..]]

parseMove :: [Char] -> [Int]
parseMove  xs = intMap . words $ filter (not . isLetter) xs

applyMove :: ([a] -> [a]) -> [[a]] -> [Int] -> [[a]]
applyMove op stack move = swapFor to newDest $ swapFor from newSource stack
               where c = move !! 0
                     from = pred $ move !! 1
                     to = pred $ move !! 2
                     source = stack !! from
                     dest = stack !! to
                     newSource = [ drop c source ]
                     newDest = [(op $ take c source) ++ dest ]

swapFor :: Int -> [a] -> [a] -> [a]
swapFor index newElement stack = take index stack ++ newElement ++ drop (succ index) stack

crateMover9000 :: [[a]] -> [[Int]] -> [[a]]
crateMover9000 stack [] = stack
crateMover9000 stack (move:moves) = crateMover9000 (applyMove reverse stack move) moves

crateMover9001 :: [[a]] -> [[Int]] -> [[a]]
crateMover9001 stack [] = stack
crateMover9001 stack (move:moves) = crateMover9001 (applyMove id stack move) moves

-- }}}
day4 :: String -> IO () -- {{{
day4 filePath = do
                  print $ "Day 4:"
                  rawLines <- readLines filePath
                  let pairs = map (splitOn (/= ',')) rawLines
                  let ranges = map (handleRanges) pairs
                  let subsumedRangeCount = sum $ map (subsumedRanges) ranges
                  let overlapRangeCount = sum $ map (overlappedRanges) ranges
                  --print $ ranges
                  --print $ zip ranges overlapRangeCount
                  print subsumedRangeCount -- 509
                  print overlapRangeCount -- 870

subsumedRanges :: (Ord a1, Ord a2, Num a3) =>  ((a1, a2), (a1, a2)) -> a3
subsumedRanges ((x, y),(s,t))
         | x >= s && y <= t = 1
         | x <= s && y >= t = 1
         | otherwise = 0

overlappedRanges :: (Ord a1, Num a2) => ((a1, a1), (a1, a1)) -> a2
overlappedRanges ((x, y),(s,t))
         | x >= s && x <= t = 1
         | y >= s && y <= t = 1
         | s >= x && s <= y = 1
         | t >= x && t <= y = 1
         | otherwise = 0

handleRanges :: ([Char], [Char]) -> ((Int, Int), (Int, Int))
handleRanges (x,y) = ((numberize . subdiv) x, (numberize . subdiv) y)
                    where subdiv = splitOn (/= '-')
                          numberize (f, s) = (read f :: Int, read s :: Int)

splitOn :: (a -> Bool) -> [a] -> ([a], [a])
splitOn p x = ( front , back)
            where both = span (p) x
                  front = fst both
                  back = (drop 1 . snd) both

-- }}}
day3 :: String -> IO () -- {{{
day3 filePath = do
                  print $ "Day 3:"
                  rawLines <- readLines filePath
                  let sacks = map (splitSack) rawLines
                  let sharedItems = map (sharedSackItem) sacks
                  print $ sum $ map (alphaLookup) sharedItems -- 7889
                  let groupedSacks = groupSacks $ map (nub . sort) rawLines
                  let groupSharedItems = map (foldl1 (intersect)) groupedSacks
                  print $ sum $ map (alphaLookup . head) groupSharedItems -- 2825

splitSack :: String -> (String, String)
splitSack xs = ( front, back )
               where half = (length xs) `div` 2
                     front = (sort . take half) xs
                     back = (sort . drop half) xs

sharedSackItem :: Eq a => ([a], [a]) -> a
sharedSackItem (x, y) = head $ nub $ intersect x y

alphaLookup :: Char -> Int
alphaLookup x = succ $ fromJust $ elemIndex x $ ['a'..'z'] ++ ['A'..'Z']

groupSacks :: [a] -> [[a]]
groupSacks [] = []
groupSacks xs = group ++ rest
                where group = [take 3 xs]
                      rest = (groupSacks . drop 3) xs

-- }}}
day2 :: String -> IO () -- {{{
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

--- }}}
day1 :: String -> IO () -- {{{
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
--- }}}
