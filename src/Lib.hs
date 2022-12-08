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
import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)

aoc :: IO ()
aoc = do
         --day8 "aoc/8/example"
         day8 "aoc/8/input"

older :: IO () -- {{{
older = do
         day1 "aoc/1/input"
         day2 "aoc/2/input"
         day3 "aoc/3/input"
         day4 "aoc/4/input"
         day5 "aoc/5/input"
         day6 "aoc/6/input"
         --day7 "aoc/7/input"
         --day7 "aoc/7/example"
--}}}

readLines :: FilePath -> IO [String]
readLines filePath = do
                       rawContents <- readFile filePath
                       return $ lines rawContents

intMap :: [String] -> [Int]
intMap xs = map (read::String->Int) xs

str2ints xs = intMap $ map (:[]) xs

day8 :: String -> IO () -- {{{
day8 filePath = do
                  print $ "Day 8:"
                  rawLines <- readLines filePath
                  let horizontal = map (str2ints) rawLines
                  let vertical = transpose horizontal
                  let coords = [ (x, y) | x <- [0..((pred . length . head) horizontal)], y <- [0..((pred . length . head) vertical)] ]
                  let visi = visiGroups vertical horizontal
                  let results = fmap (visi) coords
                  let visiMap = fmap (checkVisi) results
                  let visibles = fmap (or) $ visiMap
                  let scoreMap = fmap (scoreVisis) results
                  --print $ coords
                  --print $ horizontal
                  --print $ vertical
                  --print $ results
                  --print $ length results
                  --print $ visiMap
                  --print $ visibles
                  --print $ scoreMap
                  --print $ length coords
                  --let testCase = fmap (scoreVisis) [(5,[[3,5,3],[3],[3,3],[4,9]])]
                  --print $ "testCase: " ++ show testCase
                  print $ "length: " ++ show (length visibles)
                  print $ "part 1: " ++ show (sum $ map fromEnum visibles) -- 1695
                  print $ "part 2: " ++ show (last $ sort $ map (foldl1 (*)) scoreMap) -- 287040

--checkVisi (v, [xs]) = map (\x -> (x)) $ filter (not . null) xs
checkVisi (_, []) = []
checkVisi (j, xs) = fmap (checkIt) xs
                    where checkIt [] = True
                          checkIt ls = all (< j) ls

scoreVisis (_, []) = []
scoreVisis (j, xs) = fmap (checkIt) xs
                    where checkIt [] = 0
                          checkIt ls = untilVisible j ls

untilVisible idx [] = 0
untilVisible idx (x:xs)  | x < idx  = 1 + untilVisible idx xs
                         | x >= idx = 1

--visiGroups vert hori (x,y) = ((x,y), v == u, [rbefore, rafter, cbefore, cafter])
visiGroups vert hori (x,y) = (v, [reverse rbefore, rafter, reverse cbefore, cafter])
                                  where row = vert !! y
                                        col = hori !! x
                                        v =  row !! x
                                        u =  col !! y
                                        rbefore = take x row
                                        rafter = drop (x + 1) row
                                        cbefore = take y col
                                        cafter = drop (y + 1) col


-- }}}
day7 :: String -> IO () -- {{{
day7 filePath = do
                  print $ "Day 7:"
                  rawLines <- readLines filePath
                  let listing = map (words) rawLines
                  let tokens = map (tokenizeListing) listing
                  print $ tokens
                  let extendedPaths = extendPaths tokens
                  print $ extendedPaths
                  let totalSizes = processGraph tokens
                  print $ snd totalSizes
                  print $ sum $ map (\(_,size) -> size) $ filter (\(_,size) -> size < 100000) $ fst totalSizes

extendPaths [] = []
extendPaths xs = foldl (applyDir) ["", [], []] xs

applyDir stuff token = case token of
                            ("cd":"..":[]) -> token
                            ("cd":dir:[]) -> token
                            x -> token

processGraph tokens = (zipSizes, graph)
                      where graph = walkTokens tokens
                            dirSizes = fmap (\(idx,(dirs,_)) -> (idx, dirTotal dirs)) graph
                            fileSizes = fmap (\(idx,(_,sizes)) -> (idx, reduceSizes sizes)) graph
                            reduceSizes sizes = sum $ intMap sizes
                            dirTotal dirs = sum $ concat $ fmap (calculate) dirs
                            calculate dir = map (\(_,size) -> size) $ matching dir fileSizes
                            zipSizes = map (\(idx, size) -> (idx, size + (snd $ head $ matching idx dirSizes))) fileSizes
                            matching key = filter (\(idx, _) -> idx == key)

tokenizeListing ("$":"cd":dir) = "cd":dir
tokenizeListing ("$":"ls":[]) = []
tokenizeListing ("dir":dir:[]) = "dir":dir:[]
tokenizeListing (number:_:[]) = number:[]
tokenizeListing (_:_) = error "unknown command"

walkTokens [] = []
walkTokens (token:tokens) = handleToken token
                            where handleToken ("cd":"..":[]) = walkTokens remainder
                                  handleToken ("cd":dir:[]) = [(dir,(getDirs, getFiles))] ++ walkTokens remainder
                                  listing = filter (not . null) $ takeWhile (isContents) tokens
                                  remainder = dropWhile (isContents) tokens
                                  getDirs = concat $ map (tail) $ filter (\x -> head x == "dir") listing
                                  getFiles = concat $ filter (\x -> length x == 1) listing

handleContents token = case (token) of
                            [] -> []
                            (x:y:[]) -> y
                            (n:[]) -> n

isContents token = case (token) of
                        ("cd":dir) -> False
                        (_) -> True

-- }}}
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
                  --print $ not Sop Fours
                  print $ windows 4 elfSignal
                  print $ 4 + length notSopFours -- 1802
                  let takeFourteens = takeGroups 14
                  let fourteens = takeFourteens elfSignal 0
                  let notMessageFourteens = takeWhile (not . detectDistincts 14) fourteens
                  --print $ notMessageFourteens
                  print $ 14 + length notMessageFourteens -- 3551

windows :: Int -> [a] -> [[a]]
windows m = getZipList . sequenceA . map ZipList . take m . tails

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
