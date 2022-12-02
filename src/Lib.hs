module Lib
    (
      aoc
    ) where

import Data.List (sort)

aoc :: IO ()
--aoc = day1 "aoc/1/input"
aoc = day1 "aoc/1/input"

readLines :: FilePath -> IO [String]
readLines filePath = do
                       rawContents <- readFile filePath
                       return $ lines rawContents

day1 :: String -> IO ()
day1 filePath = do
                  rawLines <- readLines filePath
                  let groupSums = map (foldr (+) 0) $ parseCalorieGroups rawLines
                  print $ maximum groupSums
                  print $ (foldr (+) 0 . take 3 . reverse . sort) groupSums

parseCalorieGroups :: [String] -> [[Int]]
parseCalorieGroups [] = []
parseCalorieGroups xs = [(intMap . fst . extractGroup) xs] ++ (parseCalorieGroups . drop 1 . snd . extractGroup) xs
                        where extractGroup xt = span (/= "") xt
                              intMap xu = map (read::String->Int) xu
