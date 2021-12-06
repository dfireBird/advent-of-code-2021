module Day06 (solve) where

import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)

parse :: String -> [Integer]
parse = map read . splitOn ","

toMap :: [Integer] -> Map Integer Integer
toMap = foldr (\x map -> M.insertWith (+) x 1 map) M.empty

simulate :: Map Integer Integer -> Map Integer Integer
simulate fishes = M.insert 8 countZero . M.insertWith (+) 6 countZero . M.delete (-1) . M.mapKeys (+ (-1)) $ fishes
  where
    countZero = fromMaybe 0 $ M.lookup 0 fishes

simulateForN :: Integer -> Map Integer Integer -> Map Integer Integer
simulateForN n days = foldr (\_ b -> simulate b) days [1 .. n]

count :: Map Integer Integer -> Integer
count = M.foldl (+) 0

part1 :: String -> String
part1 = show . count . simulateForN 80 . toMap . parse

part2 :: String -> String
part2 = show . count . simulateForN 256 . toMap . parse

solve :: String -> IO ()
solve input = putStrLn "--- Day 06 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
