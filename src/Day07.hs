module Day07 (solve) where

import           Data.List.Split (splitOn)

parse :: String -> [Integer]
parse = map read . splitOn ","

part1 :: String -> String
part1 s = show $ minimum $ map (helper positions) [0 .. maximum positions]
  where
    positions = parse s

    helper xs n = sum . map (\x -> abs (n - x)) $ xs

part2 :: String -> String
part2 s = show $ minimum $ map (helper positions) [0 .. maximum positions]
  where
    positions = parse s

    helper xs n = sum . map (\x -> sum [1 .. abs (n - x)]) $ xs

solve :: String -> IO ()
solve input = putStrLn "--- Day 07 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
