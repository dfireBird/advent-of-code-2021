module Day07 (solve) where

import           Data.List.Split (splitOn)

parse :: String -> [Integer]
parse = map read . splitOn ","

part1 :: String -> String
part1 s = show $ minimum $ map (helper positions) [0 .. maximum positions]
  where
    positions = parse s

    helper xs n = sum . map (abs . (n -)) $ xs

part2 :: String -> String
part2 s = show $ minimum $ map (helper positions) [0 .. maximum positions]
  where
    positions = parse s

    helper xs n = sum . map (\x -> let c = abs (n - x) in (c * (c + 1)) `div` 2) $ xs

solve :: String -> IO ()
solve input = putStrLn "--- Day 07 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
