{-# LANGUAGE TupleSections #-}

module Day04 (solve) where

import           Data.List       (transpose)
import           Data.List.Split

type BingoBoard = [[(Bool, Integer)]]

parse :: String -> ([Integer], [BingoBoard])
parse s = (nums, boards)
  where
    nums = map read . splitOn "," . head . lines $ s
    boards = map (flip parseBoard [] . tail) . chunksOf 6 . tail . lines $ s

parseBoard :: [String] -> BingoBoard -> BingoBoard
parseBoard [] b = b
parseBoard (x : xs) b = t : parseBoard xs b
  where
    t :: [(Bool, Integer)]
    t = map ((False,) . read) . words $ x

checkWin :: BingoBoard -> Bool
checkWin b = horizontalWin b || horizontalWin (transpose b)
  where
    horizontalWin = any (all fst)

progress :: Integer -> BingoBoard -> BingoBoard
progress _ [] = []
progress n (x : xs) = map (\(b, x) -> if x == n then (True, x) else (b, x)) x : progress n xs

calculateAns :: [BingoBoard] -> Integer -> ([BingoBoard] -> BingoBoard) -> Integer
calculateAns boards x f =
    let choosen_board = f boards
        unmarked_number = concatMap (map snd . filter (not . fst)) choosen_board
     in x * sum unmarked_number

part1 :: String -> String
part1 s = show $ part1' numbers boards
  where
    (numbers, boards) = parse s

    part1' [] boards = 0
    part1' (x : xs) boards =
        let new_boards = map (progress x) boards
         in if any checkWin new_boards
                then calculateAns new_boards x (head . filter checkWin)
                else part1' xs new_boards

part2 :: String -> String
part2 s = show $ part2' numbers boards
  where
    (numbers, boards) = parse s

    part2' [] boards = 0
    part2' (x : xs) boards =
        let new_boards = map (progress x) boards
         in if all checkWin new_boards
                then calculateAns boards x (progress x . head . filter (not . checkWin))
                else part2' xs new_boards

solve :: String -> IO ()
solve input = putStrLn "--- Day 04 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
