module Day13 (solve) where

import           Data.List.Split (splitOn)
import           Data.Set        (Set)
import qualified Data.Set        as Set

type Pos = (Integer, Integer)
type Grid = Set Pos

data Instruction = X Integer | Y Integer deriving (Eq, Show)

parse :: String -> (Grid, [Instruction])
parse s = (Set.fromList . map parsePos . lines $ positions, map parseInstruction . lines $ foldInstructions)
  where
    [positions, foldInstructions] = splitOn "\n\n" s

parsePos :: String -> Pos
parsePos line = (read x, read y)
  where
    [x, y] = splitOn "," line

parseInstruction :: String -> Instruction
parseInstruction line = case co of
    "x" -> X $ read num
    "y" -> Y $ read num
    _   -> error "unreachable"
  where
    [_, _, instruction] = words line
    [co, num] = splitOn "=" instruction

fold :: Instruction -> Grid -> Grid
fold (X n) = Set.map (\(x, y) -> if x > n then (n - (x - n), y) else (x, y))
fold (Y n) = Set.map (\(x, y) -> if y > n then (x, n - (y - n)) else (x, y))

printGrid :: Grid -> String
printGrid grid = foldr (\y acc -> foldr (\x acc -> (if Set.member (x, y) grid then '█' else ' ') : acc) "" [minX .. maxX] ++ "\n" ++ acc) "" [minY .. maxY]
  where
    (minX, maxX) = (minimum $ Set.map fst grid, maximum $ Set.map fst grid)
    (minY, maxY) = (minimum $ Set.map snd grid, maximum $ Set.map snd grid)

part1 :: String -> String
part1 s = show . Set.size $ fold (head instruction) grid
  where
    (grid, instruction) = parse s

part2 :: String -> String
part2 s = printGrid $ foldl (flip fold) grid instruction
  where
    (grid, instruction) = parse s

solve :: String -> IO ()
solve input = putStrLn "--- Day 13 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
