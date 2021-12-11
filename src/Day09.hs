module Day09 (solve) where

import           Data.Foldable   (foldr')
import           Data.List       (sortBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as S

type Pos = (Integer, Integer)
type Grid = Map Pos Integer

parse :: String -> Grid
parse = foldr (\(y, line) acc -> parseLine acc y line) M.empty . zip [0 ..] . lines

parseLine :: Grid -> Integer -> String -> Grid
parseLine grid y = foldr (\(x, c) acc -> M.insert (x, y) (read [c]) acc) grid . zip [0 ..]

isLowZone :: Grid -> (Pos, Integer) -> Bool
isLowZone grid ((x, y), elem) =
    all
        (> elem)
        [ fromMaybe 9 $ grid M.!? (x - 1, y)
        , fromMaybe 9 $ grid M.!? (x + 1, y)
        , fromMaybe 9 $ grid M.!? (x, y - 1)
        , fromMaybe 9 $ grid M.!? (x, y + 1)
        ]

validNeighbours :: Grid -> Pos -> [Pos]
validNeighbours grid (x, y) =
    filter helper [(x - 1, y), (x + 1, y), (x, y -1), (x, y + 1)]
  where
    helper pos = case grid M.!? pos of
        Just a -> a /= 9
        _      -> False

walk :: [Pos] -> Grid -> (Set Pos, Integer) -> (Set Pos, Integer)
walk [] _ (visited, size) = (visited, size)
walk (pos : queue) grid (visited, size) = walk newQueue grid (newVisited, newSize)
  where
    newVisited = S.insert pos visited
    newSize = size + 1
    newQueue = filter (not . flip S.member newVisited) $ queue ++ validNeighbours grid pos

walkBasin :: Grid -> [Integer]
walkBasin grid = snd $ foldr helper (S.empty, []) lowest
  where
    lowest = M.keys . M.filter (/= 9) $ grid

    helper pos (visited, sizes) =
        case walk [pos] grid (visited, 0) of
            (visited, 1)    -> (visited, sizes)
            (visited, size) -> (visited, size : sizes)

part1 :: String -> String
part1 s = show . foldr (\h sum -> sum + (h + 1)) 0 . M.filterWithKey (curry (isLowZone grid)) $ grid
  where
    grid = parse s

part2 :: String -> String
part2 = show . product . take 3 . sortBy (flip compare) . walkBasin . parse

solve :: String -> IO ()
solve input = putStrLn "--- Day 09 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
