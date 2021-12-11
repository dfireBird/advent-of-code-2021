module Day11 (solve) where

import           Data.Map        (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (isJust)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Debug.Trace     (trace)

type Pos = (Integer, Integer)
type Grid = Map Pos Integer

parse :: String -> Grid
parse = foldr (\(y, line) acc -> parseLine y acc line) M.empty . zip [0 ..] . lines

parseLine :: Integer -> Grid -> String -> Grid
parseLine y grid = foldr (\(x, c) acc -> M.insert (x, y) (read [c]) acc) grid . zip [0 ..]

findNeighbours :: Grid -> Pos -> [Pos]
findNeighbours grid (x, y) =
    filter
        helper
        [ (x - 1, y)
        , (x + 1, y)
        , (x, y -1)
        , (x, y + 1)
        , (x + 1, y + 1)
        , (x + 1, y - 1)
        , (x - 1, y + 1)
        , (x - 1, y - 1)
        ]
  where
    helper pos = isJust $ grid M.!? pos

flash :: [Pos] -> Grid -> Set Pos -> (Grid, Integer)
flash [] grid flashes = (grid, fromIntegral $ Set.size flashes)
flash (pos : queue) grid flashes = flash newQueue neighbourAdded newFlashes
  where
    newFlashes = Set.insert pos flashes
    validNeighbours = filter (\x -> not (Set.member x newFlashes)) $ findNeighbours grid pos
    neighbourAdded = foldr (M.adjust (+ 1)) (M.insert pos 0 grid) validNeighbours
    helper x acc = if neighbourAdded M.! x > 9 && (x `notElem` queue) then x : acc else acc
    newQueue = queue ++ foldr helper [] validNeighbours

step :: Grid -> (Grid, Integer)
step grid = flash flashQueue oneAddedGrid Set.empty
  where
    oneAddedGrid = M.map (+ 1) grid
    flashQueue = M.keys . M.filter (> 9) $ oneAddedGrid

part1 :: String -> String
part1 s = show . sum $ flashes
  where
    grid = parse s
    helper _ (grid, flashes) =
        let (newGrid, flash) = step grid
         in (newGrid, flash : flashes)
    (_grid, flashes) = foldr helper (grid, []) [1 .. 100]

part2 :: String -> String
part2 s = show $ helper 1 (parse s, [])
  where
    grid = parse s

    helper n (grid, flashes) =
        if all (== 0) . M.elems $ newGrid
            then n
            else helper (n + 1) (newGrid, flashes)
      where
        (newGrid, flash) = step grid

solve :: String -> IO ()
solve input = putStrLn "--- Day 11 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
