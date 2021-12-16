module Day15 (solve) where

import           Data.List       (minimumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (isJust)
import           Data.PSQueue    (PSQ)
import qualified Data.PSQueue    as Q

type Coord = (Integer, Integer)
type Grid = Map Coord (Integer, [Coord])
type PQ = PSQ Coord Integer

maxInt :: Integer
maxInt = 1000000

neighbours :: Coord -> Grid -> [Coord]
neighbours (x, y) grid = filter (`M.member` grid) [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

parse :: String -> Grid
parse = foldr parseLine M.empty . zip [0 ..] . lines

parseLine :: (Integer, String) -> Grid -> Grid
parseLine (y, line) grid = foldr helper grid . zip [0 ..] $ line
  where
    helper (x, c) acc = M.insert (x, y) (read [c], []) acc

fillAdjList :: Grid -> Grid
fillAdjList grid = M.mapWithKey (\k (risk, adjList) -> (risk, neighbours k grid)) grid

expandTile :: Grid -> Grid
expandTile = M.map (\(risk, adj) -> (if risk + 1 > 9 then (risk + 1) `mod` 9 else risk + 1, adj))

buildPQ :: Grid -> PQ
buildPQ = M.foldrWithKey helper Q.empty
  where
    helper (0, 0) _ pq = Q.insert (0, 0) 0 pq
    helper coord _ pq  = Q.insert coord maxInt pq

dijkstraHelper :: Grid -> Coord -> Coord -> (Map Coord Integer, PQ) -> (Map Coord Integer, PQ)
dijkstraHelper grid src vertex (dist, pq) =
    if alt < (dist M.! vertex)
        then (M.insert vertex alt dist, Q.adjust (const alt) vertex pq)
        else (dist, pq)
  where
    vertexVal = grid M.! vertex
    alt = (dist M.! src) + fst vertexVal

    helper newVal (Just (p, v)) = (10, Just (newVal, v))
    helper _ Nothing            = error "Nothing"

dijkstra :: PQ -> Grid -> Coord -> Map Coord Integer -> Map Coord Integer
dijkstra q grid dest dist
    | Q.null q = dist
    | otherwise =
        let vertex = maybe (-1, -1) Q.key (Q.findMin q)
            (_, adj) = grid M.! vertex
            newQ = Q.delete vertex q
            (newDist, newQ') = foldr (dijkstraHelper grid vertex) (dist, newQ) . filter (\x -> isJust $ x `Q.lookup` newQ) $ adj
         in dijkstra newQ' grid dest newDist

findLowPath :: Grid -> Coord -> Integer
findLowPath grid destCoord = dijkstra pq grid destCoord distMap M.! destCoord
  where
    pq = buildPQ grid
    distMap = M.insert (0, 0) 0 $ M.foldrWithKey (\k _ dist -> M.insert k maxInt dist) M.empty grid

part1 :: String -> String
part1 s = show $ findLowPath (fillAdjList grid) (maxX, maxX)
  where
    grid = parse s
    maxX = maximum . map fst . M.keys $ grid

part2 :: String -> String
part2 s = show $ findLowPath (fillAdjList expandedMap) (maxX, maxX)
  where
    grid = parse s
    size = (maximum . map fst . M.keys $ grid) + 1
    helperH (prev, grid) n = let newTile = expandTile prev in (newTile, M.foldrWithKey (\(x, y) v b -> M.insert (x + (size * n), y) v b) grid newTile)
    helperV (prev, grid) n = let newTile = expandTile prev in (newTile, M.foldrWithKey (\(x, y) v b -> M.insert (x, y + (size * n)) v b) grid newTile)
    horizontalExpandedTile = snd $ foldl helperH (grid, grid) [1 .. 4]
    expandedMap = snd $ foldl helperV (horizontalExpandedTile, horizontalExpandedTile) [1 .. 4]
    maxX = maximum . map fst . M.keys $ expandedMap

solve :: String -> IO ()
solve input = putStrLn "--- Day 15 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
