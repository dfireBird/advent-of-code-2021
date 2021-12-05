module Day05 (solve) where

import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import           Debug.Trace     (trace)

data Coords = Coords {x :: Integer, y :: Integer} deriving (Eq, Show)

instance Ord Coords where
    compare (Coords a b) (Coords c d) = if a == c then compare b d else compare a c

parse :: String -> [(Coords, Coords)]
parse = map (parseCoords . map (map read . splitOn ",") . splitOn " -> ") . lines

parseCoords :: [[Integer]] -> (Coords, Coords)
parseCoords [[xa, ya], [xb, yb]] = (Coords xa ya, Coords xb yb)
parseCoords _                    = error "Not reachable"

inLine :: (Coords, Coords) -> Coords -> Bool
inLine (Coords x0 y0, Coords x1 y1) (Coords x y)
    | x0 == x1 = x0 == x && x1 == x
    | y0 == y1 = y0 == y && y1 == y
    | otherwise = y - y0 == m * (x - x0)
  where
    m = (y1 - y0) `div` (x1 - x0)

getPoints :: (Coords, Coords) -> [Coords]
getPoints p@(Coords xa ya, Coords xb yb) = filter (inLine p) $ case (compare xa xb, compare ya yb) of
    (GT, GT) -> [Coords x y | x <- [xb .. xa], y <- [yb .. ya]]
    (GT, _)  -> [Coords x y | x <- [xb .. xa], y <- [ya .. yb]]
    (_, GT)  -> [Coords x y | x <- [xa .. xb], y <- [yb .. ya]]
    (_, _)   -> [Coords x y | x <- [xa .. xb], y <- [ya .. yb]]

part1 :: String -> String
part1 s = show . M.size . M.filter (>= 2) $ hash
  where
    endCoords = parse s
    allPoints = concatMap getPoints . filter (\(Coords xa ya, Coords xb yb) -> xa == xb || ya == yb) $ endCoords
    hash = foldr (\x h -> M.insertWith (+) x 1 h) M.empty allPoints

part2 :: String -> String
part2 s = show . M.size . M.filter (>= 2) $ hash
  where
    endCoords = parse s
    allPoints = concatMap getPoints endCoords
    hash = foldr (\x h -> M.insertWith (+) x 1 h) M.empty allPoints

solve :: String -> IO ()
solve input = putStrLn "--- Day 05 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
