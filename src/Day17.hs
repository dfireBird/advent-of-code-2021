module Day17 (solve) where

import           Data.List.Split (splitOn)
import           Debug.Trace     (trace)

data TargetArea = TargetArea
    { minX :: Integer
    , minY :: Integer
    , maxX :: Integer
    , maxY :: Integer
    }
    deriving (Show, Eq)

type Velocity = (Integer, Integer)
type Coord = (Integer, Integer)

parse :: String -> TargetArea
parse s = TargetArea (minimum x) (minimum y) (maximum x) (maximum y)
  where
    [xString, yString] = drop 2 . words $ s
    x = map read . splitOn ".." . drop 2 . init $ xString
    y = map read . splitOn ".." . drop 2 $ yString

step :: (Velocity, Coord) -> (Velocity, Coord)
step ((vx, vy), (x, y)) = ((decX vx, vy - 1), (x + vx, y + vy))
  where
    decX vx
        | vx > 0 = vx - 1
        | vx < 0 = vx + 1
        | otherwise = vx

simulate :: TargetArea -> Velocity -> Bool
simulate area v = helper area (v, (0, 0))
  where
    helper area@(TargetArea minX minY maxX maxY) cv@((vx, _), (x, y))
        | (x <= maxX && x >= minX) && (y <= maxY && y >= minY) = True
        | x > 100000 || y > 100000 = False
        | x < -100000 || y < -100000 = False
        | otherwise = helper area $ step cv

maxYCoord :: Velocity -> Integer
maxYCoord v = helper (v, (0, 0)) 0
  where
    helper cv maximumY =
        let newCv@((_, _), (_, y)) = step cv
         in if y < 0
                then maximumY
                else if y > maximumY then helper newCv y else helper newCv maximumY

part1 :: String -> String
part1 s = show . maximum . map maxYCoord $ velocitiesInTarget
  where
    area@(TargetArea _ _ maxX _) = parse s
    velocitiesInTarget = filter (simulate area) $ [(x, y) | x <- [1 .. maxX], y <- [1 .. 10000]]

part2 :: String -> String
part2 s = show . length $ velocitiesInTarget
  where
    area@(TargetArea _ minY maxX _) = parse s
    velocitiesInTarget = filter (simulate area) $ [(x, y) | x <- [0 .. maxX], y <- [minY .. 1000]]

solve :: String -> IO ()
solve input = putStrLn "--- Day 17 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
