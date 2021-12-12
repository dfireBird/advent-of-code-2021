{-# LANGUAGE LambdaCase #-}

module Day12 (solve) where

import           Data.Char       (isLower, isUpper)
import           Data.List.Split
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

data Cave = Small String | Big String | Start | End deriving (Eq, Show, Ord)

type CaveSystem = Map Cave [Cave]

type Path = [Cave]

parse :: String -> CaveSystem
parse = foldr parseLine M.empty . lines

parseLine :: String -> CaveSystem -> CaveSystem
parseLine s = M.insertWith (++) destCave [srcCave] . M.insertWith (++) srcCave [destCave]
  where
    [src, dest] = splitOn "-" s
    srcCave = stringToCave src
    destCave = stringToCave dest

stringToCave :: String -> Cave
stringToCave s = case s of
    "start"           -> Start
    "end"             -> End
    a | all isLower a -> Small a
    a | all isUpper a -> Big a
    _                 -> error "unreachable"

count :: Eq a => a -> [a] -> Integer
count x = fromIntegral . length . filter (== x)

part1 :: String -> String
part1 = show . explore . parse
  where
    walk :: Cave -> CaveSystem -> Path -> Integer
    walk curr system prev = case curr of
        End -> 1
        Start | curr `elem` prev -> 0
        Small _ | curr `elem` prev -> 0
        _ -> foldr (\x acc -> walk x system newVisited + acc) 0 (fromMaybe [] $ system M.!? curr)
      where
        newVisited = curr : prev
        smallCaves = filter (\case Small _ -> True; _ -> False) . M.keys $ system

    explore :: CaveSystem -> Integer
    explore system = walk Start system []

part2 :: String -> String
part2 = show . explore 2 . parse
  where
    walk :: Cave -> CaveSystem -> Path -> Integer -> Integer
    walk curr system prev max = case curr of
        End -> 1
        Start | curr `elem` prev -> 0
        Small _ | Set.size twoVisited == 1 && Set.member curr twoVisited -> 0
        Small _ | Set.size twoVisited == 1 && count curr prev >= 1 -> 0
        _ -> foldr (\x acc -> walk x system newVisited max + acc) 0 (fromMaybe [] $ system M.!? curr)
      where
        newVisited = curr : prev
        smallCaves = filter (\case Small _ -> True; _ -> False) . M.keys $ system

        twoVisited = Set.fromList $ filter (\x -> count x prev >= max) smallCaves

    explore :: Integer -> CaveSystem -> Integer
    explore max system = walk Start system [] max

solve :: String -> IO ()
solve input = putStrLn "--- Day 12 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
