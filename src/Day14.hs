module Day14 (solve) where

import           Data.List       (delete)
import           Data.List.Split (splitOn)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Instructions = Map String Char

type Pair = (Char, Char)

parse :: String -> (String, Instructions)
parse s = (template, foldr parseInstruction M.empty . lines $ instructions)
  where
    [template, instructions] = splitOn "\n\n" s

parseInstruction :: String -> Instructions -> Instructions
parseInstruction line = M.insert pair (head c)
  where
    [pair, c] = splitOn " -> " line

pairMaps :: String -> Map Pair Integer
pairMaps [x, y]   = M.insert (x, y) 1 M.empty
pairMaps (x : xs) = M.insertWith (+) (x, head xs) 1 $ pairMaps xs
pairMaps _        = error "should be unreachable"

step :: Map Pair Integer -> Instructions -> Map Pair Integer
step pairCounts instructions = M.foldrWithKey applyPair pairCounts pairCounts
  where
    applyPair :: Pair -> Integer -> Map Pair Integer -> Map Pair Integer
    applyPair _ 0 pairCount = pairCount
    applyPair pair@(x, y) n pairCount = M.adjust (\x -> x - n) pair . M.insertWith (+) (x, c) n . M.insertWith (+) (c, y) n $ pairCount
      where
        c = instructions M.! [x, y]

generations :: Map Pair Integer -> Instructions -> Integer -> Map Pair Integer
generations pairMap instructions n = foldr (\_ acc -> step acc instructions) pairMap [1 .. n]

findMaxMin :: Map Pair Integer -> Char -> (Integer, Integer)
findMaxMin polymer lastChar = (maximum $ M.elems occurenceMap, minimum $ M.elems occurenceMap)
  where
    occurenceToMap = M.foldrWithKey (\(x, _) k acc -> M.insertWith (+) x k acc) M.empty
    occurenceMap = M.insertWith (+) lastChar 1 . occurenceToMap $ polymer

part1 :: String -> String
part1 s = show (max - min)
  where
    (template, instructions) = parse s
    finalPolymer = foldr (\_ acc -> step acc instructions) (pairMaps template) [1 .. 10]
    (max, min) = findMaxMin finalPolymer (last template)

part2 :: String -> String
part2 s = show (max - min)
  where
    (template, instructions) = parse s
    finalPolymer = foldr (\x acc -> step acc instructions) (pairMaps template) [1 .. 40]
    (max, min) = findMaxMin finalPolymer (last template)

solve :: String -> IO ()
solve input = putStrLn "--- Day 14 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
