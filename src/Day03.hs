module Day03 (solve) where

import           Data.Char       (digitToInt)
import           Data.List       (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

invert :: String -> String
invert = map flip
  where
    flip '0' = '1'
    flip '1' = '0'
    flip _   = error "Not possible"

frequencies :: String -> Map Char Int
frequencies =
    foldr
        ( \x freq ->
            if x == '0'
                then M.adjust (+ 1) '0' freq
                else M.adjust (+ 1) '1' freq
        )
        (M.fromList [('0', 0), ('1', 0)])

separateBits :: [String] -> [String]
separateBits xs = map (\n -> map (!! n) xs) [0 .. length (head xs) -1]

calculateFreq :: [String] -> [Map Char Int]
calculateFreq = map frequencies . separateBits

calRating :: [String] -> Int -> (Int -> Int -> Bool) -> Char -> String
calRating [x] _ _ _ = x
calRating xs n f bit = calRating filtered_list (n + 1) f bit
  where
    bit_frequency = calculateFreq xs !! n
    filtering_bit
        | (bit_frequency M.! '0') == (bit_frequency M.! '1') = bit
        | (bit_frequency M.! '0') `f` (bit_frequency M.! '1') = '0'
        | otherwise = '1'

    filtered_list = filter (\x -> x !! n == filtering_bit) xs

part1 :: String -> String
part1 s = show $ toDec epsilonRate * toDec gamaRate
  where
    binaries = words s

    calGammaRate :: [Map Char Int] -> String
    calGammaRate = foldr gammaBit ""
      where
        gammaBit :: Map Char Int -> String -> String
        gammaBit freq s = if (freq M.! '0') > (freq M.! '1') then '0' : s else '1' : s

    gamaRate = calGammaRate $ calculateFreq binaries
    epsilonRate = invert gamaRate

part2 :: String -> String
part2 s = show $ toDec oxyRating * toDec co2Rating
  where
    binaries = words s

    oxyRating = calRating binaries 0 (>) '1'
    co2Rating = calRating binaries 0 (<) '0'

solve :: String -> IO ()
solve input = putStrLn "--- Day 03 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
