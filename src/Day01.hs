module Day01 (solve) where

part1 :: String -> String
part1 s = show $ fst $ foldl helper (0, Nothing) depths
  where
    depths :: [Integer]
    depths = map read (lines s)

    helper :: (Integer, Maybe Integer) -> Integer -> (Integer, Maybe Integer)
    helper (count, prev) curr =
        case prev of
            Nothing -> (count, Just curr)
            Just n -> if curr > n then (count + 1, Just curr) else (count, Just curr)

part2 :: String -> String
part2 s = show $ fst $ foldl helper (0, Nothing) windows
  where
    depths :: [Integer]
    depths = map read (lines s)

    windows :: [(Integer, Integer, Integer)]
    windows = zip3 depths (tail depths) (tail (tail depths))

    helper :: (Integer, Maybe Integer) -> (Integer, Integer, Integer) -> (Integer, Maybe Integer)
    helper (count, prev) (x, y, z) =
        case prev of
            Nothing -> (count, Just (x + y + z))
            Just n -> if (x + y + z) > n then (count + 1, Just (x + y + z)) else (count, Just (x + y + z))

solve :: String -> IO ()
solve input = putStrLn "--- Day 01 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
