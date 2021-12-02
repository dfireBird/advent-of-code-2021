module Day02 (solve) where

data Direction = Forward Integer | Up Integer | Down Integer deriving (Eq, Show)

parser :: String -> [Direction]
parser = map parser' . lines
  where
    parser' s = case words s of
        ["forward", x] -> Forward $ read x
        ["down", x]    -> Down $ read x
        ["up", x]      -> Up $ read x
        _              -> undefined

part1 :: String -> String
part1 s = show (uncurry (*) final_pos)
  where
    paths = parser s

    part1' (hor, ver) d = case d of
        Forward x -> (hor + x, ver)
        Down x    -> (hor, ver + x)
        Up x      -> (hor, ver - x)

    final_pos = foldl part1' (0, 0) paths

part2 :: String -> String
part2 s = show (hor * ver)
  where
    paths = parser s

    part2' (hor, ver, aim) d = case d of
        Forward x -> (hor + x, ver + (aim * x), aim)
        Down x    -> (hor, ver, aim + x)
        Up x      -> (hor, ver, aim - x)

    (hor, ver, aim) = foldl part2' (0, 0, 0) paths

solve :: String -> IO ()
solve input = putStrLn "--- Day 02 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
