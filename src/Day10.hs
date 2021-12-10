module Day10 (solve) where

import           Data.List  (sort)
import           Data.Maybe (fromMaybe)

type Stack a = [a]

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> (Maybe a, Stack a)
pop []       = (Nothing, [])
pop (x : xs) = (Just x, xs)

peek :: Stack a -> Maybe a
peek []      = Nothing
peek (x : _) = Just x

invert :: Char -> Maybe Char
invert '}' = Just '{'
invert ']' = Just '['
invert '>' = Just '<'
invert ')' = Just '('
invert '(' = Just ')'
invert '[' = Just ']'
invert '{' = Just '}'
invert '<' = Just '>'
invert _   = Nothing

isInvalid :: String -> Bool
isInvalid xs = helper xs []
  where
    helper [] _ = False
    helper (x : xs) s
        | x `elem` "({[<" = helper xs (push x s)
        | x `elem` ")}]>" = peek s /= invert x || helper xs (snd . pop $ s)
        | otherwise = error "Unreahcable"

completeIncompelete :: String -> [Maybe Char]
completeIncompelete xs = helper xs []
  where
    helper [] s = map invert s
    helper (x : xs) s
        | x `elem` "({[<" = helper xs (push x s)
        | x `elem` ")}]>" = helper xs (snd . pop $ s)
        | otherwise = error "Unreacable"

part1 :: String -> String
part1 = show . sum . map (maybe 0 score . invalidChar) . lines
  where
    invalidChar :: String -> Maybe Char
    invalidChar xs = helper xs []
      where
        helper [] _ = Nothing
        helper (x : xs) s
            | x `elem` "({[<" = helper xs (push x s)
            | x `elem` ")}]>" = if peek s == invert x then helper xs (snd . pop $ s) else Just x
            | otherwise = error "Unreachbale"

    score ')' = 3
    score ']' = 57
    score '}' = 1197
    score '>' = 25137
    score _   = error "Unreachable"

part2 :: String -> String
part2 = show . getMiddle . map (foldl helper 0 . fromMaybe "" . sequence . completeIncompelete) . filter (not . isInvalid) . lines
  where
    getMiddle xs = sort xs !! (length xs `div` 2)

    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4
    score _   = error "Unreachable"

    helper = \accum x -> accum * 5 + score x

solve :: String -> IO ()
solve input = putStrLn "--- Day 10 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
