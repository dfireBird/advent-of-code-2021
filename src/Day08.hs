{-# OPTIONS_GHC -Wno-missing-fields #-}

module Day08 (solve) where

import           Data.List (sort)

type Display = String

data SevenSegment = SevenSegment
    { top      :: Char
    , mid      :: Char
    , bottom   :: Char
    , lefttop  :: Char
    , leftbtm  :: Char
    , righttop :: Char
    , rightbtm :: Char
    }
    deriving (Show, Eq)

parse :: String -> [([Display], [Display])]
parse = map parseSingle . lines

parseSingle :: String -> ([Display], [Display])
parseSingle s = (take 10 . words $ s, drop 11 . words $ s)

count :: Eq a => a -> [a] -> Integer
count a = foldr (\x sum -> if x == a then sum + 1 else sum) 0

decode :: [Display] -> SevenSegment
decode dpy = SevenSegment top mid btm leftTop leftBtm rightTop rightBtm
  where
    allSegments = concat dpy
    alpha = "abcdefg"
    four = head . filter ((== 4) . length) $ dpy

    top = head . filter (\x -> (count x allSegments == 8) && (x `notElem` four)) $ alpha
    leftTop = head . filter (\x -> count x allSegments == 6) $ alpha
    rightTop = head . filter (\x -> (count x allSegments == 8) && (x `elem` four)) $ alpha
    mid = head . filter (\x -> (count x allSegments == 7) && (x `elem` four)) $ alpha
    leftBtm = head . filter (\x -> count x allSegments == 4) $ alpha
    rightBtm = head . filter (\x -> count x allSegments == 9) $ alpha
    btm = head . filter (\x -> (count x allSegments == 7) && (x `notElem` four)) $ alpha

digit :: SevenSegment -> Display -> Integer
digit ss display
    | sort display == sort [righttop ss, rightbtm ss] = 1
    | sort display == sort [bottom ss, leftbtm ss, mid ss, righttop ss, top ss] = 2
    | sort display == sort [bottom ss, mid ss, top ss, rightbtm ss, righttop ss] = 3
    | sort display == sort [lefttop ss, mid ss, righttop ss, rightbtm ss] = 4
    | sort display == sort [bottom ss, rightbtm ss, mid ss, lefttop ss, top ss] = 5
    | sort display == sort [bottom ss, rightbtm ss, mid ss, leftbtm ss, lefttop ss, top ss] = 6
    | sort display == sort [top ss, righttop ss, rightbtm ss] = 7
    | sort display == sort [top ss, righttop ss, rightbtm ss, mid ss, lefttop ss, leftbtm ss, bottom ss] = 8
    | sort display == sort [bottom ss, rightbtm ss, mid ss, righttop ss, lefttop ss, top ss] = 9
    | sort display == sort [top ss, righttop ss, rightbtm ss, lefttop ss, leftbtm ss, bottom ss] = 0
    | otherwise = error "Unpre"

decodeDigits :: SevenSegment -> [Display] -> Integer
decodeDigits ss = foldl (\num x -> num * 10 + digit ss x) 0

part1 :: String -> String
part1 = show . length . concatMap (filter (isUnique . length) . snd) . parse
  where
    isUnique x = (x == 2) || (x == 3) || (x == 4) || (x == 7)

part2 :: String -> String
part2 = show . sum . map helper . parse
  where
    helper (segments, digits) = decodeDigits (decode segments) digits

solve :: String -> IO ()
solve input = putStrLn "--- Day 08 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
