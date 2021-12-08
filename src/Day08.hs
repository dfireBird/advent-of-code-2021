{-# OPTIONS_GHC -Wno-missing-fields #-}

module Day08 (solve) where

import           Data.List (delete, sort)

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

isUnique :: Int -> Bool
isUnique x
    | x == 2 = True
    | x == 3 = True
    | x == 4 = True
    | x == 7 = True
    | otherwise = False

ifExist :: Eq a => [a] -> [a] -> Bool
ifExist [] _        = True
ifExist (c : cs) xs = c `elem` xs && ifExist cs xs

remove :: Eq a => [a] -> [a] -> [a]
remove [] xs       = xs
remove (x : xs) ys = remove xs (delete x ys)

decode :: [Display] -> SevenSegment
decode dpy = SevenSegment top mid btm leftTop leftBtm rightTop rightBtm
  where
    possiblerights = head . filter ((== 2) . length) $ dpy -- 1
    midOrLeftTop = remove possiblerights . head . filter ((== 4) . length) $ dpy -- 4
    top = head . remove possiblerights . head . filter ((== 3) . length) $ dpy -- 7
    midOrBtm = remove (top : possiblerights) . head . filter (ifExist possiblerights) . filter ((== 5) . length) $ dpy -- 3
    (mid, btm) = (head . filter (`elem` midOrLeftTop) $ midOrBtm, head . remove midOrLeftTop $ midOrBtm)
    leftTop = head . delete mid $ midOrLeftTop
    six = head . filter (not . ifExist possiblerights) . filter ((== 6) . length) $ dpy -- 6
    rightBtm = head . filter (`elem` possiblerights) $ six
    rightTop = head . delete rightBtm $ possiblerights
    leftBtm = head . remove (top : leftTop : rightBtm : midOrBtm) $ six

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
part1 s = show . length . concatMap (filter (isUnique . length) . snd) $ segments
  where
    segments = parse s

part2 :: String -> String
part2 = show . sum . map helper . parse
  where
    helper (segments, digits) = decodeDigits (decode segments) digits

solve :: String -> IO ()
solve input = putStrLn "--- Day 08 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
