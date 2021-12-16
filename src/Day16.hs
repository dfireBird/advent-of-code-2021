module Day16 (solve) where

import           Control.Monad   (liftM2, replicateM)
import           Data.Char       (digitToInt)
import           Data.List       (foldl')
import           Data.Void       (Void)
import           Text.Megaparsec

hexIntoBits :: Char -> String
hexIntoBits '0' = "0000"
hexIntoBits '1' = "0001"
hexIntoBits '2' = "0010"
hexIntoBits '3' = "0011"
hexIntoBits '4' = "0100"
hexIntoBits '5' = "0101"
hexIntoBits '6' = "0110"
hexIntoBits '7' = "0111"
hexIntoBits '8' = "1000"
hexIntoBits '9' = "1001"
hexIntoBits 'A' = "1010"
hexIntoBits 'B' = "1011"
hexIntoBits 'C' = "1100"
hexIntoBits 'D' = "1101"
hexIntoBits 'E' = "1110"
hexIntoBits 'F' = "1111"
hexIntoBits _   = error "Unreachable"

type Parser = Parsec Void String

data Value = Literal Integer | Operator [Packet] deriving (Show, Eq)

data Packet = Packet
    { version :: Integer
    , typeId  :: Integer
    , value   :: Value
    }
    deriving (Show, Eq)

parseIntoBits :: String -> String
parseIntoBits = foldr (\a b -> hexIntoBits a ++ b) ""

toDec :: String -> Integer
toDec = foldl' (\acc x -> acc * 2 + fromIntegral (digitToInt x)) 0

parseLiteral :: Parser Value
parseLiteral = fmap (Literal . toDec) helper
  where
    helper :: Parser String
    helper = do
        bits <- takeP (Just "literal") 5
        if head bits == '0'
            then return (tail bits)
            else fmap (\x -> tail bits ++ x) helper

parseWithNumber :: Parser [Packet]
parseWithNumber = do
    number <- fmap toDec . takeP (Just "number") $ 11
    replicateM (fromInteger number) parsePacket

parseWithLength :: Parser [Packet]
parseWithLength = do
    len <- fmap toDec . takeP (Just "length") $ 15
    start <- getOffset
    helper start (fromInteger len)
  where
    helper :: Int -> Int -> Parser [Packet]
    helper start len = do
        cur <- getOffset
        if (cur - start) /= len
            then liftM2 (:) parsePacket (helper start len)
            else return []

parseOperator :: Parser Value
parseOperator = do
    lengthType <- takeP (Just "length type") 1
    fmap Operator $ if lengthType == "1" then parseWithNumber else parseWithLength

parsePacket :: Parser Packet
parsePacket = do
    version <- fmap toDec . takeP (Just "version") $ 3
    typeId <- fmap toDec . takeP (Just "type id") $ 3
    value <- if typeId == 4 then parseLiteral else parseOperator
    return $ Packet version typeId value

sumVersion :: Packet -> Integer
sumVersion packet =
    case value packet of
        Literal _   -> version packet
        Operator ps -> version packet + sum (map sumVersion ps)

apply :: Packet -> Integer
apply (Packet _ 0 (Operator ps))     = foldr (\a b -> apply a + b) 0 ps
apply (Packet _ 1 (Operator ps))     = foldr (\a b -> apply a * b) 1 ps
apply (Packet _ 2 (Operator ps))     = minimum . map apply $ ps
apply (Packet _ 3 (Operator ps))     = maximum . map apply $ ps
apply (Packet _ 4 (Literal value))   = value
apply (Packet _ 5 (Operator [a, b])) = if apply a > apply b then 1 else 0
apply (Packet _ 6 (Operator [a, b])) = if apply a < apply b then 1 else 0
apply (Packet _ 7 (Operator [a, b])) = if apply a == apply b then 1 else 0
apply _                              = error "Unreachable"

part1 :: String -> String
part1 s = show . sumVersion $ packet
  where
    bits = parseIntoBits s
    (Right packet) = runParser parsePacket "" bits

part2 :: String -> String
part2 s = show . apply $ packet
  where
    bits = parseIntoBits s
    (Right packet) = runParser parsePacket "" bits

solve :: String -> IO ()
solve input = putStrLn "--- Day 15 ---" >> putStrLn (part1 input) >> putStrLn (part2 input)
