{-# LANGUAGE OverloadedStrings #-}

module Level4 where

import Data.List
import System.IO 
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (chunksOf)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import Data.Text
import Control.Applicative
import Data.Either (fromRight)

type Range = (Int, Int)
type Pair = (Range, Range)

level4 = do 
    input <- liftM (parseOnly pairsParser) . BS.readFile  $ "./files/level4.txt"
    case input of 
        Right pairs -> 
            return $ Just $ Data.List.length $ Data.List.filter isOverlapping pairs 
        Left _ -> return Nothing
        
    

pairsParser :: Parser [Pair]
pairsParser = pairParser `sepBy'` endOfLine

pairParser :: Parser Pair
pairParser = do 
    p1 <- rangeParser
    char ','
    p2 <- rangeParser
    return (p1, p2)

rangeParser :: Parser Range
rangeParser = do 
    l <- decimal
    char '-'
    r <- decimal 
    return (l, r)

isContained :: Pair -> Bool
isContained (r1, r2) = r1 `contains` r2 || r2 `contains` r1
    where 
        contains :: Range -> Range -> Bool
        contains (a1,b1) (a2,b2) = a1 <= a2 && b1 >= b2

isOverlapping :: Pair -> Bool 
isOverlapping (r1, r2) = r1 `overlaps` r2 || r2 `overlaps` r1

overlaps :: Range -> Range -> Bool 
overlaps (a1,b1) (a2,b2) =  a1 <= a2 && b1 >= a2