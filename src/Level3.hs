module Level3 where

import Data.List
import System.IO 
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (chunksOf)


priorities = Map.fromList $ zip (['a'..'z'] ++ ['A'..'Z']) [1..]

data Rucksack = Rucksack {
    left :: String,
    right :: String
}

dublicateItems :: Rucksack -> Set Char 
dublicateItems r = Set.intersection (Set.fromList $ left r) (Set.fromList $ right r)

intersectionM :: Ord a => [Set a] -> Set a
intersectionM [] = Set.empty
intersectionM [x] = x
intersectionM (x:xs) = foldr Set.intersection x xs

type Group = [Rucksack]

badge :: Group -> Char 
badge = head . Set.elems . intersectionM . map (Set.fromList . \r -> left r ++ right r)

score :: Rucksack -> Int 
score = sum . Set.map (priorities Map.!) . dublicateItems 

parseRucksack :: String -> Rucksack
parseRucksack s = Rucksack l r 
    where 
        (l, r) = splitAt (length s `div` 2) s

computeBadgePriorities :: [String] -> Int
computeBadgePriorities = sum . map ((priorities Map.!) . badge) . chunksOf 3 . map parseRucksack