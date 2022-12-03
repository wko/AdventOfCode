module Level1
    ( splitList
    , parseElves
    , maxCalories
    ) where

import Data.List

splitList :: Eq a => a -> [a] -> [[a]]
splitList c = (\(a,b) -> a:b) . foldr f ([],[])
    where 
        f x (cur, acc)
            | x == c = ([], cur:acc)
            | x /= c = (x:cur, acc)

parseElves :: [String] -> [[Int]]
parseElves = map (map read) . splitList "" 

maxCalories :: Int -> [[Int]] -> [Int]
maxCalories n = maximumN n . map sum

maximumN :: Ord a => Int -> [a] -> [a]
maximumN n = take n . reverse . sort

--maximumN :: Int -> [Int] -> [Int]
--maximumN n = foldr f (take n [0..]) 
--    where 
--        f x (m:ms) = if x > m then take n $ reverse $ sort (x:m:ms) else m:ms 