{-# LANGUAGE OverloadedStrings #-}

module Level6 where

import Data.List ( findIndex)
import System.IO () 
import qualified Data.Map as Map

level6a :: IO(Maybe Int)
level6a = do 
    input <- readFile  $ "./files/level6.txt"
    -- fold input to find the first position where four consecutive characters are different
    return $ solve 4 input

level6b :: IO(Maybe Int)
level6b = do 
    input <- readFile  $ "./files/level6.txt"
    -- fold input to find the first position where four consecutive characters are different
    return $ solve 14 input


solve :: Int -> String -> Maybe Int 
solve n = fmap (+ n) . findIndexOfNConsecutiveDifferent n 

findIndexOfNConsecutiveDifferent :: Int -> String -> Maybe Int
findIndexOfNConsecutiveDifferent n = findIndex (not . containsDuplicates) . helper n
    

helper :: Int -> [a] -> [[a]]
helper n xs = if length xs >= n then take n xs : helper n (tail xs) else []

-- check if a list contains any duplicates
containsDuplicates :: Ord a => [a] -> Bool
containsDuplicates xs = length xs /= length (Map.keys . Map.fromList $ zip xs ([1..] :: [Int]))