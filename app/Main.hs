module Main (main) where

import Level1
import Level2
import Level3
import FordFulkerson ( test )
import Control.Monad



main :: IO ()
main = do 
    test
    

level1a = do 
    input <- liftM lines . readFile $  "./files/level1.txt"
    let m = head $ computeCalories 1 input
    putStrLn $ show m 

level1b = do 
    input <- liftM lines . readFile $  "./files/level1.txt"
    let m = sum $ computeCalories 3 input
    putStrLn $ show m 

level2a = do 
    input <- liftM lines . readFile $  "./files/level2.txt"
    let m = sum $ map (roundScore . parseRound mapping) input
    return m

level2b = do 
    input <- liftM lines . readFile $  "./files/level2.txt"
    let m = sum $ map (roundScore . parseRoundB) input
    return m


level3a = do 
    input <- liftM lines . readFile $  "./files/level3.txt"
    let m = sum $ map (Level3.score . parseRucksack) input
    return m

level3b = do 
    input <- liftM lines . readFile $  "./files/level3.txt"
    let m = computeBadgePriorities input
    return m


mapping :: Char -> Char 
mapping 'X' = 'A'
mapping 'Y' = 'B' 
mapping 'Z' = 'C'


computeCalories n = maxCalories n . parseElves 