module Level2 where

import Data.List
import System.IO 
import Control.Monad


data Operation = Paper | Stone | Scissor deriving (Show)
data Round = Round {
    opp :: Operation,
    you :: Operation
} deriving (Show)
data Result = Win | Lose | Draw deriving (Show)

score :: Operation -> Int 
score Stone = 1
score Paper = 2
score Scissor = 3

result :: Round -> Result
result (Round Paper Scissor) = Win 
result (Round Scissor Stone) = Win 
result (Round Stone Paper) = Win
result (Round Scissor Paper) = Lose 
result (Round Stone Scissor) = Lose 
result (Round Paper Stone) = Lose
result _ = Draw

winScore :: Result -> Int 
winScore Lose = 0
winScore Win = 6
winScore Draw = 3
    
roundScore :: Round -> Int 
roundScore r = winScore (result r) + score (you r)

parseRound :: (Char -> Char) -> String -> Round 
parseRound decode [o,_,y] = Round (parseOperation o) (parseOperation $ decode y)   
        
parseOperation :: Char -> Operation
parseOperation 'A' = Stone
parseOperation 'B' = Paper 
parseOperation 'C' = Scissor

parseResult :: Char -> Result
parseResult 'X' = Lose 
parseResult 'Y' = Draw 
parseResult 'Z' = Win 

parseRoundB :: String -> Round 
parseRoundB [o,_,y] = let opp = parseOperation o in Round opp (turn opp $ parseResult y)

turn :: Operation -> Result -> Operation 
turn op Draw = op
turn Stone Win = Paper 
turn Paper Win = Scissor
turn Scissor Win = Stone
turn Stone Lose = Scissor
turn Paper Lose = Stone
turn Scissor Lose = Paper