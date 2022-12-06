{-# LANGUAGE OverloadedStrings #-}

module Level5 where

import Data.List
import System.IO 
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split (chunksOf)
import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Applicative
import Data.Either (fromRight)
import Data.Maybe (fromJust, catMaybes)
import Data.Vector (Vector)
import qualified Data.Vector as V 
import qualified Data.Text.IO as TIO
import Debug.Trace

type Block = Char 
type Stack = [Block]
type Storage = Vector Stack
data Move = Move {
    num :: Int,
    from :: Int,
    to :: Int 
} deriving Show

type Puzzle = (Storage, [Move])

getPuzzleSolution :: Storage -> String 
getPuzzleSolution = V.toList . V.map head 



level5a :: IO(Maybe String)
level5a = do 
    input <- liftM (parseOnly puzzleParser) . TIO.readFile  $ "./files/level5.txt"
    case input of 
        Right (initialStorage, moves) -> do
            putStrLn (getPuzzleSolution initialStorage)
            let finalStorage = foldl' (applyMove reverse) initialStorage moves
            putStrLn $ show finalStorage
            return $ Just $ getPuzzleSolution finalStorage 
        Left _ -> return Nothing
        

level5b :: IO(Maybe String)
level5b = do 
    input <- liftM (parseOnly puzzleParser) . TIO.readFile  $ "./files/level5.txt"
    case input of 
        Right (initialStorage, moves) -> do
            putStrLn (getPuzzleSolution initialStorage)
            let finalStorage = foldl' (applyMove id) initialStorage moves
            putStrLn $ show finalStorage
            return $ Just $ getPuzzleSolution finalStorage 
        Left _ -> return Nothing



storageParser :: Parser [Storage]
storageParser = undefined

surroundedBy begin end middle = begin *> middle <* end


blockParser :: Parser (Maybe Block)
blockParser = filledBlock <|> emptyBlock
    where
        filledBlock = Just <$> surroundedBy (char '[') (char ']') letter
        emptyBlock :: Parser (Maybe Block)
        emptyBlock =  string "   " *> return Nothing

rowParser :: Parser [Maybe Block]
rowParser = blockParser `sepBy'` char ' '

fieldParser :: Parser [[Maybe Block]]
fieldParser = do 
    rows <- rowParser `sepBy'` endOfLine
    _ <- surroundedBy space space digit `sepBy'` space
    return rows


transform :: [[Maybe Block]] -> Storage
transform = V.fromList . map catMaybes . Data.List.transpose
    

move :: Parser Move 
move = do 
    _ <- string "move "
    n <- decimal
    _ <- string " from " 
    f <- decimal 
    _ <- " to "
    t <- decimal 
    return $ Move n (f-1) (t-1)

moves :: Parser [Move]
moves = move `sepBy1` endOfLine

puzzleParser :: Parser Puzzle
puzzleParser = do 
    storage <- transform <$> fieldParser 
    endOfLine
    endOfLine
    m <- moves 
    return (storage, m)
    
applyMove :: ([Block] -> [Block]) -> Storage -> Move -> Storage
applyMove f s m = s V.// [(from m, remaining), (to m,  f toMove ++ s V.! (to m) )]
    where 
        (toMove, remaining) = splitAt (num m) $ s V.! (from m) 
        
