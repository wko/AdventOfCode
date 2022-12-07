{-# LANGUAGE OverloadedStrings #-}

module Level7 where

import qualified Data.List as List
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


data FileSystem = File T.Text Int | Directory T.Text [FileSystem] deriving Show

data Path = Absolute [T.Text] | Relative [T.Text] deriving Show
data Command = Ls [Output] | Cd Path deriving Show
data Output = FileOutput T.Text Int | DirectoryOutput T.Text deriving Show

-- pretty print the filesystem
prettyPrint :: FileSystem -> String
prettyPrint (File name size) = show name ++ " " ++ show size
prettyPrint (Directory name children) = show name ++ "\n" ++ List.intercalate "\n" (List.map (\c -> "  " ++ prettyPrint c) children)


--level7a :: IO(Maybe String)
level7a = do 
    --parsed <- liftM (parseOnly (cmdParser `sepBy1'` space')) . TIO.readFile  $ "./files/level7test.txt"
    --print parsed
    input <- liftM (parseOnly fileSystemParser) . TIO.readFile  $ "./files/level7.txt"
    case input of 
        Left err -> return Nothing
        Right fs -> do 
            let sizes = Map.toList $ sizeMap "/" fs
                threshold = 100000 
                smallDirs = List.filter (\(_, size) -> size <= threshold) sizes
            return $ Just $ sum $ List.map snd smallDirs
            
level7b = do 
    input <- liftM (parseOnly fileSystemParser) . TIO.readFile  $ "./files/level7.txt"
    case input of 
        Left err -> return Nothing
        Right fs -> do 
            let sizes = Map.toList $ sizeMap "/" fs
                capacity = 70000000
                spaceRequired = 30000000
                spaceTaken = size fs
                freeSpace = capacity - spaceTaken
                minAmountOfSpaceToFree = spaceRequired - freeSpace 
            print spaceTaken
            print minAmountOfSpaceToFree
            let
                largeEnoughDirs = List.filter (\(_, size) -> size >= minAmountOfSpaceToFree) sizes
            print largeEnoughDirs
            -- pay attention to sort by size, not name
            return $ Just $ head $ List.sortBy (\a b -> compare (snd a) (snd b)) largeEnoughDirs


fileSystemParser :: Parser FileSystem
fileSystemParser = transform <$> cmdParser `sepBy1'` endOfLine
    
-- We use a tree zipper to navigate the filesystem
data Cxt = Cxt T.Text [FileSystem] [FileSystem] deriving Show

type FSZipper = (FileSystem, [Cxt])

-- We can move up and down the filesystem
up :: FSZipper -> FSZipper
up (fs, Cxt name left right : cxts) = (Directory name (left ++ [fs] ++ right), cxts)
up (fs, []) = traceShow fs $ error "Can't go up from root"

to :: T.Text -> FSZipper -> FSZipper
to _ (File _ _, _) = error "Can't cd into a file"
to subName (Directory name children, cxts) = let (left, cur:right) = List.break (nameIs subName) children in 
    (cur, Cxt name left right : cxts)
    where 
        nameIs name (File name' _) = name == name'
        nameIs name (Directory name' _) = name == name'

-- move to root directory
toRoot :: FSZipper -> FSZipper
toRoot (fs, []) = (fs, [])
toRoot z = toRoot $ up z

navigateTo :: Path -> FSZipper -> FSZipper
navigateTo (Absolute path) = processPath path . toRoot 
navigateTo (Relative path) = processPath path
    

processPath :: [T.Text] -> FSZipper -> FSZipper
processPath [] z = z
processPath ("..":xs) z = processPath xs $ up z
processPath (x:xs) z = processPath xs $ to x z

fsNewFile :: T.Text -> Int -> FSZipper -> FSZipper
fsNewFile name size (Directory dirName children, cxts) = (Directory dirName (File name size : children), cxts)
fsNewFile _ _ (File _ _, _) = error "Can't create a file in a file"

fsNewDir :: T.Text -> FSZipper -> FSZipper
fsNewDir name (Directory dirName children, cxts) = (Directory dirName (Directory name [] : children), cxts)
fsNewDir _ (File _ _, _) = error "Can't create a directory in a file"


transform :: [Command] -> FileSystem
transform = fst . toRoot . List.foldl' (\c z -> applyCommand z c) (Directory "/" [], [])
    where 
        applyCommand :: Command -> FSZipper -> FSZipper
        applyCommand (Ls outputs) z = foldr (transformOutput) z outputs
        applyCommand (Cd p) z = navigateTo p z
        transformOutput (FileOutput name size) = fsNewFile name size
        transformOutput (DirectoryOutput name) = fsNewDir name 

cmdParser :: Parser Command
cmdParser = do 
    Cd <$> (string "$ cd " *> pathParser)
    <|>
    Ls <$> (string "$ ls" *> endOfLine *> outputParser `sepBy'` endOfLine) 
    where

pathParser :: Parser Path
pathParser = do 
    Absolute <$> (string "/" *> nameP `sepBy` "/")
    <|> do 
        Relative . T.splitOn "/" <$> nameP 
    
outputParser :: Parser Output
outputParser = do 
    name <- string "dir " *> nameP
    return $ DirectoryOutput name
    <|> do 
        size <- decimal 
        name <- space' *> nameP
        return $ FileOutput name size


nameP = takeWhile1 (\c -> c /= ' ' && c /= '\n')

space' :: Parser Char
space' = char ' '

-- compute the size of a filesystem recursively
size :: FileSystem -> Int
size (File _ size) = size
size (Directory _ children) = List.foldl' (\acc c -> acc + size c) 0 children

-- compute a map of the size of each directory
sizeMap :: T.Text -> FileSystem -> Map T.Text Int
sizeMap path (File _ _) = Map.empty
sizeMap path (Directory name children) = Map.insert dirPath (List.foldl' (\acc c -> acc + size c) 0 children) $ Map.unions $ List.map (sizeMap dirPath) children
    where 
        dirPath = path `T.append` "/" `T.append` name