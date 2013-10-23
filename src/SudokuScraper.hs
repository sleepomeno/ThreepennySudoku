module Main where

import Text.HTML.TagSoup
import Data.List.Split
import Solver (solveSudoku)
import Network.HTTP
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import System.Exit
  
-- lists of INTs are either sudoku rows or sudoku squares
type RawSudoku = [[Int]]
data RawSudokuWithId = RawS Int RawSudoku

data Digit = Guess Int | Free Int deriving (Show) -- a digit is either visible to
                           -- the user (=Free) oder has to be guessed
                           -- (= Guess)
type Sudoku = [Digit] -- a Sudoku consists of rows of digits
data SudokuWithId = Sudoku Int [Digit] deriving (Show) -- a Sudoku consists of rows of digits

data SudokuRequest = SudokuRequest { sid :: Int, url :: String }

{-- These will be provided on the command line --}
-- sudokuRange :: [Int]
-- sudokuRange = [0..10]
-- sudokuStart :: Int
-- sudokuStart = 1000000000
sudokuStringBase = "http://www.soduko.org/sudoku-print.php?id="

exit    = exitSuccess
usage = putStrLn "Usage SudokuScraper base number"          
                   
main = do
  args <- getArgs
  case args of
    [start,number] -> scrapeSudokus (read start) (read number)
    _ -> putStrLn "Usage: ./SudokuScraper sudokuStart numberOfSudokus" >> exit
    
-- The scraping function which does the work
-- start : the first sudoku that will be fetched
-- number : the number-1 of sudokus that will be fetched starting from start
scrapeSudokus :: Int -> Int -> IO ()
scrapeSudokus  start number = do
  let reqs = [SudokuRequest { sid = reqId, url = sudokuStringBase ++ show reqId }| offset <- [0..number], let reqId = start+offset]
  sudokus <- mapM getSudoku reqs
  let getSolvedSudokus = [Sudoku sid $ toSudoku prob sol | sudoku@(RawS sid _) <- sudokus, let prob = toString sudoku, let sol = solveSudoku prob]
  print getSolvedSudokus

-- Requests the HTML page and parses the Sudoku. Returns the sudoku as
-- a list of digits
getSudoku :: SudokuRequest -> IO RawSudokuWithId
getSudoku req = do
  resp <- Network.HTTP.simpleHTTP $ getRequest $ url req
  -- let's wait a little bit so that sudoku.org does not hate us
  threadDelay 1500
  html <- getResponseBody resp
  let tdRegex = "<td width=60>" :: String
      allTds = sections (~== tdRegex) $ parseTags html :: [[Tag String]]
      allSquaresRaw = map maybeTagText $ take 81 $ head $ map (filter isTagText) allTds
      allSquares = let beautify (Just x) = if x == "\160" then 0 else read x :: Int
                       beautify _ = error "No Nothing allowed" in
                   map beautify allSquaresRaw
  return $ RawS (sid req) $ toLines $ chunksOf 9 allSquares


-- Transforms a Sudoku given by a list of squares of digits into a Sudoko given by a list of rows of digits
toLines :: RawSudoku -> RawSudoku
toLines squares = 
  let
    square1To3 = take 3 squares 
    square4To6 = take 3 $ drop 3 squares
    square7To9 = drop 6 squares in
  squaresToLines' square1To3 ++ squaresToLines' square4To6 ++ squaresToLines' square7To9

-- Takes three horizontally linked sudoku squares and returns the
-- corresponding sudoku rows
squaresToLines' squares =
 let squ1 = squares!!0
     squ2 = squares!!1
     squ3 = squares!!2 in
  [first3 squ1 ++ first3 squ2 ++ first3 squ3, snd3 squ1 ++ snd3 squ2 ++ snd3 squ3, thrd3 squ1 ++ thrd3 squ2 ++ thrd3 squ3] 
    where
    slice begin end = take (end - begin) . drop begin
    first3 = slice 0 3
    snd3 = slice 3 6
    thrd3 = slice 6 9

toString :: RawSudokuWithId -> String
toString (RawS _ rows) = concatMap show $ concat rows

-- Takes the sudoku problem specifation and its solution and returns a
                         -- sudoku representation filling in the free
                         -- or solved digits in the appropriate places
toSudoku :: String -> String -> Sudoku
toSudoku prob sol = map (\(p,s) -> if p==0 then Guess s else Free p) $
                        zipWith (\p s -> (read [p], read [s]) ) prob sol
