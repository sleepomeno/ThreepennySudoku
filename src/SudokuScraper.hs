module Main where

import Text.HTML.TagSoup
import Data.List.Split
import Solver (solveSudoku)
import Network.HTTP
  
-- lists of INTs are either sudoku rows or sudoku squares
type RawSudoku = [[Int]]

type Sudoku = [Digit] -- a Sudoku consists of rows of digits
data Digit = Guess Int | Free Int deriving (Show) -- a digit is either visible to
                           -- the user (=Free) oder has to be guessed
                           -- (= Guess)



-- Number of Sudokus to fetch
sudokuRange = [0..1]
sudokuStart = 1000000000
sudokuStringBase = "http://www.soduko.org/sudoku-print.php?id="

main = do
  let urls = [sudokuStringBase ++ show (sudokuStart+offset)| offset <- sudokuRange]
  sudokus <- mapM getSudoku urls
  let getSolvedSudokus = [toSudoku prob sol | sudoku <- sudokus, let prob = toString sudoku, let sol = solveSudoku prob]
  mapM_ print getSolvedSudokus

getSudoku url = do
  resp <- Network.HTTP.simpleHTTP (getRequest url)
  html <- getResponseBody resp
  let tdRegex = "<td width=60>" :: String
      allTds = sections (~== tdRegex) $ parseTags html :: [[Tag String]]
      allSquaresRaw = map (\x -> maybeTagText x) $ take 81 $ head $ map (\x -> filter isTagText x) allTds
      allSquares = let beautify (Just x) = if x == "\160" then 0 else read x :: Int
                       beautify _ = error "No Nothing allowed" in
                   map beautify allSquaresRaw
  return $ toLines $ chunksOf 9 allSquares


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

toString :: RawSudoku -> String
toString rows = concatMap show $ concat rows

toSudoku :: [Char] -> [Char] -> Sudoku
toSudoku prob sol = map (\(p,s) -> if p==0 then Guess s else Free p) $
                        zipWith (\p s -> (read (p:[]), read (s:[])) ) prob sol
