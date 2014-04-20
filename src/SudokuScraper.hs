module Main where

import Text.HTML.TagSoup
import Data.List.Split
import Solver (solveSudoku)
import Network.HTTP
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import System.Exit
import Common
import Database.HDBC
import Database.HDBC.Sqlite3

data SudokuRequest = SudokuRequest { sid :: Int, url :: String }

sudokuStringBase = "http://www.soduko.org/sudoku-print.php?id="

exit    = exitSuccess
usage = putStrLn "Usage SudokuScraper base number"          

main = handleSqlError $ do
  args <- getArgs
  case args of
    [] -> scrapeSudokus 
    _ -> putStrLn "Usage: ./scraper" >> exit

initDatabase :: IO ()
initDatabase = do
  conn <- connectSqlite3 db
  run conn "CREATE TABLE sudokus (id INTEGER PRIMARY KEY, level VARCHAR(20), content VARCHAR(120))" []
  commit conn
  disconnect conn

saveSudokus :: [SudokuWithLevel] -> IO ()
saveSudokus sudokus = do
  conn <- connectSqlite3 db
  stmt <- prepare conn "INSERT INTO sudokus (level, content) VALUES (?,?)"
  executeMany stmt sudokusToSql
  commit conn
  disconnect conn
    where
    sudokusToSql :: [[SqlValue]]
    sudokusToSql = (`map` sudokus) $ \(SudokuL level digits) -> [toSql (show level), toSql (show digits)]
    
scrapeSudokus :: IO ()
scrapeSudokus  = do
  initDatabase
  sudokusToSave <- mapM scrapeSudokusOfLevel levels
  saveSudokus $ concat sudokusToSave

scrapeSudokusOfLevel :: Level -> IO [SudokuWithLevel]
scrapeSudokusOfLevel (Level start number level) = do
  let reqs = [SudokuRequest { sid = reqId, url = sudokuStringBase ++ show reqId }| offset <- [0..number], let reqId = fromIntegral start + offset]

  sudokus <- mapM getSudoku reqs
  return [SudokuL level $ toSudoku prob sol | sudoku <- sudokus, let prob = toString sudoku, let sol = solveSudoku prob]


-- Requests the HTML page and parses the Sudoku. Returns the sudoku as
-- a list of digits
getSudoku :: SudokuRequest -> IO RawSudoku
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
  putStrLn $ "Scraped sudoku with id " ++ show (sid req)
  return $ toLines $ chunksOf 9 allSquares


-- Transforms a Sudoku given by a list of squares of digits into a Sudoko given by a list of rows of digits
toLines :: [[Int]] -> RawSudoku
toLines squares = 
  let
    square1To3 = take 3 squares 
    square4To6 = take 3 $ drop 3 squares
    square7To9 = drop 6 squares in
  RawS $ squaresToLines' square1To3 ++ squaresToLines' square4To6 ++ squaresToLines' square7To9

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
toString (RawS rows) = concatMap show $ concat rows

-- Takes the sudoku problem specifation and its solution and returns a
                         -- sudoku representation filling in the free
                         -- or solved digits in the appropriate places
toSudoku :: String -> String -> Sudoku
toSudoku prob sol = map (\(p,s) -> if p==0 then Guess s else Free p) $
                        zipWith (\p s -> (read [p], read [s]) ) prob sol
