{-# LANGUAGE CPP, RankNTypes, TemplateHaskell , PackageImports, NoMonomorphismRestriction #-}

module Main where

#ifdef CABAL
import qualified  "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif
import Paths
import Data.List(groupBy,sort)
import Data.List.Split
import Common
import System.FilePath((</>),(<.>))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State as S
import Control.Lens ((^.), Lens', makeLenses, view)
import qualified Control.Lens (set)
import Database.HDBC
import Database.HDBC.Sqlite3

data App = App { _easy :: [SudokuWithId],
                _medium :: [SudokuWithId],
                _hard :: [SudokuWithId],
                _insane :: [SudokuWithId],
                _window :: Window,
                _chosen :: SudokuWithId }
makeLenses ''App

type SudokuLens = Lens' App [SudokuWithId] 



main :: IO ()
main = do
    static <- getStaticDir
    let database = static </> db -- database file is in /wwwroot
    [easySudokus, mediumSudokus, hardSudokus, insaneSudokus] <- readSudokus database

    let initialState w = App { _chosen = head easySudokus, _easy = easySudokus, _medium = mediumSudokus, _hard = hardSudokus, _insane = insaneSudokus, _window = w } 

        -- Run the application with the initial configuration
        executeApplication w = evalStateT mysetup $ initialState w

    startGUI defaultConfig
        { tpPort       = Just 10000
        , tpStatic     = Just static
        } executeApplication

fromSQLRow [sqlId, sqlLevel, sqlContent] =
  let id = (fromSql sqlId) :: Integer
      level = (fromSql sqlLevel) :: String
      content = (fromSql sqlContent) :: String in
  Sudoku id (read level) (read content)

readSudokus :: FilePath -> IO [[SudokuWithId]]
readSudokus database  = do
  conn <- connectSqlite3 database
  rows <- quickQuery' conn "SELECT * FROM sudokus" []
  Database.HDBC.disconnect conn
  let sudokus = groupBy (\(Sudoku _ l1 _) (Sudoku _ l2 _) -> l1 == l2) $ sort $ map fromSQLRow rows
  return sudokus

mysetup :: StateT App UI ()
mysetup =  do
  app <- S.get
  let win = app^.window
  -- lift $ return (win C.# C.set title "Sudoku")
  lift $ UI.addStyleSheet win "sudoku.css"
  showSudoku

showSudoku :: StateT App UI ()
showSudoku  = do
  app <- S.get
  let sdkToDisplay = view chosen app
      (Sudoku sid _ _) = sdkToDisplay
  caption <- lift $ UI.h1 # set text ("Sudoku " ++ show sid)
  sudokus <- mapM (uncurry $ flip selectSudokus) [("Easy", easy),("Medium",medium),("Hard",hard),("Insane",insane)]
                
  select <- lift $ UI.div #. "selectSudokus" #+ map return sudokus
  content <- lift $ UI.div #. "sudoku" #+ sudokuGrid [createCell row col sdkToDisplay | row <- [0..8], col <- [0..8]] 

  -- (Re)draws the whole html body
  lift $ getBody (app^.window) # set children [caption, content, select]
  return ()
    
selectSudokus :: SudokuLens -> String -> StateT App UI Element
selectSudokus lens caption = do
  app <- S.get
  let sdks = view lens app
      options = map (\(Sudoku sid _ _) -> UI.option # set value (show sid) # set text (show sid)) sdks

  select <- lift $ UI.select #. "selectSudoku" #+ options

  -- Show the selected sudoku on selection change
  lift $ on UI.selectionChange select  $ \(Just val) -> do
    let sudokuWithId :: Integer -> SudokuWithId
        sudokuWithId searchId = head $ filter (\(Sudoku sid _ _) -> sid == searchId) sdks 
        getIdOfNthSudoku nth = let (Sudoku sid _ _) = (sdks!!nth) in sid
        -- This is the sudoku selected in the select box
        selectedSudoku = sudokuWithId $ getIdOfNthSudoku val
        -- The selected sudoku should be the first in the select box
        sudokusOfBox = selectedSudoku : filter (selectedSudoku /=) sdks

    evalStateT
      -- Sets the sudokus of the select box
      (do
        modify $ Control.Lens.set lens sudokusOfBox 
        -- redraw the body
        showSudoku
        return ())
      -- Update the state with the selectedSudoku
      (Control.Lens.set chosen selectedSudoku app)

  -- Create the select box with a caption
  lift $ UI.div #+ [UI.h1 # set text caption, element select]

-- Separates the cells into 9 sudoku blocks and concatenates them
sudokuGrid :: [UI Element] -> [UI Element]
sudokuGrid cells =  concatMap (concat . blockify [[],[],[]]) $ chunksOf 27 cells
  where
    blockify :: [[UI Element]] -> [UI Element] -> [[UI Element]]
    blockify blocks [] = [ [UI.div #. "square" #+ cs] | cs <- blocks]
    blockify acc cs = blockify (zipWith (++) acc row ) restCells
      where
      row = chunksOf 3 $ take 9 cs
      restCells = drop 9 cs
    
createCell :: Int -> Int -> SudokuWithId -> UI Element
createCell r c (Sudoku _ _ digits) =
  let row = show r
      col = show c
      sudoku = chunksOf 9 digits 
      -- these classes are not necessary but good to indicate that the
      -- cell is in its correct place in the grid
      classes = "cell row"++row++"col"++col
      solution = case (sudoku!!r)!!c of
        Free digit -> digit
        Guess digit -> digit
      in

   do
    cell <- UI.div #. classes
    cellInput <- UI.input # set (attr "maxlength") "1"

    -- Change background of a digit square on value change
    on UI.valueChange cellInput $ \val -> do
      if val == "" then
        return cell #. classes
        else if read val == solution then
               return cell #. (classes ++ " right")
             else
               return cell #. (classes ++ " wrong")
      return ()

    -- Mark the free digits 
    case (sudoku!!r)!!c of
      Free _ -> element cellInput # set value (show solution) # set (attr "readonly") "true" #. "free"
      _ -> element cellInput # set value ""

    element cell #+ [return cellInput]
