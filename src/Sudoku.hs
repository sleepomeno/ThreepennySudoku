{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables, TemplateHaskell , PackageImports, NoMonomorphismRestriction #-}

module Main where

import Control.Monad
-- import Safe

#ifdef CABAL
import qualified  "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core as C
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C
#endif
import Paths
import Data.List.Split
import Common
import System.FilePath((</>))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State as S
import Control.Lens

data App = App { _easy :: [SudokuWithId],
                            _medium :: [SudokuWithId],
                            _hard :: [SudokuWithId],
                            _insane :: [SudokuWithId],
                            _window ::
                              Window,
                            _chosen :: SudokuWithId }
makeLenses ''App

main :: IO ()
main = do
    static <- getStaticDir
    putStrLn static
    easySudokus <- readSudokus $ static </> "sudokus" </> "easy.sudoku"
    mediumSudokus <- readSudokus $ static </> "sudokus" </> "medium.sudoku"
    hardSudokus <- readSudokus $ static </> "sudokus" </> "hard.sudoku"
    insaneSudokus <- readSudokus $ static </> "sudokus" </> "insane.sudoku"
    startGUI defaultConfig
        { tpPort       = 10000
        , tpStatic     = Just static
        } $ \w -> evalStateT mysetup App { _chosen = head easySudokus, _easy = easySudokus, _medium = mediumSudokus, _hard = hardSudokus, _insane = insaneSudokus, _window = w } 

readSudokus :: String -> IO [SudokuWithId]
readSudokus filename = do
  content <- readFile filename
  let mySudokus :: [SudokuWithId]
      mySudokus =  read content
  return mySudokus

mysetup :: StateT App IO ()
mysetup =  do
  app <- S.get
  -- let chosenSdk = _chosen app
  let win = app^.window
  -- return (window # set title "Sudoku")
  lift $ UI.addStyleSheet win "sudoku.css"
  showSudoku
  -- where
  --   sudoku = head easy 

showSudoku :: StateT App IO ()
showSudoku  = do
  app <- S.get
  let chosenSdk = _chosen app
  let window = _window app
  let (Sudoku sid _) = chosenSdk
  caption <- lift $ UI.h1 C.# C.set text ("Sudoku " ++ show sid)
  myselect <- selectSudokus easy
  select <- lift $ UI.div #. "selectSudokus" #+  [return myselect]
  content <- lift $ UI.div #. "sudoku" #+ sudokuGrid [createCell row col chosenSdk | row <- [0..8], col <- [0..8]] 
  lift $ getBody window C.# C.set C.children [caption, content, select]
  return ()
    
selectSudokus :: Lens App App [SudokuWithId] [SudokuWithId] -> StateT App IO Element
selectSudokus linse = do
  app <- S.get
  let getter = view linse
  let setter = Control.Lens.set linse 
  let easySdks = getter app
  -- lens .= []
  let options = map (\(Sudoku sid _) -> UI.option C.# C.set value (show sid) C.# C.set text (show sid)) easySdks
  select <- lift $ UI.select #. "selectSudoku" #+ options

  lift $ on UI.selectionChange select  (\(Just val) -> do
    let chosenSudoku = getSudoku easySdks $ getSID (easySdks!!val)
        restSudoku = chosenSudoku : filter (\(Sudoku sid _) -> sid /= getSID (easySdks!!val)) easySdks
    evalStateT (do { state <- S.get; put $ setter restSudoku state; return () } >> showSudoku) (app { _chosen = chosenSudoku, _easy = restSudoku } )
    return ())

  lift $ C.element select

  where
    getSudoku :: [SudokuWithId] -> Int -> SudokuWithId
    getSudoku sdks id = head $ filter (\(Sudoku sid _) -> sid == id) sdks
    getSID (Sudoku sid _) = sid
    

sudokuGrid :: [IO Element] -> [IO Element]
sudokuGrid cells =  concat [makePartition firstThird, makePartition secondThird, makePartition thirdThird]
  where
    firstThird = take 27 cells
    secondThird = take 27 $ drop 27 cells
    thirdThird = drop 54 cells

    makePartition :: [IO Element] -> [IO Element]
    makePartition cs = concat $ partitionCells cs [[],[],[]]
      where
        -- Separates 27 cells into 3 sudo squares
        partitionCells :: [IO Element] -> [[IO Element]] -> [[IO Element]]
        partitionCells [] acc = [makeSquare (acc!!0), makeSquare (acc!!1), makeSquare (acc!!2)]
                                where
                                makeSquare :: [IO Element] -> [IO Element]
                                makeSquare cells = [UI.div #. "square" #+ cells]
        partitionCells cs acc = partitionCells restCells [acc!!0 ++ firstThree, acc!!1 ++ secondThree, acc!!2 ++ thirdThree]
          where
            firstThree = take 3 cs
            secondThree = take 3 $ drop 3 cs
            thirdThree = take 3 $ drop 6 cs
            restCells = drop 9 cs

    
createCell :: Int -> Int -> SudokuWithId -> IO Element
createCell r c (Sudoku _ digits) =
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
    cellInput <- UI.input C.# C.set (attr "maxlength") "1"

    -- Change background of a digit square on value change
    on UI.valueChange cellInput $ \val -> do
      if val == "" then
        return cell C.#. classes
        else if read val == solution then
               return cell C.#. (classes ++ " right")
             else
               return cell C.#. (classes ++ " wrong")
      return ()

    -- Mark the free digits 
    case (sudoku!!r)!!c of
      Free _ -> return cellInput C.# C.set value (show solution) C.# C.set (attr "readonly") "true" C.#. "free"
      _ -> return cellInput C.# C.set value ""

    return cell C.#+ [return cellInput]
