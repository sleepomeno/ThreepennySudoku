{-# LANGUAGE CPP, RankNTypes, TemplateHaskell , PackageImports, NoMonomorphismRestriction #-}

module Main where

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
import Control.Lens hiding (chosen)

data App = App { _easy :: [SudokuWithId],
                _medium :: [SudokuWithId],
                _hard :: [SudokuWithId],
                _insane :: [SudokuWithId],
                _window :: Window,
                _chosen :: SudokuWithId }
makeLenses ''App

type SudokuLens = Lens App App [SudokuWithId] [SudokuWithId] 

main :: IO ()
main = do
    static <- getStaticDir
    easySudokus <- readSudokus $ static </> "sudokus" </> "easy.sudoku"
    mediumSudokus <- readSudokus $ static </> "sudokus" </> "medium.sudoku"
    hardSudokus <- readSudokus $ static </> "sudokus" </> "hard.sudoku"
    insaneSudokus <- readSudokus $ static </> "sudokus" </> "insane.sudoku"

    let initialState = App { _chosen = head easySudokus, _easy = easySudokus, _medium = mediumSudokus, _hard = hardSudokus, _insane = insaneSudokus} 

        -- Run the application with the initial configuration
        executeApplication w = evalStateT mysetup initialState { _window = w }

    startGUI defaultConfig
        { tpPort       = 10000
        , tpStatic     = Just static
        } $ executeApplication

readSudokus :: String -> IO [SudokuWithId]
readSudokus filename = do
  content <- readFile filename
  let mySudokus :: [SudokuWithId]
      mySudokus =  read content
  return mySudokus

mysetup :: StateT App IO ()
mysetup =  do
  app <- S.get
  let win = app^.window
  -- lift $ return (win C.# C.set title "Sudoku")
  lift $ UI.addStyleSheet win "sudoku.css"
  showSudoku


  -- 00 01 02 10 11 12 20 21 22 03 04 05 13 14 15 23 24 25

showSudoku :: StateT App IO ()
showSudoku  = do
  app <- S.get
  let chosenSdk = _chosen app
      (Sudoku sid _) = chosenSdk
  caption <- lift $ UI.h1 C.# C.set text ("Sudoku " ++ show sid)
  selectEasy <- selectSudokus easy "Easy"
  selectMedium <- selectSudokus medium "Medium"
  selectHard <- selectSudokus hard "Hard"
  selectInsane <- selectSudokus insane "Insane"
  let sudokus = map return [selectEasy, selectMedium, selectHard, selectInsane] 
                
  select <- lift $ UI.div #. "selectSudokus" #+ sudokus
  content <- lift $ UI.div #. "sudoku" #+ sudokuGrid [createCell row col chosenSdk | row <- [0..8], col <- [0..8]] 

  -- (Re)draws the whole html body
  lift $ getBody (app^.window) C.# C.set C.children [caption, content, select]
  return ()
    
selectSudokus :: SudokuLens -> String -> StateT App IO Element
selectSudokus lens caption = do
  app <- S.get
  let sdks = Control.Lens.view lens app
      options = map (\(Sudoku sid _) -> UI.option C.# C.set value (show sid) C.# C.set text (show sid)) sdks

  select <- lift $ UI.select #. "selectSudoku" #+ options

  -- Show the selected sudoku on selection change
  lift $ on UI.selectionChange select  $ \(Just val) -> do
    let chosenSudoku = getSudoku sdks $ getSID (sdks!!val)
        setter = Control.Lens.set lens 
        -- The chosen sudoku should be the first in the select box
        sudokusOfBox = chosenSudoku : filter (\(Sudoku sid _) -> sid /= getSID (sdks!!val)) sdks

    evalStateT
      -- Sets the sudokus of the select box
      (do
        state <- S.get
        put $ setter sudokusOfBox state
        return ()
        -- redraw the body
     >> showSudoku)
      -- Update the state with the chosenSudoku
      (Control.Lens.set chosen chosenSudoku app)
    return ()

  lift $ UI.div #+ [UI.h1 C.# C.set text caption, C.element select]

  where
    getSudoku :: [SudokuWithId] -> Int -> SudokuWithId
    getSudoku sdks id = head $ filter (\(Sudoku sid _) -> sid == id) sdks
    getSID (Sudoku sid _) = sid

sudokuGrid :: [IO Element] -> [IO Element]
sudokuGrid cells =  concatMap (concat . blockify [[],[],[]]) $ chunksOf 27 cells
  where
    -- Separates 27 cells into 3 sudoku squares
    blockify :: [[IO Element]] -> [IO Element] -> [[IO Element]]
    blockify acc [] = map makeSquare acc
      where
      makeSquare cells = [UI.div #. "square" #+ cells]

    blockify acc cs = blockify (zipWith (++) acc row ) restCells
      where
      row = chunksOf 3 $ take 9 cs
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
