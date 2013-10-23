{-# LANGUAGE CPP, PackageImports #-}

module Main where

import Control.Monad
-- import Safe

#ifdef CABAL
import qualified  "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif
import Paths

type Sudoku = [[Digit]] -- a Sudoku consists of rows of digits
data Digit = Guess Int | Free Int -- a digit is either visible to
                           -- the user (=Free) oder has to be guessed
                           -- (= Guess)

-- Sample Sudoku (TODO: Download free Sudokus and write UI to choose
              -- between them)
mysudoku :: Sudoku
mysudoku = [[Guess 1, Free 6, Guess 2, Free 5, Guess 7, Free 3, Guess 9, Free 8, Guess 4],
            [Guess 7, Guess 3, Free 4, Guess 8, Free 9, Guess 2, Guess 5, Guess 1, Guess 6],
            [Guess 5, Guess 9, Guess 8, Guess 1, Guess 4, Guess 6, Free 2, Guess 3, Free 7],
            [Guess 4, Guess 5, Guess 7, Free 2, Guess 6, Guess 1, Guess 8, Guess 9, Guess 3],
            [Guess 8, Guess 2, Free 6, Guess 7, Free 3, Free 9, Free 1, Guess 5, Guess 5],
            [Guess 9, Free 1, Free 3, Free 4, Guess 6, Guess 8, Guess 6, Guess 7, Guess 2],
            [Guess 3, Free 7, Guess 1, Free 9, Guess 2, Guess 5, Guess 4, Guess 6, Guess 8],
            [Guess 6, Free 8, Guess 5, Guess 3, Free 1, Guess 4, Guess 7, Guess 2, Guess 9],
            [Free 2, Guess 4, Free 9, Free 6, Guess 8, Guess 7, Guess 3, Free 5, Guess 1]]

  
main :: IO ()
main = do
    static <- getStaticDir
    putStrLn static
    startGUI defaultConfig
        { tpPort       = 10000
        , tpStatic     = Just static
        } mysetup


mysetup :: Window -> IO ()
mysetup w = void $ do
  return w # set title "Sudoku"
  UI.addStyleSheet w "sudoku.css"
  getBody w #+ [UI.div #. "sudoku" #+ sudokuGrid [createCell row col mysudoku | row <- [0..8], col <- [0..8]]]

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

    
createCell :: Int -> Int -> Sudoku -> IO Element
createCell r c sudoku =
  let row = show r
      col = show c
      -- these classes are not necessary but good to indicate that the
            -- cell is in its correct place in the grid
      classes = "cell row"++row++"col"++col
      solution = case (sudoku!!r)!!c of
        Free digit -> digit
        Guess digit -> digit
      in do
    cell <- UI.div #. classes
    cellInput <- UI.input # set (attr "maxlength") "1"

    on UI.valueChange cellInput $ \val -> do
      if val == "" then
        element cell #. classes
        else if read val == solution then
               element cell #. (classes ++ " right")
             else
               element cell #. (classes ++ " wrong")
      return ()

    case (sudoku!!r)!!c of
      Free _ -> element cellInput # set value (show solution) # set (attr "readonly") "true" #. "free"
      _ -> element cellInput # set value ""

    element cell #+ [element cellInput]
