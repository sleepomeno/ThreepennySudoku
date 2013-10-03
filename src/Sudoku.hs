{-# LANGUAGE CPP, PackageImports #-}

module Main where

import Control.Monad
import Safe

#ifdef CABAL
import qualified  "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif
import Paths

type Sudoku = [[Digit]] -- a Sudoku consists of rows of digits
data Digit = NotFree Int | Free Int -- a digit is either visible to
                           -- the user (=Free) oder has to be guessed
                           -- (= NotFree)

-- Sample Sudoku (TODO: Download free Sudokus and write UI to choose
              -- between them)
mysudoku :: Sudoku
mysudoku = [[NotFree 1, Free 6, NotFree 2, Free 5, NotFree 7, Free 3, NotFree 9, Free 8, NotFree 4],
            [NotFree 7, NotFree 3, Free 4, NotFree 8, Free 9, NotFree 2, NotFree 5, NotFree 1, NotFree 6],
            [NotFree 5, NotFree 9, NotFree 8, NotFree 1, NotFree 4, NotFree 6, Free 2, NotFree 3, Free 7],
            [NotFree 4, NotFree 5, NotFree 7, Free 2, NotFree 6, NotFree 1, NotFree 8, NotFree 9, NotFree 3],
            [NotFree 8, NotFree 2, Free 6, NotFree 7, Free 3, Free 9, Free 1, NotFree 5, NotFree 5],
            [NotFree 9, Free 1, Free 3, Free 4, NotFree 6, NotFree 8, NotFree 6, NotFree 7, NotFree 2],
            [NotFree 3, Free 7, NotFree 1, Free 9, NotFree 2, NotFree 5, NotFree 4, NotFree 6, NotFree 8],
            [NotFree 6, Free 8, NotFree 5, NotFree 3, Free 1, NotFree 4, NotFree 7, NotFree 2, NotFree 9],
            [Free 2, NotFree 4, Free 9, Free 6, NotFree 8, NotFree 7, NotFree 3, Free 5, NotFree 1]]

  
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig
        { tpPort       = 10000
        , tpStatic     = Just static
        } mysetup


mysetup :: Window -> IO ()
mysetup w = void $ do
  return w # set title "Sudoku"
  UI.addStyleSheet w "sudoku.css"
  getBody w #+ [UI.div #. "sudoku" #+ [createCell row col mysudoku | row <- [0..8], col <- [0..8]]]

    
createCell :: Int -> Int -> Sudoku -> IO Element
createCell r c sudoku =
  let row = show r
      col = show c
      -- these classes are not necessary but good to indicate that the
            -- cell is in its correct place in the grid
      classes = "cell row"++row++"col"++col
      solution = case (sudoku!!r)!!c of
        Free digit -> digit
        NotFree digit -> digit
      in do
    cell <- UI.div #. classes
    cellInput <- UI.input # set (attr "maxlength") "1"

    on UI.valueChange cellInput $ \val -> do
      if (val == "") then
        element cell #. classes
        else if (read val == solution) then
               element cell #. (classes ++ " right")
             else
               element cell #. (classes ++ " wrong")
      return ()

    case (sudoku!!r)!!c of
      Free digit -> element cellInput # set value (show solution) # set (attr "readonly") "true" #. "free"
      _ -> element cellInput # set value ""

    element cell #+ [element cellInput]
