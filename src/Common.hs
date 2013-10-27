module Common where

-- lists of INTs are either sudoku rows or sudoku squares
type RawSudoku = [[Int]]
data RawSudokuWithId = RawS Int RawSudoku

data Digit = Guess Int | Free Int deriving (Show, Read) -- a digit is either visible to
                           -- the user (=Free) oder has to be guessed
                           -- (= Guess)
type Sudoku = [Digit] -- a Sudoku consists of rows of digits
data SudokuWithId = Sudoku Int [Digit] deriving (Show, Read) -- a Sudoku consists of rows of digits
