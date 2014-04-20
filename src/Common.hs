module Common where

-- lists of INTs are either sudoku rows or sudoku squares
data RawSudoku = RawS [[Int]]

data Digit = Guess Int | Free Int deriving (Show, Read, Eq) -- a digit is either visible to
                           -- the user (=Free) oder has to be guessed
                           -- (= Guess)
type Sudoku = [Digit] -- a Sudoku consists of rows of digits
data SudokuWithId = Sudoku Integer LevelDescription [Digit] deriving (Show, Read, Eq) 

instance Ord SudokuWithId where
  compare (Sudoku i1 l1 _) (Sudoku i2 l2 _) = case compare l1 l2 of
    EQ -> compare  i1 i2
    LT -> LT
    GT -> GT

data SudokuWithLevel = SudokuL LevelDescription [Digit] deriving (Show, Read, Eq) 

data Level = Level Integer Int LevelDescription 

data LevelDescription = Easy | Medium | Hard | Insane deriving (Show, Read, Eq, Enum, Ord)

levels :: [Level]
-- levels = [Level 1000000000 977 "Easy" , Level 2000000000 590 "Medium" , Level 3000000000 485 "Hard", Level 4000000000 808 "Insane"]
levels = [Level 1000000000 2 Easy, Level 2000000000 2 Medium, Level 3000000000 2Hard, Level 4000000000 2 Insane]

db = "sudokus.db"
