module Common where

-- lists of INTs are either sudoku rows or sudoku squares
data RawSudoku = RawS [[Int]]

data Digit = Guess Int | Free Int deriving (Show, Read, Eq) -- a digit is either visible to
                           -- the user (=Free) oder has to be guessed
                           -- (= Guess)

fromEither :: Either Int Int -> Digit
fromEither x = case x of
                 (Left y) -> Guess y
                 (Right y) -> Free y

toEither :: Digit -> Either Int Int
toEither digit = case digit of
                  (Guess y) -> (Left y)
                  (Free y) -> (Right y)

type Sudoku = [Digit] -- a Sudoku consists of rows of digits
data SudokuWithId = Sudoku Integer LevelDescription [Digit] deriving (Show, Read, Eq) 

extractDigits (Sudoku _ _ digits) = digits

instance Ord SudokuWithId where
  compare (Sudoku i1 l1 _) (Sudoku i2 l2 _) = case compare l1 l2 of
    EQ -> compare  i1 i2
    LT -> LT
    GT -> GT

numberOfLevels :: Int
numberOfLevels = length levels

data SudokuWithLevel = SudokuL LevelDescription [Digit] deriving (Show, Read, Eq) 
data Level = Level Integer Int LevelDescription 
data LevelDescription = Easy | Medium | Hard | Insane deriving (Show, Read, Eq, Enum, Ord)
levels :: [Level]
levels = [Level 1000000000 200 Easy , Level 2000000000 200 Medium , Level 3000000000 200 Hard, Level 4000000000 200 Insane]
-- levels = [Level 1000000000 2 Easy, Level 2000000000 2 Medium, Level 3000000000 2 Hard, Level 4000000000 2 Insane]

db = "sudokus.db"

type LabeledSudokus = [(String,[SudokuWithId])]
